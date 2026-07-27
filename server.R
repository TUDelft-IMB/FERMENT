# =============================================================================
# server.R
# Contains all the logic: reading files, filtering, and rendering plots.
# The server function is called once per user session. It receives three
# arguments automatically from Shiny:
#   input   - a list of current values from all UI inputs
#   output  - a list where you assign rendered objects (plots, text, etc.)
#   session - used to update UI elements (e.g. dropdowns)
#
# ALL plots are rendered with renderPlotly(ggplotly(...)) so every chart is
# interactive. The ggplot functions live in global.R; this file just wraps
# them in ggplotly() before handing them to the UI.
# =============================================================================

server <- function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Step 1: Build a metadata table from all Excel files in EXCEL_DIR.
  #
  # file_metadata() scans the folder and reads ONLY the "Experimental_parameters"
  # sheet from each file (not the full workbook; for startup speed purposes).
  # It returns a data frame with one row per experiment file, containing the
  # key experimental conditions used to populate and filter the dropdowns.
  #
  # reactive() means this code re-runs automatically if EXCEL_DIR changes.
  # In practice it runs once at startup.
  # ---------------------------------------------------------------------------

  # Build metadata table from Experimental_parameters sheet
  file_metadata <- reactive({
    # List all .xlsx files in the configured folder (filenames only, not paths)
    files <- list.files(EXCEL_DIR, pattern = "\\.xlsx$", full.names = FALSE)
    # Strip out temp files created by Excel while a file is open
    files <- files[!grepl("^~\\$", files)]
    # If the folder is empty, return an empty data frame
    if (length(files) == 0) {
      return(data.frame())
    }
    # Path to the cache file, stored alongside the Excel files
    cache_path <- metadata_cache_path

    # Modification times for all current files — used to detect changes
    mtimes <- file.mtime(file.path(EXCEL_DIR, files))

    # Try the cache first: only reuse it if the exact same files with the
    # exact same modification times are present (nothing added, removed, or edited)
    if (file.exists(cache_path)) {
      cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      if (!is.null(cached) &&
        !is.null(cached$files) && !is.null(cached$mtimes) &&
        identical(sort(files), sort(cached$files)) &&
        identical(mtimes[order(files)], cached$mtimes[order(cached$files)])) {
        return(cached$data)
      }
    }

    withProgress(message = "Scanning experiment files...", value = 0, {
      # Loop over every file, read its parameters sheet, and build one data frame
      # row per file. do.call(rbind, ...) stacks all rows into a single table.
      result <- do.call(rbind, lapply(files, function(f) {
        incProgress(1 / length(files), detail = paste("Reading", f))
        # Build full path
        path <- file.path(EXCEL_DIR, f)
        # Try to read the parameters sheet; if it fails (e.g. wrong sheet name),
        # return NULL so that file is silently skipped
        params <- tryCatch(
          read_excel(path, sheet = "Experimental_parameters", col_names = TRUE),
          error = function(e) NULL
        )
        # Skip this file if the sheet couldn't be read or has no data rows
        if (is.null(params) || nrow(params) < 1) {
          return(NULL)
        }
        # Read the Notes sheet for A1 — silently returns NA if missing
        notes_val <- tryCatch(
          {
            notes_sheet <- read_excel(path, sheet = "Notes", col_names = FALSE)
            as.character(notes_sheet[[1]][1])
          },
          error = function(e) NA_character_
        )

        # H2 = column 8, row 1 (after header row). Format as dd/mm/yyyy.
        raw_date <- params[[8]][1]

        formatted_date <- tryCatch(
          {
            if (inherits(raw_date, "POSIXct") || inherits(raw_date, "Date")) {
              format(as.Date(raw_date), "%d/%m/%Y")
            } else {
              # fallback: treat as Excel serial number
              format(as.Date(as.numeric(raw_date), origin = "1899-12-30"), "%d/%m/%Y")
            }
          },
          error = function(e) as.character(raw_date)
        )
        # Build a one-row data frame from the first data row (row 2 of the sheet,
        # since row 1 is the header). Column positions match the sheet layout:
        # [[2]] = B = Species
        # [[3]] = C = Strain
        # [[4]] = D = Starting gravity
        # [[5]] = E = Inoculum
        # [[6]] = F = Temperature
        # [[7]] = G = Experiment number
        data.frame(
          filename = f,
          species = as.character(params[[2]][1]),
          strain = as.character(params[[3]][1]),
          gravity = as.character(params[[4]][1]),
          inoculum = as.character(params[[5]][1]),
          temperature = as.character(params[[6]][1]),
          exp_number = as.character(params[[7]][1]),
          date = formatted_date,
          notes = ifelse(is.na(notes_val), "", notes_val),
          stringsAsFactors = FALSE
        )
      }))
    })

    # Save the freshly scanned result to the cache for next time
    tryCatch(
      saveRDS(list(data = result, files = files, mtimes = mtimes), cache_path),
      error = function(e) NULL
    )

    result
  })

  # ---------------------------------------------------------------------------
  # Step 2: Populate all filter dropdowns once metadata is ready.

  # Single Experiment filters get "All" prepended (single-select pattern).
  # Compare and Averages filters start empty (multi-select; empty = no filter).
  # ---------------------------------------------------------------------------
  observe({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return()
    }

    # Single Experiment filters
    updateSelectInput(session, "species", choices = c("All", sort(unique(meta$species))))
    updateSelectInput(session, "strain", choices = c("All", sort(unique(meta$strain))))
    updateSelectInput(session, "gravity", choices = c("All", sort(unique(meta$gravity))))
    updateSelectInput(session, "inoculum", choices = c("All", sort(unique(meta$inoculum))))
    updateSelectInput(session, "temperature", choices = c("All", sort(unique(meta$temperature))))

    # Compare filters
    updateSelectizeInput(session, "cmp_species", choices = sort(unique(meta$species)), server = TRUE)
    updateSelectizeInput(session, "cmp_strain", choices = sort(unique(meta$strain)), server = TRUE)
    updateSelectizeInput(session, "cmp_gravity", choices = sort(unique(meta$gravity)), server = TRUE)
    updateSelectizeInput(session, "cmp_inoculum", choices = sort(unique(meta$inoculum)), server = TRUE)
    updateSelectizeInput(session, "cmp_temperature", choices = sort(unique(meta$temperature)), server = TRUE)

    # Averages filters
    updateSelectizeInput(session, "avg_species", choices = sort(unique(meta$species)), server = TRUE)
    updateSelectizeInput(session, "avg_strain", choices = sort(unique(meta$strain)), server = TRUE)
    updateSelectizeInput(session, "avg_gravity", choices = sort(unique(meta$gravity)), server = TRUE)
    updateSelectizeInput(session, "avg_inoculum", choices = sort(unique(meta$inoculum)), server = TRUE)
    updateSelectizeInput(session, "avg_temperature", choices = sort(unique(meta$temperature)), server = TRUE)
  })

  # ---------------------------------------------------------------------------
  # Step 3: Overview tab metrics and plots.
  # ---------------------------------------------------------------------------

  output$unique_species_count <- renderText({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return("0")
    }
    paste0(" ", length(unique(meta$species)), " ")
  })

  output$unique_strains_count <- renderText({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return("0")
    }
    paste0(" ", length(unique(meta$strain)), " ")
  })

  output$unique_temps_count <- renderText({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return("0")
    }
    paste0(" ", length(unique(meta$temperature)), " ")
  })

  output$total_experiments_count <- renderText({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return("0")
    }
    paste0(" ", nrow(meta), " ")
  })

  output$experiments_per_species_plot <- renderPlotly({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return(NULL)
    }

    # Generate a consistent, named color palette based on all unique species
    unique_species <- sort(unique(meta$species))
    species_colors <- setNames(
      hcl.colors(length(unique_species), palette = "Dynamic"),
      unique_species
    )
    unique_species <- sort(unique(meta$species))
    base_palette <- c(rev(okabe10), polychrome_extra) # okabe10 + extended safe colours from global.R
    n_sp <- length(unique_species)
    # If there are more species than colours, repeat the palette (safe fallback)
    cols <- if (n_sp <= length(base_palette)) base_palette[seq_len(n_sp)] else rep(base_palette, length.out = n_sp)
    species_colors <- setNames(cols, unique_species)

    species_counts <- meta %>%
      group_by(species) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      mutate(species_label = paste0("<i>", species, "</i>"))

    plot_ly(
      species_counts,
      labels = ~species_label,
      values = ~n,
      type = "pie",
      hole = 0.55,
      rotation = -90,
      textposition = "outside",
      textinfo = "label+value",
      insidetextorientation = "radial",
      hovertemplate = "<i>%{label}</i><br>%{value} experiments<br>%{percent}<extra></extra>",
      marker = list(
        colors = species_colors[species_counts$species],
        line = list(color = "white", width = 2)
      ),
      height = 350
    ) %>%
      layout(
        showlegend = FALSE,
        uniformtext = list(minsize = 10, mode = "show"),
        margin = list(l = 20, r = 20, t = 20, b = 20),
        annotations = list(list(
          text = paste0("<b>", sum(species_counts$n), "</b><br>total"),
          x = 0.5, y = 0.5,
          font = list(size = 16),
          showarrow = FALSE
        ))
      )
  })

  output$experiments_per_strain_plot <- renderPlotly({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return(NULL)
    }

    unique_species <- sort(unique(meta$species))
    species_colors <- setNames(
      hcl.colors(length(unique_species), palette = "Dynamic"),
      unique_species
    )
    base_palette <- c(rev(okabe10), polychrome_extra)
    n_sp <- length(unique_species)
    cols <- if (n_sp <= length(base_palette)) base_palette[seq_len(n_sp)] else rep(base_palette, length.out = n_sp)
    species_colors <- setNames(cols, unique_species)

    strain_counts <- meta %>%
      group_by(strain, species) %>%
      summarise(n = n(), .groups = "drop")

    strain_order <- strain_counts %>%
      group_by(strain) %>%
      summarise(total_n = sum(n), .groups = "drop") %>%
      arrange(desc(total_n), strain) %>%
      slice_head(n = 50) %>%
      pull(strain)

    # Filter to top 50 strains and set factor levels
    strain_counts <- strain_counts %>%
      filter(strain %in% strain_order) %>%
      mutate(
        strain = factor(strain, levels = strain_order),
        species = factor(as.character(species), levels = unique_species),
        species_label = sprintf(
          "<i>%s</i><br>%d exp.",
          trimws(as.character(species)),
          n
        )
      )

    plot_ly(
      data = strain_counts,
      y = ~strain,
      x = ~n,
      color = ~species,
      colors = species_colors,
      type = "bar",
      orientation = "h",
      hovertext = ~species_label,
      hoverinfo = "text",
      textposition = "none"
    ) %>%
      layout(
        barmode = "stack", # Explicitly set stacking mode for species mixes
        showlegend = FALSE,
        hoverlabel = list(namelength = -1),
        xaxis = list(
          title = "Number of Experiments",
          side = "top",
          showgrid = TRUE,
          gridcolor = "gray90"
        ),
        yaxis = list(
          title = list(text = "Strains", standoff = 10),
          type = "category",
          categoryorder = "array",
          categoryarray = levels(strain_counts$strain),
          autorange = "reversed"
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 150)
      )
  })

  output$gravity_distribution_plot <- renderPlotly({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return(NULL)
    }

    gravity_data <- meta %>%
      mutate(gravity_num = suppressWarnings(as.numeric(as.character(gravity)))) %>%
      filter(!is.na(gravity_num)) %>%
      group_by(gravity_num) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(gravity_num)
    
    gravity_levels <- as.character(gravity_data$gravity_num)
    base_palette <- c(okabe10, polychrome_extra)
    n_lv <- length(gravity_levels)
    cols <- if (n_lv <= length(base_palette)) {
      base_palette[seq_len(n_lv)]
    } else {
      rep(base_palette, length.out = n_lv)
    }
    fill_map <- setNames(cols, gravity_levels)
    
    p <- ggplot(
      gravity_data,
      aes(x = factor(gravity_num), y = n, fill = factor(gravity_num), text = n)
    ) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = fill_map) +
      theme_minimal() +
      labs(x = "Starting Gravity (SG)", y = "Number of Experiments") +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_line(colour = "gray90")
      )
    
    ggplotly(p, tooltip = "text")
  })

  output$inoculum_distribution_plot <- renderPlotly({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return(NULL)
    }

    inoculum_data <- meta %>%
      mutate(inoculum = as.character(inoculum)) %>%
      filter(!is.na(inoculum), inoculum != "") %>%
      group_by(inoculum) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(inoculum)

    inoc_levels <- as.character(inoculum_data$inoculum)
    base_palette <- c(okabe10, polychrome_extra)
    n_lv <- length(inoc_levels)
    cols <- if (n_lv <= length(base_palette)) base_palette[seq_len(n_lv)] else rep(base_palette, length.out = n_lv)
    fill_map <- setNames(cols, inoc_levels)

    p <- ggplot(
      inoculum_data,
      aes(x = inoculum, y = n, fill = inoculum, text = n)
    ) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = fill_map) +
      theme_minimal() +
      labs(x = "Inoculum", y = "Number of Experiments") +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_line(colour = "gray90")
      )

    ggplotly(p, tooltip = "text")
  })

  output$temperature_distribution_plot <- renderPlotly({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return(NULL)
    }

    temp_data <- meta %>%
      mutate(temperature = as.numeric(temperature)) %>%
      filter(!is.na(temperature)) %>%
      group_by(temperature) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(temperature)

    temp_levels <- as.character(temp_data$temperature)
    base_palette <- c(rev(okabe10), polychrome_extra)
    n_lv <- length(temp_levels)
    cols <- if (n_lv <= length(base_palette)) base_palette[seq_len(n_lv)] else rep(base_palette, length.out = n_lv)
    fill_map <- setNames(cols, temp_levels)

    p <- ggplot(temp_data, aes(x = as.factor(temperature), y = n, fill = as.factor(temperature), text = n)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = fill_map) +
      theme_minimal() +
      labs(x = "Temperature (°C)", y = "Number of Experiments") +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_line(colour = "gray90")
      )

    ggplotly(p, tooltip = "text")
  })

  output$conditions_summary_table <- renderDT(
    {
      meta <- file_metadata()
      if (is.null(meta) || nrow(meta) == 0) {
        return(NULL)
      }
      meta %>%
        arrange(species, strain, as.numeric(gravity), as.numeric(temperature)) %>%
        transmute(
          `Experiment` = paste0("#", exp_number),
          `Species` = paste0("<i>", species, "</i>"),
          `Strain` = strain,
          `Gravity` = gravity,
          `Inoculum` = inoculum,
          `Temp.` = temperature,
          `Date start exp.` = date,
          `Additional notes` = notes
        )
    },
    options = list(pageLength = 15, scrollX = TRUE),
    rownames = FALSE,
    escape = FALSE
  )

  # ---------------------------------------------------------------------------
  # Helper: build a tidy display table from a set of filenames.
  # Used by all three summary table outputs (single, compare, averages).
  # Returns one row per experiment with columns in a consistent display order.
  # ---------------------------------------------------------------------------
  make_summary_table <- function(filenames, meta) {
    if (length(filenames) == 0 || is.null(meta) || nrow(meta) == 0) {
      return(NULL)
    }
    matched <- meta[meta$filename %in% filenames, ]
    if (nrow(matched) == 0) {
      return(NULL)
    }
    # Preserve the order the user selected them in
    matched <- matched[match(filenames, matched$filename), ]
    data.frame(
      `Experiment` = paste0("#", matched$exp_number),
      `Species` = paste0("<i>", matched$species, "</i>"),
      `Strain` = matched$strain,
      `Gravity` = matched$gravity,
      `Inoculum` = matched$inoculum,
      `Temp.` = matched$temperature,
      `Date start exp.` = matched$date,
      `Additional notes` = matched$notes,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Step 4: Single Experiment — filter dropdown logic.
  # ---------------------------------------------------------------------------
  filtered_files <- reactive({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return(character(0))
    }

    matched <- meta
    if (input$species != "All") matched <- matched[matched$species == input$species, ]
    if (input$strain != "All") matched <- matched[matched$strain == input$strain, ]
    if (input$gravity != "All") matched <- matched[matched$gravity == input$gravity, ]
    if (input$inoculum != "All") matched <- matched[matched$inoculum == input$inoculum, ]
    if (input$temperature != "All") matched <- matched[matched$temperature == input$temperature, ]

    if (nrow(matched) == 0) {
      return(character(0))
    }
    matched$filename
  })

  # ---------------------------------------------------------------------------
  # Step 5: Keep the experiment dropdown in sync with the active filters.
  # ---------------------------------------------------------------------------
  observe({
    choices <- filtered_files()

    updateSelectInput(
      session,
      "experiment",
      choices = c("Select an experiment..." = "", choices),
      selected = ""
    )
  })

  # Step 5a: Clear Single Experiment filters.
  observeEvent(input$clear_filters, {
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return()
    }
    updateSelectInput(session, "species", selected = "All")
    updateSelectInput(session, "strain", selected = "All")
    updateSelectInput(session, "gravity", selected = "All")
    updateSelectInput(session, "inoculum", selected = "All")
    updateSelectInput(session, "temperature", selected = "All")
  })

  # Step 6: Build the full path for the selected experiment.
  workbook_path <- reactive({
    req(input$experiment)
    file.path(EXCEL_DIR, input$experiment)
  })

  # ---------------------------------------------------------------------------
  # Step 7: Load the selected workbook using read_tt_workbook() from global.R.
  # Returns NULL if the file doesn't exist (e.g. was deleted after startup).
  # ---------------------------------------------------------------------------
  tt_data <- reactive({
    path <- workbook_path()
    if (!file.exists(path)) {
      return(NULL)
    }
    read_tt_workbook(path)
  })

  # ---------------------------------------------------------------------------
  # Step 8: Show a short status message in the sidebar confirming whether
  # the selected file loaded successfully.
  # ---------------------------------------------------------------------------
  output$file_status <- renderText({
    path <- workbook_path()
    if (is.null(tt_data())) {
      paste0("File not found:\n", basename(path))
    } else {
      paste0("Loaded:\n", basename(path))
    }
  })

  # Step 8a: Single Experiment summary table.
  # Shows one row — the metadata of the currently loaded experiment.
  output$single_summary_table <- renderDT(
    {
      req(input$experiment)
      make_summary_table(input$experiment, file_metadata())
    },
    options = list(dom = "t", ordering = FALSE),
    rownames = FALSE,
    escape = FALSE
  )

  # ---------------------------------------------------------------------------
  # Step 9: Sheet accessors — single experiment.
  #
  # Each reactive extracts one named sheet from the loaded workbook.
  # req() ensures the downstream plot code only runs once a workbook is loaded.
  # Sheet names must exactly match those in the Excel files.
  # ---------------------------------------------------------------------------

  hplc <- reactive({
    req(tt_data())
    tt_data()[["HPLC"]]
  })
  gc_esters <- reactive({
    req(tt_data())
    tt_data()[["GC_esters"]]
  })
  gc_ketones <- reactive({
    req(tt_data())
    tt_data()[["GC_ketones"]]
  })
  att <- reactive({
    req(tt_data())
    tt_data()[["Attenuation"]]
  })
  ph <- reactive({
    req(tt_data())
    tt_data()[["pH"]]
  })
  viability <- reactive({
    req(tt_data())
    tt_data()[["CellCount_Viability"]]
  })
  # ---------------------------------------------------------------------------
  # Step 10: Render single-experiment plots.
  #
  # Each output calls the matching ggplot function from global.R, then wraps
  # it in ggplotly() so the chart is interactive:
  # - Hover over any point to see its exact value
  # - Click legend entries to show/hide individual compounds
  # - Drag to zoom, double-click to reset zoom
  #
  # The output ID (e.g. "hplcTT1Plot") must match plotlyOutput() in ui.R.
  # renderPlotly() re-runs automatically whenever its reactive data changes.
  # ---------------------------------------------------------------------------

  # HPLC: sugars and ethanol over time, TT1 and TT2 side by side
  output$hplcTT1Plot <- renderPlotly({
    req(hplc())
    ggplotly(plot_hplc_tube(hplc(), tube_num = 1))
  })
  output$hplcTT2Plot <- renderPlotly({
    req(hplc())
    ggplotly(plot_hplc_tube(hplc(), tube_num = 2))
  })

  # GC Esters: volatile esters over time, TT1 and TT2 side by side
  output$gcEstersTT1Plot <- renderPlotly({
    req(gc_esters())
    ggplotly(plot_gc_esters_tube(gc_esters(), tube_num = 1))
  })
  output$gcEstersTT2Plot <- renderPlotly({
    req(gc_esters())
    ggplotly(plot_gc_esters_tube(gc_esters(), tube_num = 2))
  })

  # GC Ketones: diacetyl and 2,3-pentanedione over time, TT1 and TT2
  output$gcKetonesTT1Plot <- renderPlotly({
    req(gc_ketones())
    ggplotly(plot_gc_ketones_tube(gc_ketones(), tube_num = 1))
  })
  output$gcKetonesTT2Plot <- renderPlotly({
    req(gc_ketones())
    ggplotly(plot_gc_ketones_tube(gc_ketones(), tube_num = 2))
  })

  # Attenuation and pH: one line per tube (TT1 and TT2)
  output$attPlot <- renderPlotly({
    req(att())
    ggplotly(plot_att(att()))
  })
  output$phPlot <- renderPlotly({
    req(ph())
    ggplotly(plot_ph(ph()))
  })

  # Cell Count and Viability: one line per tube (TT1 and TT2)
  output$cellCountPlot <- renderPlotly({
    req(viability())
    ggplotly(plot_cell_count(viability()))
  })
  output$viabilityPlot <- renderPlotly({
    req(viability())
    ggplotly(plot_viability(viability()))
  })

  # ---------------------------------------------------------------------------
  # Step 11: Averages — filter the pool of available experiments.
  # ---------------------------------------------------------------------------
  avg_filtered_files <- reactive({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return(character(0))
    }
    matched <- meta
    if (length(input$avg_species) > 0) matched <- matched[matched$species %in% input$avg_species, ]
    if (length(input$avg_strain) > 0) matched <- matched[matched$strain %in% input$avg_strain, ]
    if (length(input$avg_gravity) > 0) matched <- matched[matched$gravity %in% input$avg_gravity, ]
    if (length(input$avg_inoculum) > 0) matched <- matched[matched$inoculum %in% input$avg_inoculum, ]
    if (length(input$avg_temperature) > 0) matched <- matched[matched$temperature %in% input$avg_temperature, ]
    if (nrow(matched) == 0) {
      return(character(0))
    }
    matched$filename
  })

  # Step 11a: Keep avg_experiments in sync with filters.
  observe({
    updateSelectizeInput(session, "avg_experiments",
      choices = avg_filtered_files(), server = TRUE
    )
  })

  # Step 11b: Clear Averages filters.
  observeEvent(input$avg_clear_filters, {
    updateSelectizeInput(session, "avg_species", selected = character(0))
    updateSelectizeInput(session, "avg_strain", selected = character(0))
    updateSelectizeInput(session, "avg_gravity", selected = character(0))
    updateSelectizeInput(session, "avg_inoculum", selected = character(0))
    updateSelectizeInput(session, "avg_temperature", selected = character(0))
  })

  # Step 12: Load averages data for selected experiments.
  avg_data_list <- reactive({
    req(input$avg_experiments)

    withProgress(message = "Loading averages...", value = 0, {
      result <- lapply(input$avg_experiments, function(f) {
        incProgress(1 / length(input$avg_experiments), detail = paste("Reading", f))
        read_avg_sheet(file.path(EXCEL_DIR, f))
      })
    })

    names(result) <- input$avg_experiments
    # Drop entries where the sheet was missing (read_avg_sheet returned NULL)
    result[!sapply(result, is.null)]
  })

  avg_exp_labels <- reactive({
    req(avg_data_list())
    meta <- file_metadata()
    filenames <- names(avg_data_list())
    matched <- meta[meta$filename %in% filenames, ]
    # Match labels to filenames in the exact order the user selected them
    paste0("#", matched$exp_number[match(filenames, matched$filename)])
  })

  # Step 12a: Averages summary table — one row per selected experiment.
  output$avg_summary_table <- renderDT(
    {
      req(input$avg_experiments)
      make_summary_table(input$avg_experiments, file_metadata())
    },
    options = list(dom = "t", ordering = TRUE),
    rownames = FALSE,
    escape = FALSE
  )

  # ---------------------------------------------------------------------------
  # Step 13: Render averages plots
  #
  # Same pattern as Step 10: each output calls a ggplot function from global.R
  # then wraps it in ggplotly(). This gives the Averages page the same
  # interactive behaviour as the single-experiment plots, plus clickable
  # legend items to toggle individual experiments on/off.
  #
  # req() on avg_data_list() ensures nothing renders until at least one
  # experiment with a valid averages_to_plot sheet has been selected.
  # ---------------------------------------------------------------------------

  # Sugars & Ethanol: one line per compound per experiment
  output$avgHplcPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_hplc(avg_data_list(), avg_exp_labels()))
  })

  # GC Esters: stacked bar charts — ethyl esters and acetate esters
  output$avgEthylEstersBarPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_ethyl_esters_bar(avg_data_list(), avg_exp_labels()))
  })
  output$avgAcetateEstersBarPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_acetate_esters_bar(avg_data_list(), avg_exp_labels()))
  })

  # Higher Alcohols: stacked bar chart
  output$avgHigherAlcoholsBarPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_higher_alcohols_bar(avg_data_list(), avg_exp_labels()))
  })

  # Vicinal Diketones: diacetyl and 2,3-pentanedione over time
  output$avgDiketonesPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_diketones(avg_data_list(), avg_exp_labels()))
  })

  # GC Ratio plot
  output$avg_gc_ratio_plot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_gc_ratio_bar(avg_data_list(), avg_exp_labels()))
  })

  # Attenuation, pH, Cell Count, Viability: one line per experiment
  output$avgAttenuationPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_attenuation(avg_data_list(), avg_exp_labels()))
  })
  output$avgPhPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_ph(avg_data_list(), avg_exp_labels()))
  })
  output$avgCellCountPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_cell_count(avg_data_list(), avg_exp_labels()))
  })
  output$avgViabilityPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_viability(avg_data_list(), avg_exp_labels()))
  })

  # Cone Viability: one bar per experiment with error bars
  output$avgConeViabilityPlot <- renderPlotly({
    req(avg_data_list())
    ggplotly(plot_avg_cone_viability(avg_data_list(), avg_exp_labels()))
  })

  # ---------------------------------------------------------------------------
  # Step 14: Compare — filter the pool of available experiments.
  #
  # cmp_filtered_files() mirrors filtered_files() from Step 4 but reads from
  # the cmp_* input IDs so the Compare sidebar is fully independent.
  # Returns a named vector (label -> filename) just like filtered_files().
  # ---------------------------------------------------------------------------
  cmp_filtered_files <- reactive({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) {
      return(character(0))
    }

    # Multi-select filters: if the user has chosen one or more values, keep only
    # rows that match any of them. An empty selection (length 0) means no filter
    # is applied for that dimension — all values pass through.
    matched <- meta
    if (length(input$cmp_species) > 0) matched <- matched[matched$species %in% input$cmp_species, ]
    if (length(input$cmp_strain) > 0) matched <- matched[matched$strain %in% input$cmp_strain, ]
    if (length(input$cmp_gravity) > 0) matched <- matched[matched$gravity %in% input$cmp_gravity, ]
    if (length(input$cmp_inoculum) > 0) matched <- matched[matched$inoculum %in% input$cmp_inoculum, ]
    if (length(input$cmp_temperature) > 0) matched <- matched[matched$temperature %in% input$cmp_temperature, ]

    if (nrow(matched) == 0) {
      return(character(0))
    }
    matched$filename
  })

  # Step 15: Keep cmp_experiments in sync with filters.
  observe({
    updateSelectizeInput(session, "cmp_experiments",
      choices  = cmp_filtered_files(),
      server   = TRUE
    )
  })

  # Step 15a: Clear Compare filters.
  observeEvent(input$cmp_clear_filters, {
    updateSelectizeInput(session, "cmp_species", selected = character(0))
    updateSelectizeInput(session, "cmp_strain", selected = character(0))
    updateSelectizeInput(session, "cmp_gravity", selected = character(0))
    updateSelectizeInput(session, "cmp_inoculum", selected = character(0))
    updateSelectizeInput(session, "cmp_temperature", selected = character(0))
  })

  # Step 16: Load full workbooks for selected Compare experiments.
  cmp_data_list <- reactive({
    req(input$cmp_experiments)

    withProgress(message = "Loading experiments...", value = 0, {
      result <- lapply(input$cmp_experiments, function(f) {
        incProgress(1 / length(input$cmp_experiments), detail = paste("Reading", f))
        path <- file.path(EXCEL_DIR, f)
        tryCatch(read_tt_workbook(path), error = function(e) NULL)
      })
    })

    names(result) <- input$cmp_experiments
    result[!sapply(result, is.null)]
  })

  cmp_exp_labels <- reactive({
    req(cmp_data_list())
    meta <- file_metadata()
    filenames <- names(cmp_data_list())
    matched <- meta[meta$filename %in% filenames, ]
    paste0("#", matched$exp_number[match(filenames, matched$filename)])
  })

  output$cmp_status <- renderText({
    n <- length(cmp_data_list())
    if (n == 0) {
      "No experiments loaded"
    } else {
      paste0(n, " experiment", if (n == 1) "" else "s", " loaded")
    }
  })

  # Step 16a: Compare summary table — one row per selected experiment.
  output$cmp_summary_table <- renderDT(
    {
      req(input$cmp_experiments)
      make_summary_table(input$cmp_experiments, file_metadata())
    },
    options = list(dom = "t", ordering = TRUE),
    rownames = FALSE,
    escape = FALSE
  )

  # ---------------------------------------------------------------------------
  # Step 17: Render Compare plots.
  #
  # Each output calls the matching plot_cmp_*() function from global.R section
  # 7, then wraps it in ggplotly(). TT1 and TT2 remain on separate charts
  # (same layout as Single Experiment) but each chart now overlays all
  # selected experiments.
  #
  # Attenuation, pH, Cell Count, and Viability pass explicit tube_col and
  # tube_label strings rather than a tube number, because their raw sheet
  # column names don't follow the "N compound" pattern that HPLC/GC use.
  # ---------------------------------------------------------------------------

  # HPLC: one line per compound per experiment, TT1 and TT2 overlaid
  output$cmpHplcPlot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_hplc(cmp_data_list(), cmp_exp_labels()))
  })

  # GC Esters: one line per compound per experiment, TT1 and TT2 overlaid
  output$cmpGcEstersPlot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_gc_esters(cmp_data_list(), cmp_exp_labels()))
  })

  # GC Ketones: one line per compound per experiment, TT1 and TT2 overlaid
  output$cmpGcKetonesPlot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_gc_ketones(cmp_data_list(), cmp_exp_labels()))
  })

  # Attenuation: both TT1 and TT2 in one chart; colour = experiment, linetype = tube
  output$cmpAttPlot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_att(cmp_data_list(), cmp_exp_labels()))
  })

  # pH: both TT1 and TT2 in one chart; colour = experiment, linetype = tube
  output$cmpPhPlot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_ph(cmp_data_list(), cmp_exp_labels()))
  })

  # Cell Count: both TT1 and TT2 in one chart; colour = experiment, linetype = tube
  output$cmpCellCountPlot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_cell_count(cmp_data_list(), cmp_exp_labels()))
  })

  # Viability: both TT1 and TT2 in one chart; colour = experiment, linetype = tube
  output$cmpViabilityPlot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_viability(cmp_data_list(), cmp_exp_labels()))
  })
} # end server
