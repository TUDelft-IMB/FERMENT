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
    if (length(files) == 0) return(data.frame())
    
    # Loop over every file, read its parameters sheet, and build one data frame
    # row per file. do.call(rbind, ...) stacks all rows into a single table.
    do.call(rbind, lapply(files, function(f) {
      # Build full path
      path <- file.path(EXCEL_DIR, f)
      # Try to read the parameters sheet; if it fails (e.g. wrong sheet name),
      # return NULL so that file is silently skipped
      params <- tryCatch(
        read_excel(path, sheet = "Experimental_parameters", col_names = TRUE),
        error = function(e) NULL
      )
      # Skip this file if the sheet couldn't be read or has no data rows
      if (is.null(params) || nrow(params) < 1) return(NULL)
      
      # Build a one-row data frame from the first data row (row 2 of the sheet,
      # since row 1 is the header). Column positions match the sheet layout:
      #   [[2]] = B = Species
      #   [[3]] = C = Strain
      #   [[4]] = D = Starting gravity
      #   [[5]] = E = Inoculum
      #   [[6]] = F = Temperature
      #   [[7]] = G = Experiment number
      
      data.frame(
        filename    = f,
        species     = as.character(params[[2]][1]),
        strain      = as.character(params[[3]][1]),
        gravity     = as.character(params[[4]][1]),
        inoculum    = as.character(params[[5]][1]),
        temperature = as.character(params[[6]][1]),
        exp_number  = as.character(params[[7]][1]),
        stringsAsFactors = FALSE
      )
    }))
  })
  
  # ---------------------------------------------------------------------------
  # Step 2: Populate all filter dropdowns and the averages multi-select.
  #
  # This observe() block runs once when file_metadata() is first ready.
  # "All" is prepended to each filter so the user can opt out of filtering
  # by a particular condition. The averages multi-select gets every available
  # experiment as a named choice (label = "Experiment #N", value = filename).
  # ---------------------------------------------------------------------------
  observe({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return()
    
    # Single-experiment filter dropdowns
    updateSelectInput(session, "species",     choices = c("All", sort(unique(meta$species))))
    updateSelectInput(session, "strain",      choices = c("All", sort(unique(meta$strain))))
    updateSelectInput(session, "gravity",     choices = c("All", sort(unique(meta$gravity))))
    updateSelectInput(session, "inoculum",    choices = c("All", sort(unique(meta$inoculum))))
    updateSelectInput(session, "temperature", choices = c("All", sort(unique(meta$temperature))))
    
    # Averages multi-select: label shown = "Experiment #N", value sent to
    # server = the actual filename (used to load the averages sheet)
    avg_choices <- setNames(meta$filename, paste0("Experiment #", meta$exp_number))
    updateSelectizeInput(session, "avg_experiments", choices = avg_choices, server = TRUE)
    
    # Compare filter dropdowns — separate input IDs from Single Experiment so
    # the two tabs don't interfere with each other
    updateSelectInput(session, "cmp_species",     choices = c("All", sort(unique(meta$species))))
    updateSelectInput(session, "cmp_strain",      choices = c("All", sort(unique(meta$strain))))
    updateSelectInput(session, "cmp_gravity",     choices = c("All", sort(unique(meta$gravity))))
    updateSelectInput(session, "cmp_inoculum",    choices = c("All", sort(unique(meta$inoculum))))
    updateSelectInput(session, "cmp_temperature", choices = c("All", sort(unique(meta$temperature))))
  })
  
  # ---------------------------------------------------------------------------
  # Step 2.5: Overview Tab Metrics
  # Compute summary statistics from the metadata for the Overview tab
  # ---------------------------------------------------------------------------
  
  output$total_experiments_count <- renderText({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return("0")
    paste0(" ", nrow(meta), " ")
  })
  
  output$unique_strains_count <- renderText({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return("0")
    n_strains <- length(unique(meta$strain))
    paste0(" ", n_strains, " ")
  })
  
  output$unique_species_count <- renderText({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return("0")
    n_species <- length(unique(meta$species))
    paste0(" ", n_species, " ")
  })
  
  output$unique_temps_count <- renderText({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return("0")
    n_temps <- length(unique(meta$temperature))
    paste0(" ", n_temps, " ")
  })
  
  output$experiments_per_strain_plot <- renderPlot({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return(NULL)
    
    strain_counts <- meta %>%
      group_by(strain) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n))
    
    ggplot(strain_counts, aes(x = reorder(strain, -n), y = n, fill = strain)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Strain", y = "Number of Experiments") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            panel.grid.major.y = element_line(colour = "gray90"))
  })
  
  output$experiments_per_species_plot <- renderPlot({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return(NULL)
    
    species_counts <- meta %>%
      group_by(species) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n))
    
    ggplot(species_counts, aes(x = reorder(species, -n), y = n, fill = species)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Species", y = "Number of Experiments") +
      theme(legend.position = "none",
            panel.grid.major.y = element_line(colour = "gray90"))
  })
  
  output$temperature_distribution_plot <- renderPlot({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return(NULL)
    
    # Convert temperature to numeric for plotting (if stored as character)
    temp_data <- meta %>%
      mutate(temperature = as.numeric(temperature)) %>%
      filter(!is.na(temperature)) %>%
      group_by(temperature) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(temperature)
    
    ggplot(temp_data, aes(x = as.factor(temperature), y = n, fill = temperature)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "darkred") +
      theme_minimal() +
      labs(x = "Temperature (°C)", y = "Number of Experiments") +
      theme(legend.position = "none",
            panel.grid.major.y = element_line(colour = "gray90"))
  })
  
  output$gravity_distribution_plot <- renderPlot({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return(NULL)
    
    # Convert gravity to numeric for plotting (if stored as character)
    gravity_data <- meta %>%
      mutate(gravity = as.numeric(gravity)) %>%
      filter(!is.na(gravity)) %>%
      group_by(gravity) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(gravity)
    
    ggplot(gravity_data, aes(x = as.factor(gravity), y = n, fill = gravity)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "lightyellow", high = "darkgoldenrod") +
      theme_minimal() +
      labs(x = "Starting Gravity (SG)", y = "Number of Experiments") +
      theme(legend.position = "none",
            panel.grid.major.y = element_line(colour = "gray90"))
  })
  
  output$conditions_summary_table <- renderTable({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return(NULL)
    
    summary_table <- meta %>%
      group_by(species, strain, gravity, temperature, inoculum) %>%
      summarise(
        `Exp Count` = n(),
        `Exp Numbers` = paste(unique(exp_number), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(species, strain, as.numeric(gravity), as.numeric(temperature))
    
    summary_table
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%")
  
  # ---------------------------------------------------------------------------
  # Step 3: Filter the single-experiment dropdown.
  #
  # Returns a named vector of filenames that match all active filter values.
  # Each filter is only applied when the user has chosen something other
  # than "All", so any combination of conditions is supported.
  # ---------------------------------------------------------------------------
  filtered_files <- reactive({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return(character(0))
    
    matched <- meta
    if (input$species     != "All") matched <- matched[matched$species     == input$species, ]
    if (input$strain      != "All") matched <- matched[matched$strain      == input$strain, ]
    if (input$gravity     != "All") matched <- matched[matched$gravity     == input$gravity, ]
    if (input$inoculum    != "All") matched <- matched[matched$inoculum    == input$inoculum, ]
    if (input$temperature != "All") matched <- matched[matched$temperature == input$temperature, ]
    
    if (nrow(matched) == 0) return(character(0))
    # Return a named vector: label "Experiment #N" maps to the filename
    setNames(matched$filename, paste0("Experiment #", matched$exp_number))
  })
  
  # ---------------------------------------------------------------------------
  # Step 4: Keep the experiment dropdown in sync with the active filters.
  # ---------------------------------------------------------------------------
  observe({
    updateSelectInput(session, "experiment", choices = filtered_files())
  })
  
  # ---------------------------------------------------------------------------
  # Step 5: Build the full file path for the selected experiment.
  # req() silently stops execution until the user has made a selection.
  # ---------------------------------------------------------------------------
  workbook_path <- reactive({
    req(input$experiment)
    file.path(EXCEL_DIR, input$experiment)
  })
  
  # ---------------------------------------------------------------------------
  # Step 6: Load the selected workbook using read_tt_workbook() from global.R.
  # Returns NULL if the file doesn't exist (e.g. was deleted after startup).
  # ---------------------------------------------------------------------------
  tt_data <- reactive({
    path <- workbook_path()
    if (!file.exists(path)) return(NULL)
    read_tt_workbook(path)
  })
  
  # ---------------------------------------------------------------------------
  # Step 7: Show a short status message in the sidebar confirming whether
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
  
  # ---------------------------------------------------------------------------
  # Step 8: Sheet accessors — single experiment.
  #
  # Each reactive extracts one named sheet from the loaded workbook.
  # req() ensures the downstream plot code only runs once a workbook is loaded.
  # Sheet names must exactly match those in the Excel files.
  # ---------------------------------------------------------------------------
  hplc       <- reactive({ req(tt_data()); tt_data()[["HPLC"]] })
  gc_esters  <- reactive({ req(tt_data()); tt_data()[["GC_esters"]] })
  gc_ketones <- reactive({ req(tt_data()); tt_data()[["GC_ketones"]] })
  att        <- reactive({ req(tt_data()); tt_data()[["Attenuation"]] })
  ph         <- reactive({ req(tt_data()); tt_data()[["pH"]] })
  viability  <- reactive({ req(tt_data()); tt_data()[["CellCount_Viability"]] })
  
  # ---------------------------------------------------------------------------
  # Step 9: Render single-experiment plots.
  #
  # Each output calls the matching ggplot function from global.R, then wraps
  # it in ggplotly() so the chart is interactive:
  #   - Hover over any point to see its exact value
  #   - Click legend entries to show/hide individual compounds
  #   - Drag to zoom, double-click to reset zoom
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
  # Step 10: Averages — load data for all selected experiments
  #
  # avg_data_list() reads the "averages_to_plot" sheet from each selected
  # file using read_avg_sheet() from global.R. Files where that sheet is
  # missing are silently dropped so they don't cause plot errors.
  # [TODO - decide on behaviour]!
  #
  # avg_exp_labels() builds the human-readable "Experiment #N" label for
  # each loaded file, preserving the order the user selected them in.
  # These labels are passed to the plotting functions as legend text.
  # ---------------------------------------------------------------------------
  avg_data_list <- reactive({
    req(input$avg_experiments)
    result <- lapply(input$avg_experiments, function(f) {
      read_avg_sheet(file.path(EXCEL_DIR, f))
    })
    names(result) <- input$avg_experiments
    # Drop entries where the sheet was missing (read_avg_sheet returned NULL)
    result[!sapply(result, is.null)]
  })
  
  avg_exp_labels <- reactive({
    req(avg_data_list())
    meta      <- file_metadata()
    filenames <- names(avg_data_list())
    matched   <- meta[meta$filename %in% filenames, ]
    # Match labels to filenames in the exact order the user selected them
    paste0("Experiment #", matched$exp_number[match(filenames, matched$filename)])
  })
  
  # ---------------------------------------------------------------------------
  # Step 11: Render averages plots
  #
  # Same pattern as Step 9: each output calls a ggplot function from global.R
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
  # Step 12: Compare — filter the pool of available experiments.
  
  # cmp_filtered_files() mirrors filtered_files() from Step 3 but reads from
  # the cmp_* input IDs so the Compare sidebar is fully independent.
  # Returns a named vector (label -> filename) just like filtered_files().
  # ---------------------------------------------------------------------------
  cmp_filtered_files <- reactive({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return(character(0))
  
    matched <- meta
    if (input$cmp_species     != "All") matched <- matched[matched$species     == input$cmp_species, ]
    if (input$cmp_strain      != "All") matched <- matched[matched$strain      == input$cmp_strain, ]
    if (input$cmp_gravity     != "All") matched <- matched[matched$gravity     == input$cmp_gravity, ]
    if (input$cmp_inoculum    != "All") matched <- matched[matched$inoculum    == input$cmp_inoculum, ]
    if (input$cmp_temperature != "All") matched <- matched[matched$temperature == input$cmp_temperature, ]
  
    if (nrow(matched) == 0) return(character(0))
    setNames(matched$filename, paste0("Experiment #", matched$exp_number))
  })
  
  # ---------------------------------------------------------------------------
  # Step 13: Keep the Compare multi-select in sync with the active filters.
  
  # When the filter dropdowns narrow the pool, update cmp_experiments to only
  # show matching experiments. Previously selected experiments that no longer
  # match the filters are automatically deselected by Shiny.
  # ---------------------------------------------------------------------------
  observe({
    updateSelectizeInput(session, "cmp_experiments",
                         choices  = cmp_filtered_files(),
                         server   = TRUE)
  })
  
  # ---------------------------------------------------------------------------
  # Step 14: Load the full workbooks for all selected Compare experiments.
  
  # cmp_data_list() calls read_tt_workbook() (not read_avg_sheet) for each
  # selected file, returning a named list of workbook lists. Each element is
  # itself a named list of data frames — one per sheet — exactly as returned
  # by read_tt_workbook(). Files that fail to load are silently dropped.
  
  # cmp_exp_labels() builds "Experiment #N" labels in selection order,
  # matching the same pattern used for avg_exp_labels() in Step 10.
  
  # cmp_status text gives the user lightweight feedback about how many
  # experiments are currently loaded and ready to plot.
  # ---------------------------------------------------------------------------
  cmp_data_list <- reactive({
    req(input$cmp_experiments)
    result <- lapply(input$cmp_experiments, function(f) {
      path <- file.path(EXCEL_DIR, f)
      tryCatch(read_tt_workbook(path), error = function(e) NULL)
    })
    names(result) <- input$cmp_experiments
    # Drop any files that failed to load
    result[!sapply(result, is.null)]
  })
  
  cmp_exp_labels <- reactive({
    req(cmp_data_list())
    meta      <- file_metadata()
    filenames <- names(cmp_data_list())
    matched   <- meta[meta$filename %in% filenames, ]
    paste0("Experiment #", matched$exp_number[match(filenames, matched$filename)])
  })
  
  output$cmp_status <- renderText({
    n <- length(cmp_data_list())
    if (n == 0) "No experiments loaded"
    else paste0(n, " experiment", if (n == 1) "" else "s", " loaded")
  })
  
  # ---------------------------------------------------------------------------
  # Step 15: Render Compare plots.
  
  # Each output calls the matching plot_cmp_*() function from global.R section
  # 7, then wraps it in ggplotly(). TT1 and TT2 remain on separate charts
  # (same layout as Single Experiment) but each chart now overlays all
  # selected experiments.
  
  # Attenuation, pH, Cell Count, and Viability pass explicit tube_col and
  # tube_label strings rather than a tube number, because their raw sheet
  # column names don't follow the "N compound" pattern that HPLC/GC use.
  # ---------------------------------------------------------------------------
  
  # HPLC: one line per compound per experiment, TT1 and TT2 side by side
  output$cmpHplcTT1Plot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_hplc_tube(cmp_data_list(), cmp_exp_labels(), tube_num = 1))
  })
  output$cmpHplcTT2Plot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_hplc_tube(cmp_data_list(), cmp_exp_labels(), tube_num = 2))
  })
  
  # GC Esters: one line per compound per experiment, TT1 and TT2 side by side
  output$cmpGcEstersTT1Plot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_gc_esters_tube(cmp_data_list(), cmp_exp_labels(), tube_num = 1))
  })
  output$cmpGcEstersTT2Plot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_gc_esters_tube(cmp_data_list(), cmp_exp_labels(), tube_num = 2))
  })
  
  # GC Ketones: one line per compound per experiment, TT1 and TT2 side by side
  output$cmpGcKetonesTT1Plot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_gc_ketones_tube(cmp_data_list(), cmp_exp_labels(), tube_num = 1))
  })
  output$cmpGcKetonesTT2Plot <- renderPlotly({
    req(cmp_data_list())
    ggplotly(plot_cmp_gc_ketones_tube(cmp_data_list(), cmp_exp_labels(), tube_num = 2))
  })
  
  # Attenuation: one line per experiment; TT1 and TT2 on separate charts
  output$cmpAttPlot <- renderPlotly({
    req(cmp_data_list())
    att_tt1 <- ggplotly(plot_cmp_att(cmp_data_list(), cmp_exp_labels(),
                                     tube_col = "TT1", tube_label = "TT1"))
    att_tt2 <- ggplotly(plot_cmp_att(cmp_data_list(), cmp_exp_labels(),
                                     tube_col = "TT2", tube_label = "TT2"))
    subplot(att_tt1, att_tt2, nrows = 1, shareY = TRUE, titleX = TRUE)
  })
  
  # pH: one line per experiment; TT1 and TT2 on separate charts
  output$cmpPhPlot <- renderPlotly({
    req(cmp_data_list())
    ph_tt1 <- ggplotly(plot_cmp_ph(cmp_data_list(), cmp_exp_labels(),
                                    tube_col = "TT1", tube_label = "TT1"))
    ph_tt2 <- ggplotly(plot_cmp_ph(cmp_data_list(), cmp_exp_labels(),
                                    tube_col = "TT2", tube_label = "TT2"))
    subplot(ph_tt1, ph_tt2, nrows = 1, shareY = TRUE, titleX = TRUE)
  })
  
  # Cell Count: one line per experiment; TT1 and TT2 on separate charts
  output$cmpCellCountPlot <- renderPlotly({
    req(cmp_data_list())
    cc_tt1 <- ggplotly(plot_cmp_cell_count(cmp_data_list(), cmp_exp_labels(),
                                            tube_col = "1 Total cells", tube_label = "TT1"))
    cc_tt2 <- ggplotly(plot_cmp_cell_count(cmp_data_list(), cmp_exp_labels(),
                                            tube_col = "2 Total cells", tube_label = "TT2"))
    subplot(cc_tt1, cc_tt2, nrows = 1, shareY = TRUE, titleX = TRUE)
  })
  
  # Viability: one line per experiment; TT1 and TT2 on separate charts
  output$cmpViabilityPlot <- renderPlotly({
    req(cmp_data_list())
    vb_tt1 <- ggplotly(plot_cmp_viability(cmp_data_list(), cmp_exp_labels(),
                                           tube_col = "1 Viability (%)", tube_label = "TT1"))
    vb_tt2 <- ggplotly(plot_cmp_viability(cmp_data_list(), cmp_exp_labels(),
                                           tube_col = "2 Viability (%)", tube_label = "TT2"))
    subplot(vb_tt1, vb_tt2, nrows = 1, shareY = TRUE, titleX = TRUE)
  })
  
  } # end server
