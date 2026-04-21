# =============================================================================
# server.R
# Contains all the logic: reading files, filtering, and rendering plots.
# The server function is called once per user session. It receives three
# arguments automatically from Shiny:
#   input   - a list of current values from all UI inputs
#   output  - a list where you assign rendered objects (plots, text, etc.)
#   session - used to update UI elements (e.g. dropdowns)
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
  # reactive() means this code re-runs automatically if EXCEL_DIR ever changes.
  # In practice it runs once on startup.
  # ---------------------------------------------------------------------------
  
  # Build metadata table from Experimental_parameters sheet
  file_metadata <- reactive({
    # List all .xlsx files in the configured folder (filenames only, not paths)
    files <- list.files(EXCEL_DIR, pattern = "\\.xlsx$", full.names = FALSE)
    # remove temp files before reading anything
    files <- files[!grepl("^~\\$", files)]
    # If the folder is empty, return an empty data frame
    if (length(files) == 0) return(data.frame())
    
    # Loop over every file, read its parameters sheet, and build one data frame
    # row per file. do.call(rbind, ...) stacks all rows into a single table.
    do.call(rbind, lapply(files, function(f) {
      # Build full pathj
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
  # Step 2: Populate all filter dropdowns with the unique values found in
  # the metadata table. "All" is prepended for each dropdown.
  # Also populates avg_experiments with every available experiment.
  # ---------------------------------------------------------------------------
  
  # Populate all dropdowns from actual data
  observe({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return()
    updateSelectInput(session, "species",     choices = c("All", sort(unique(meta$species))))
    updateSelectInput(session, "strain",      choices = c("All", sort(unique(meta$strain))))
    updateSelectInput(session, "gravity",     choices = c("All", sort(unique(meta$gravity))))
    updateSelectInput(session, "inoculum",    choices = c("All", sort(unique(meta$inoculum))))
    updateSelectInput(session, "temperature", choices = c("All", sort(unique(meta$temperature))))
    
    # Averages multi-select: label = "Experiment #N", value = filename
    avg_choices <- setNames(meta$filename, paste0("Experiment #", meta$exp_number))
    updateSelectizeInput(session, "avg_experiments", choices = avg_choices, server = TRUE)
  })
  
  # ---------------------------------------------------------------------------
  # Step 3: Filter the list of experiments based on the current dropdown values.
  #
  # Each filter is only applied if the user has chosen something other than "All".
  # This lets filter by any combination of conditions.
  # ---------------------------------------------------------------------------
  
  # Filter experiment list based on sidebar selections
  filtered_files <- reactive({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return(character(0))
    
    matched <- meta
    if (input$species     != "All") matched <- matched[matched$species     == input$species,     ]
    if (input$strain      != "All") matched <- matched[matched$strain      == input$strain,      ]
    if (input$gravity     != "All") matched <- matched[matched$gravity     == input$gravity,     ]
    if (input$inoculum    != "All") matched <- matched[matched$inoculum    == input$inoculum,    ]
    if (input$temperature != "All") matched <- matched[matched$temperature == input$temperature, ]
    
    if (nrow(matched) == 0) return(character(0))
    # Return a named vector: label "Experiment #N" maps to the filename
    setNames(matched$filename, paste0("Experiment #", matched$exp_number))
  })
  
  # ---------------------------------------------------------------------------
  # Steps 4-7: Experiment dropdown, workbook path, loading, and status
  # ---------------------------------------------------------------------------
  
  # Experiments dropdown menu
  observe({
    updateSelectInput(session, "experiment", choices = filtered_files())
  })
  
  # Build full path from selected filename
  workbook_path <- reactive({
    req(input$experiment) # do nothing until user has selected an experiment
    file.path(EXCEL_DIR, input$experiment)
  })
  
  # Load workbook reactively (load the selected workbook using read_tt_workbook() from global.R)
  tt_data <- reactive({
    path <- workbook_path()
    if (!file.exists(path)) return(NULL)
    read_tt_workbook(path)
  })
  
  # File status indicator in sidebar
  output$file_status <- renderText({
    path <- workbook_path()
    if (is.null(tt_data())) {
      paste0("File not found:\n", basename(path))
    } else {
      paste0("Loaded:\n", basename(path))
    }
  })
  
  # ---------------------------------------------------------------------------
  # Step 8: Sheet accessors — single experiment
  # req() ensures plots only render after a workbook is loaded.
  # Sheet names must exactly match those in the Excel files.
  # ---------------------------------------------------------------------------
  
  # Sheet accessors
  hplc      <- reactive({ req(tt_data()); tt_data()[["HPLC"]]                   })
  gc_esters <- reactive({ req(tt_data()); tt_data()[["GC_esters"]]              })
  gc_ketones<- reactive({ req(tt_data()); tt_data()[["GC_ketones"]]             })
  att       <- reactive({ req(tt_data()); tt_data()[["Attenuation"]]            })
  ph        <- reactive({ req(tt_data()); tt_data()[["pH"]]                     })
  viability <- reactive({ req(tt_data()); tt_data()[["CellCount_Viability"]]    })
  
  
  # ---------------------------------------------------------------------------
  # Step 9: Render plots
  # Each renderPlot() calls the corresponding plot function from global.R,
  # passing in the relevant sheet data frame. The output ID (e.g. "hplcTT1Plot")
  # must match the plotOutput() ID in ui.R exactly.
  # renderPlot() re-runs automatically whenever its reactive data changes.
  # ---------------------------------------------------------------------------
  
  # HPLC plots

  output$hplcTT1Plot <- renderPlot({ req(hplc()); plot_hplc_tube(hplc(), tube_num = 1) })
  output$hplcTT2Plot <- renderPlot({ req(hplc()); plot_hplc_tube(hplc(), tube_num = 2) })
  
  # GC Esters plots
  
  output$gcEstersTT1Plot <- renderPlot({ req(gc_esters()); plot_gc_esters_tube(gc_esters(), tube_num = 1) })
  output$gcEstersTT2Plot <- renderPlot({ req(gc_esters()); plot_gc_esters_tube(gc_esters(), tube_num = 2) })
  
  
  # GC Ketones plots

  output$gcKetonesTT1Plot <- renderPlot({ req(gc_ketones()); plot_gc_ketones_tube(gc_ketones(), tube_num = 1) })
  output$gcKetonesTT2Plot <- renderPlot({ req(gc_ketones()); plot_gc_ketones_tube(gc_ketones(), tube_num = 2) })
  
  # Attenuation & pH plots
  output$attPlot <- renderPlot({ req(att()); plot_att(att()) })
  output$phPlot  <- renderPlot({ req(ph());  plot_ph(ph())   })
  
  # Cell Count & Viability plots
  output$cellCountPlot <- renderPlot({ req(viability()); plot_cell_count(viability()) })
  output$viabilityPlot <- renderPlot({ req(viability()); plot_viability(viability())  })
  
  # ---------------------------------------------------------------------------
  # Step 10: Averages — load data for all selected experiments
  #
  # avg_data_list() maps input$avg_experiments (a vector of filenames) to a
  # list of data frames, one per selected experiment, by calling
  # read_avg_sheet() from global.R. Files where the sheet is missing are
  # silently dropped so they don't break the plots.
  #
  # avg_exp_labels() returns the matching human-readable labels
  # ("Experiment #N") used in plot legends.
  # ---------------------------------------------------------------------------
  
  avg_data_list <- reactive({
    req(input$avg_experiments)
    result <- lapply(input$avg_experiments, function(f) {
      read_avg_sheet(file.path(EXCEL_DIR, f))
    })
    names(result) <- input$avg_experiments
    # Drop any files where the sheet was missing
    result[!sapply(result, is.null)]
  })
  
  avg_exp_labels <- reactive({
    req(avg_data_list())
    meta <- file_metadata()
    filenames <- names(avg_data_list())
    matched   <- meta[meta$filename %in% filenames, ]
    # Preserve the order the user selected
    paste0("Experiment #", matched$exp_number[match(filenames, matched$filename)])
  })
  
  # ---------------------------------------------------------------------------
  # Step 11: Render averages plots
  # req() on avg_data_list() ensures nothing renders until at least one
  # experiment with a valid averages_to_plot sheet has been selected.
  # ---------------------------------------------------------------------------
  
  output$avgHplcPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_hplc(avg_data_list(), avg_exp_labels())
  })
  
  output$avgEthylEstersBarPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_ethyl_esters_bar(avg_data_list(), avg_exp_labels())
  })
  
  output$avgAcetateEstersBarPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_acetate_esters_bar(avg_data_list(), avg_exp_labels())
  })
  
  output$avgHigherAlcoholsBarPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_higher_alcohols_bar(avg_data_list(), avg_exp_labels())
  })
  
  output$avgDiketonesPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_diketones(avg_data_list(), avg_exp_labels())
  })
  
  output$avgAttenuationPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_attenuation(avg_data_list(), avg_exp_labels())
  })
  
  output$avgPhPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_ph(avg_data_list(), avg_exp_labels())
  })
  
  output$avgCellCountPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_cell_count(avg_data_list(), avg_exp_labels())
  })
  
  output$avgViabilityPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_viability(avg_data_list(), avg_exp_labels())
  })
  
  output$avgConeViabilityPlot <- renderPlot({
    req(avg_data_list())
    plot_avg_cone_viability(avg_data_list(), avg_exp_labels())
  })
}
