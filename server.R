server <- function(input, output, session) {
  
  # Build metadata table from Experimental_parameters sheet
  file_metadata <- reactive({
    files <- list.files(EXCEL_DIR, pattern = "\\.xlsx$", full.names = FALSE)
    files <- files[!grepl("^~\\$", files)]  # remove temp files before reading anything
    if (length(files) == 0) return(data.frame())
    
    do.call(rbind, lapply(files, function(f) {
      path <- file.path(EXCEL_DIR, f)
      params <- tryCatch(
        read_excel(path, sheet = "Experimental_parameters", col_names = TRUE),
        error = function(e) NULL
      )
      if (is.null(params) || nrow(params) < 1) return(NULL)
      
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
  
  # Populate all dropdowns from actual data
  observe({
    meta <- file_metadata()
    if (is.null(meta) || nrow(meta) == 0) return()
    updateSelectInput(session, "species",     choices = c("All", sort(unique(meta$species))))
    updateSelectInput(session, "strain",      choices = c("All", sort(unique(meta$strain))))
    updateSelectInput(session, "gravity",     choices = c("All", sort(unique(meta$gravity))))
    updateSelectInput(session, "inoculum",    choices = c("All", sort(unique(meta$inoculum))))
    updateSelectInput(session, "temperature", choices = c("All", sort(unique(meta$temperature))))
  })
  
  
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
    setNames(matched$filename, paste0("Experiment #", matched$exp_number))
  })
  
  
  # Experiments dropdown menu
  observe({
    updateSelectInput(session, "experiment", choices = filtered_files())
  })
  
  # Build full path from selected filename
  workbook_path <- reactive({
    req(input$experiment)
    file.path(EXCEL_DIR, input$experiment)
  })
  
  # Load workbook reactively
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
  
  # Sheet accessors
  hplc <- reactive({ req(tt_data()); tt_data()[["HPLC"]] })
  gc_esters <- reactive({ req(tt_data()); tt_data()[["GC_esters"]] })
  gc_ketones <- reactive({ req(tt_data()); tt_data()[["GC_ketones"]] })
  att <- reactive({ req(tt_data()); tt_data()[["Attenuation"]] })
  ph <- reactive({ req(tt_data()); tt_data()[["pH"]] })
  viability <- reactive({ req(tt_data()); tt_data()[["CellCount_Viability"]] })
  
  # HPLC plots
  output$hplcTT1Plot <- renderPlot({ req(hplc()); plot_hplc_tt1(hplc()) })
  output$hplcTT2Plot <- renderPlot({ req(hplc()); plot_hplc_tt2(hplc()) })
  
  # GC Esters plots
  output$gcEstersTT1Plot <- renderPlot({ req(gc_esters()); plot_gc_esters_tt1(gc_esters()) })
  output$gcEstersTT2Plot <- renderPlot({ req(gc_esters()); plot_gc_esters_tt2(gc_esters()) })
  
  # GC Ketones plots
  output$gcKetonesTT1Plot <- renderPlot({ req(gc_ketones()); plot_gc_ketones_tt1(gc_ketones()) })
  output$gcKetonesTT2Plot <- renderPlot({ req(gc_ketones()); plot_gc_ketones_tt2(gc_ketones()) })
  
  # Attenuation & pH plots
  output$attPlot <- renderPlot({ req(att()); plot_att(att()) })
  output$phPlot  <- renderPlot({ req(ph());  plot_ph(ph())   })
  
  # Cell Count & Viability plots
  output$cellCountPlot <- renderPlot({ req(viability()); plot_cell_count(viability()) })
  output$viabilityPlot <- renderPlot({ req(viability()); plot_viability(viability())  })
  
}
