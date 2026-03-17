server <- function(input, output, session) {
  
  # Scan folder for available Excel files
  available_files <- reactive({
    files <- list.files(EXCEL_DIR, pattern = "\\.xlsx$", full.names = FALSE)
    if (length(files) == 0) return(character(0))
    files
  })
  
  # Update the file selector dynamically
  observe({
    updateSelectInput(session, "experiment", choices = available_files())
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
  
  # HPLC plots
  output$hplcTT1Plot <- renderPlot({
    req(hplc())
    plot_hplc_tt1(hplc())
  })
  
  output$hplcTT2Plot <- renderPlot({
    req(hplc())
    plot_hplc_tt2(hplc())
  })
  
  # GC Esters plots
  output$gcEstersTT1Plot <- renderPlot({
    req(gc_esters())
    plot_gc_esters_tt1(gc_esters())
  })
  
  output$gcEstersTT2Plot <- renderPlot({
    req(gc_esters())
    plot_gc_esters_tt2(gc_esters())
  })
  
  # GC Ketones plots
  output$gcKetonesTT1Plot <- renderPlot({
    req(gc_ketones())
    plot_gc_ketones_tt1(gc_ketones())
  })
  
  output$gcKetonesTT2Plot <- renderPlot({
    req(gc_ketones())
    plot_gc_ketones_tt2(gc_ketones())
  })
  
  # Attenuation & pH plots
  output$attPlot <- renderPlot({
    req(att())
    plot_att(att())
  })
  
  output$phPlot <- renderPlot({
    req(ph())
    plot_ph(ph())
  })
  
  # Cell Count & viability plots
  output$cellCountPlot <- renderPlot({
    req(viability())
    plot_cell_count(viability())
  })
  
  output$viabilityPlot <- renderPlot({
    req(viability())
    plot_viability(viability())
  })
  
}
