# =============================================================================
# ui.R
# Defines the visual layout of the app; what the user sees and interacts with.
# Shiny reads this file to build the HTML page. No logic lives here; all
# reactive behaviour is handled in server.R.
# =============================================================================

ui <- fluidPage(
  # Title displayed at the top of the page
  titlePanel("FERMENT"),
  # Split the page into a narrow sidebar (filters) and a wide main panel (plots)
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      # -----------------------------------------------------------------------
      # Filter dropdowns
      # All choices start as NULL — server.R populates them dynamically by
      # reading the "Experimental_parameters" sheet from each Excel file.
      # Selecting a value here narrows down which experiments are shown below.
      # -----------------------------------------------------------------------
      
      selectInput("species",     "Species",     choices = NULL),
      selectInput("strain",      "Strain",      choices = NULL),
      selectInput("gravity",     "Gravity",     choices = NULL),
      selectInput("inoculum",    "Inoculum",    choices = NULL),
      selectInput("temperature", "Temperature", choices = NULL),
      selectInput("experiment",  "Experiments", choices = NULL),
      
      # -----------------------------------------------------------------------
      # Note to the last entry above:
      # Experiments dropdown: shows only files that match the filters above.
      # The labels shown are of the form "Experiment #<number>"; the value
      # passed to the server is the actual filename (used to load the correct
      # workbook).
      # -----------------------------------------------------------------------
      
      # visual separator
      hr(), 
      
      # Small text box showing whether a file was successfully loaded or not.
      # Its content is set by output$file_status in server.R.
      verbatimTextOutput("file_status")
    ),
    
    mainPanel(
      width = 10,
      
      # Tab strip: each tab shows a different set of analytical plots.
      # The plot output IDs here (e.g. "hplcTT1Plot") must exactly match
      # the output$<id> names used in server.R.
      
      tabsetPanel(
        
        # -------------------------------------------------------------------
        # SINGLE-EXPERIMENT TABS
        # -------------------------------------------------------------------
        tabPanel("HPLC",
                 fluidRow(
                   column(6, plotOutput("hplcTT1Plot", height = "400px")),
                   column(6, plotOutput("hplcTT2Plot", height = "400px"))
                 )
        ),
        
        tabPanel("GC Esters",
                 fluidRow(
                   column(6, plotOutput("gcEstersTT1Plot", height = "400px")),
                   column(6, plotOutput("gcEstersTT2Plot", height = "400px"))
                 )
        ),
        
        tabPanel("GC Ketones",
                 fluidRow(
                   column(6, plotOutput("gcKetonesTT1Plot", height = "400px")),
                   column(6, plotOutput("gcKetonesTT2Plot", height = "400px"))
                 )
        ),
        
        tabPanel("Attenuation & pH",
                 fluidRow(
                   column(6, plotOutput("attPlot", height = "400px")),
                   column(6, plotOutput("phPlot",  height = "400px"))
                 )
        ),
        
        tabPanel("Cell Count & Viability",
                 fluidRow(
                   column(6, plotOutput("cellCountPlot", height = "400px")),
                   column(6, plotOutput("viabilityPlot", height = "400px"))
                 )
        ),
        
        # -------------------------------------------------------------------
        # AVERAGES TAB
        # Contains a multi-select for choosing experiments to overlay, then
        # 10 sub-tabs (pill style) — one per analytical measurement.
        # The selectizeInput ID "avg_experiments" is populated by server.R.
        # The plotOutput IDs must exactly match the output$ names in server.R.
        # -------------------------------------------------------------------
        
        tabPanel("Averages",
                 br(),
                 wellPanel(
                   selectizeInput(
                     inputId  = "avg_experiments",
                     label    = "Select experiments to overlay:",
                     choices  = NULL,
                     multiple = TRUE,
                     options  = list(placeholder = "Choose one or more experiments...")
                   )
                 ),
                 tabsetPanel(
                   type = "pills",
                   
                   tabPanel("Sugars & Ethanol",
                            fluidRow(
                              column(12, plotOutput("avgHplcPlot", height = "450px"))
                            )
                   ),
                   
                   tabPanel("Esters",
                            fluidRow(
                              column(6, plotOutput("avgEthylEstersBarPlot",   height = "450px")),
                              column(6, plotOutput("avgAcetateEstersBarPlot", height = "450px"))
                            )
                   ),
                   
                   tabPanel("Higher Alcohols",
                            fluidRow(
                              column(12, plotOutput("avgHigherAlcoholsBarPlot", height = "450px"))
                            )
                   ),
                   
                   tabPanel("Vicinal Diketones",
                            fluidRow(
                              column(12, plotOutput("avgDiketonesPlot", height = "450px"))
                            )
                   ),
                   
                   tabPanel("Attenuation",
                            fluidRow(
                              column(12, plotOutput("avgAttenuationPlot", height = "450px"))
                            )
                   ),
                   
                   tabPanel("pH",
                            fluidRow(
                              column(12, plotOutput("avgPhPlot", height = "450px"))
                            )
                   ),
                   
                   tabPanel("Cell Count",
                            fluidRow(
                              column(12, plotOutput("avgCellCountPlot", height = "450px"))
                            )
                   ),
                   
                   tabPanel("Viability",
                            fluidRow(
                              column(12, plotOutput("avgViabilityPlot", height = "450px"))
                            )
                   ),
                   
                   tabPanel("Cone Viability",
                            fluidRow(
                              column(12, plotOutput("avgConeViabilityPlot", height = "450px"))
                            )
                   )
                 )
        )
      )
    )
  )
)
