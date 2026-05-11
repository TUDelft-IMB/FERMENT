# =============================================================================
# ui.R
# Defines the visual layout of the app; what the user sees and interacts with.
# Shiny reads this file to build the HTML page. No logic lives here; all
# reactive behaviour is handled in server.R.
#
# ALL plots use plotlyOutput() so every chart is interactive:
#   - Zoom and pan with the mouse
#   - Hover over points to see exact values
#   - Click legend items to show/hide individual traces
#   - Double-click a legend item to isolate it
#
# plotlyOutput() here must match renderPlotly() in server.R exactly by ID.
# =============================================================================

ui <- navbarPage(
  title = "FERMENT",
  
  # ===========================================================================
  # PAGE 1: SINGLE EXPERIMENT
  # The user picks one Excel workbook at a time via the sidebar filters.
  # Each analytical tab then shows two interactive Plotly charts side by side:
  # one for TT1 (Tall Tube 1) and one for TT2 (Tall Tube 2).
  # ===========================================================================
  tabPanel(
    title = "Single Experiment",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        
        # ---------------------------------------------------------------------
        # Filter dropdowns
        # All choices start as NULL — server.R populates them dynamically by
        # reading the "Experimental_parameters" sheet from each Excel file.
        # Selecting a value here narrows down which experiments are shown in
        # the "Experiments" dropdown below.
        # ---------------------------------------------------------------------
        selectInput("species",     "Species",     choices = NULL),
        selectInput("strain",      "Strain",      choices = NULL),
        selectInput("gravity",     "Gravity",     choices = NULL),
        selectInput("inoculum",    "Inoculum",    choices = NULL),
        selectInput("temperature", "Temperature", choices = NULL),
        selectInput("experiment",  "Experiments", choices = NULL),
        
        # Note: The label shown is "Experiment #N"; the underlying value
        # passed to server.R is the actual filename of the Excel file.
        
        hr(),
        
        # Small text box showing whether a file was successfully loaded or not.
        # Its content is set by output$file_status in server.R.
        verbatimTextOutput("file_status")
      ),
      
      mainPanel(
        width = 10,
        
        # ---------------------------------------------------------------------
        # Analytical tabs — one per measurement type.
        # All plot IDs must match the output$ names in server.R exactly.
        # Using plotlyOutput() instead of plotOutput() makes every chart
        # interactive (zoom, hover, legend toggle).
        # ---------------------------------------------------------------------
        tabsetPanel(
          
          tabPanel("HPLC",
                   # Two Plotly charts side by side: TT1 on the left, TT2 on the right.
                   fluidRow(
                     column(6, plotlyOutput("hplcTT1Plot", height = "400px")),
                     column(6, plotlyOutput("hplcTT2Plot", height = "400px"))
                   )
          ),
          
          tabPanel("GC Esters",
                   fluidRow(
                     column(6, plotlyOutput("gcEstersTT1Plot", height = "400px")),
                     column(6, plotlyOutput("gcEstersTT2Plot", height = "400px"))
                   )
          ),
          
          tabPanel("GC Ketones",
                   fluidRow(
                     column(6, plotlyOutput("gcKetonesTT1Plot", height = "400px")),
                     column(6, plotlyOutput("gcKetonesTT2Plot", height = "400px"))
                   )
          ),
          
          tabPanel("Attenuation & pH",
                   fluidRow(
                     column(6, plotlyOutput("attPlot", height = "400px")),
                     column(6, plotlyOutput("phPlot",  height = "400px"))
                   )
          ),
          
          tabPanel("Cell Count & Viability",
                   fluidRow(
                     column(6, plotlyOutput("cellCountPlot", height = "400px")),
                     column(6, plotlyOutput("viabilityPlot", height = "400px"))
                   )
          )
        )
      )
    )
  ),
  
  # ===========================================================================
  # PAGE 2: AVERAGES
  # No sidebar here — the page is full-width.
  # The user first selects which experiments to overlay using the multi-select
  # at the top, then navigates through the pill tabs to view different metrics.
  #
  # Line-based plots (sugars&ethanol, diketones, attenuation, pH, cell count, viability)
  # are fully interactive with clickable legends for toggling experiments.
  # Bar charts (GC esters, higher alcohols) and cone viability are also
  # interactive for hover/zoom, though their legend clicking is less relevant.
  #
  # All plotlyOutput() IDs must match output$ names in server.R exactly.
  # ===========================================================================
  tabPanel(
    title = "Averages",
    
    br(),
    wellPanel(
      # Multi-select for choosing which experiments to overlay.
      # Choices are populated dynamically by server.R via updateSelectInput().
      # The user can select any number of experiments; each appears as a
      # separate line (or bar group) in the plots below.
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
                 column(12, plotlyOutput("avgHplcPlot", height = "450px"))
               )
      ),
      
      tabPanel("GC Esters",
               # Two stacked bar charts side by side:
               fluidRow(
                 column(6, plotlyOutput("avgEthylEstersBarPlot",   height = "450px")),
                 column(6, plotlyOutput("avgAcetateEstersBarPlot", height = "450px"))
               )
      ),
      
      tabPanel("Higher Alcohols",
               fluidRow(
                 column(12, plotlyOutput("avgHigherAlcoholsBarPlot", height = "450px"))
               )
      ),
      
      tabPanel("Vicinal Diketones",
               fluidRow(
                 column(12, plotlyOutput("avgDiketonesPlot", height = "450px"))
               )
      ),
      
      tabPanel("Attenuation",
               fluidRow(
                 column(12, plotlyOutput("avgAttenuationPlot", height = "450px"))
               )
      ),
      
      tabPanel("pH",
               fluidRow(
                 column(12, plotlyOutput("avgPhPlot", height = "450px"))
               )
      ),
      
      tabPanel("Cell Count",
               fluidRow(
                 column(12, plotlyOutput("avgCellCountPlot", height = "450px"))
               )
      ),
      
      tabPanel("Viability",
               fluidRow(
                 column(12, plotlyOutput("avgViabilityPlot", height = "450px"))
               )
      ),
      
      tabPanel("Cone Viability",
               # Bar chart with one bar per experiment — shows cone viability at
               # the end of fermentation with error bars.
               fluidRow(
                 column(12, plotlyOutput("avgConeViabilityPlot", height = "450px"))
               )
      )
    )
  )
)
