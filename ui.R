# =============================================================================
# ui.R
# Defines the visual layout of the app; what the user sees and interacts with.
# Shiny reads this file to build the HTML page. No logic lives here; all
# reactive behaviour is handled in server.R.
# =============================================================================

ui <- navbarPage(
  title = "FERMENT",
  
  # ===========================================================================
  # PAGE 0: OVERVIEW
  # Shows high-level statistics about all loaded experiments
  # ===========================================================================
  tabPanel(
    title = "Overview",
    
    br(),
    
    # Summary cards row
    fluidRow(
      column(3, 
             div(style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
                 h4("Total Experiments"),
                 textOutput("total_experiments_count"),
                 style = "text-align: center; background-color: #f9f9f9;"
             )
      ),
      column(3, 
             div(style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
                 h4("Unique Strains"),
                 textOutput("unique_strains_count"),
                 style = "text-align: center; background-color: #f9f9f9;"
             )
      ),
      column(3, 
             div(style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
                 h4("Unique Species"),
                 textOutput("unique_species_count"),
                 style = "text-align: center; background-color: #f9f9f9;"
             )
      ),
      column(3, 
             div(style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
                 h4("Unique Temperatures"),
                 textOutput("unique_temps_count"),
                 style = "text-align: center; background-color: #f9f9f9;"
             )
      )
    ),
    
    br(),
    
    # Charts and tables
    fluidRow(
      column(6,
             h3("Experiments per Strain"),
             plotOutput("experiments_per_strain_plot", height = "350px")
      ),
      column(6,
             h3("Experiments per Species"),
             plotOutput("experiments_per_species_plot", height = "350px")
      )
    ),
    
    br(),
    
    fluidRow(
      column(6,
             h3("Temperature Distribution"),
             plotOutput("temperature_distribution_plot", height = "350px")
      ),
      column(6,
             h3("Gravity Distribution"),
             plotOutput("gravity_distribution_plot", height = "350px")
      )
    ),
    
    br(),
    
    fluidRow(
      column(12,
             h3("Experimental Conditions Summary"),
             tableOutput("conditions_summary_table")
      )
    ),
    
    br()
  ),
  
  # ===========================================================================
  # PAGE 1: SINGLE EXPERIMENT
  # Has its own sidebar with filters + experiment selector.
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
        # Selecting a value here narrows down which experiments are shown below.
        # ---------------------------------------------------------------------
        selectInput("species",     "Species",     choices = NULL),
        selectInput("strain",      "Strain",      choices = NULL),
        selectInput("gravity",     "Gravity",     choices = NULL),
        selectInput("inoculum",    "Inoculum",    choices = NULL),
        selectInput("temperature", "Temperature", choices = NULL),
        selectInput("experiment",  "Experiments", choices = NULL),
        
        # Note: The label shown is "Experiment #"; the value passed to the
        # server is the actual filename (used to load the correct workbook).
        
        hr(),
        
        # Small text box showing whether a file was successfully loaded or not.
        # Its content is set by output$file_status in server.R.
        verbatimTextOutput("file_status")
      ),
      
      mainPanel(
        width = 10,
        
        # Tab strip: each tab shows a different set of analytical plots.
        # The plot output IDs (e.g. "hplcTT1Plot") must exactly match the
        # output$ names used in server.R.
        tabsetPanel(
          
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
          )
        )
      )
    )
  ),
  
  # ===========================================================================
  # PAGE 2: AVERAGES
  # No sidebar — full-width layout with the multi-select at the top and
  # pill sub-tabs below.
  # The plotOutput IDs must exactly match the output$ names in server.R.
  # ===========================================================================
  tabPanel(
    title = "Averages",
    
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
      
      tabPanel("GC Esters",
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
