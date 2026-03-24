ui <- fluidPage(
  
  titlePanel("FERMENT"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      selectInput("species",     "Species",     choices = NULL),
      selectInput("strain",      "Strain",      choices = NULL),
      selectInput("gravity",     "Gravity",     choices = NULL),
      selectInput("inoculum",    "Inoculum",    choices = NULL),
      selectInput("temperature", "Temperature", choices = NULL),
      selectInput("experiment",  "Experiments", choices = NULL),
      
      
      hr(),
      verbatimTextOutput("file_status")
    ),
    
    mainPanel(
      width = 10,
      
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
                   column(6, plotOutput("attPlot",  height = "400px")),
                   column(6, plotOutput("phPlot",   height = "400px"))
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
)
