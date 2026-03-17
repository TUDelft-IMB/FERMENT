ui <- fluidPage(
  
  titlePanel("FERMENT"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      selectInput("species",     "Species",     choices = c("A", "B")),
      selectInput("strain",      "Strain",      choices = c("StrainA", "StrainB")),
      selectInput("gravity",     "Gravity",     choices = c("A", "B")),
      selectInput("inoculum",    "Inoculum",    choices = c("A", "B", "C")),
      selectInput("temperature", "Temperature", choices = c("12C", "18C", "22C")),
      selectInput("experiment",  "Experiments", choices = NULL),  # populated dynamically by server!
      selectInput("other",       "Other",       choices = c("Default")),
      
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
