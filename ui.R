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
#
# All plotlyOutput() calls are wrapped in withSpinner() from the
# shinycssloaders package. This shows an animated spinner inside the plot
# area while the reactive is computing, rather than leaving a blank space.
# Spinner colour matches the app's primary colour (#0072B2).
# plotlyOutput() here must match renderPlotly() in server.R exactly by ID.
# =============================================================================

# Convenience alias so the rest of the file stays readable.
spinner <- function(...) shinycssloaders::withSpinner(..., color = "#009E73", type = 7, caption = "Loading...")

# Zoom controls
zoom_controls <- tags$head(
  tags$style(HTML("
    #app-zoom-wrapper {
      zoom: 1;
    }
    .zoom-btn-group {
      position: fixed;
      top: 8px;
      right: 16px;
      z-index: 9999;
      display: flex;
      gap: 4px;
      background: white;
      border: 1px solid #ddd;
      border-radius: 6px;
      padding: 4px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.15);
    }
    .zoom-btn-group button {
      border: none;
      border-radius: 4px;
      padding: 4px 10px;
      cursor: pointer;
      font-size: 14px;
      color: white;
    }

    #zoom_out_btn, #zoom_in_btn {
      background: #009E73;
    }

    #zoom_out_btn:hover, #zoom_in_btn:hover {
      background: #005b8c;
    }

    #zoom_reset_btn {
      background: #0072B2;
    }

    #zoom_reset_btn:hover {
      background: #007f5c;
    }
  ")),
  tags$script(HTML("
    var currentZoom = 1;
    function setZoom(val) {
      currentZoom = Math.min(Math.max(val, 0.5), 2);
      document.getElementById('app-zoom-wrapper').style.zoom = currentZoom;
    }
    function zoomIn() { setZoom(currentZoom + 0.1); }
    function zoomOut() { setZoom(currentZoom - 0.1); }
    function zoomReset() { setZoom(1); }
  "))
)

###############
# Start of ui.R
###############

ui <- tagList(
  zoom_controls,
  div(
    class = "zoom-btn-group",
    actionButton("zoom_out_btn", "−", onclick = "zoomOut()"),
    actionButton("zoom_reset_btn", "Reset zoom", onclick = "zoomReset()"),
    actionButton("zoom_in_btn", "+", onclick = "zoomIn()")
  ),
  div(
    id = "app-zoom-wrapper",
    navbarPage(
      title = "FERMENT",
      theme = bs_theme(
        bootswatch = "minty",
        primary = "#0072B2",
        secondary = "#009E73"
      ),

      # ===========================================================================
      # PAGE 0: OVERVIEW
      # Shows high-level statistics about all loaded experiments
      # ===========================================================================
      tabPanel(
        title = span(
          "Overview",
          tooltip(
            trigger = bs_icon(
              "info-circle"
            ),
            "A high-level summary of all experiments.
      Shows species and strain distributions, temperature and gravity breakdowns,
      and a full conditions table."
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
              h4("Unique Species"),
              textOutput("unique_species_count"),
              style = "text-align: center; background-color: #f9f9f9;"
            )
          ),
          column(
            4,
            div(
              style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
              h4("Unique Strains"),
              textOutput("unique_strains_count"),
              style = "text-align: center; background-color: #f9f9f9;"
            )
          ),
          column(
            4,
            div(
              style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
              h4("Total Experiments"),
              textOutput("total_experiments_count"),
              style = "text-align: center; background-color: #f9f9f9;"
            )
          )
        ),
        br(),

        # Charts and tables
        fluidRow(
          column(
            6,
            h3("Experiments per Species"),
            spinner(plotlyOutput("experiments_per_species_plot", height = "350px")),
            br(),
            h3("Gravity Distribution"),
            spinner(plotlyOutput("gravity_distribution_plot", height = "350px")),
            br(),
            h3("Inoculum Distribution"),
            spinner(plotlyOutput("inoculum_distribution_plot", height = "350px")),
            br(),
            h3("Temperature Distribution"),
            spinner(plotlyOutput("temperature_distribution_plot", height = "350px"))
          ),
          column(
            6,
            h3(span(
              "Experiments per Strain",
              tooltip(
                trigger = tags$span(
                  style = "vertical-align: super; font-size: 0.65em;",
                  bs_icon("info-circle")
                ),
                "Top 50 strains are displayed."
              )
            )),
            spinner(plotlyOutput("experiments_per_strain_plot", height = "1650px"))
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            h3("Experimental Conditions Summary"),
            DTOutput("conditions_summary_table")
          )
        ),
        br()
      ),

      # ===========================================================================
      # PAGE 1: SINGLE EXPERIMENT
      # The user picks one Excel workbook at a time via the sidebar filters.
      # Each analytical tab then shows two interactive Plotly charts side by side:
      # one for TT1 (Tall Tube 1) and one for TT2 (Tall Tube 2).
      # ===========================================================================
      tabPanel(
        title = span(
          "Plot Replicates - Single Experiment",
          tooltip(
            trigger = bs_icon(
              "info-circle"
            ),
            "Explore one experiment at a time.
      Filter by species, strain, gravity, inoculum, or temperature, then select an experiment.
      TT1 and TT2 are shown side by side."
          )
        ),
        sidebarLayout(
          sidebarPanel(
            width = 2,

            # -------------------------------------------------------------------
            # Filter dropdowns
            # All choices start as NULL — server.R populates them dynamically by
            # reading the "Experimental_parameters" sheet from each Excel file.
            # Selecting a value here narrows down which experiments are shown in
            # the "Experiments" dropdown below.
            # -------------------------------------------------------------------
            selectInput("species", "Species", choices = NULL),
            selectInput("strain", "Strain", choices = NULL),
            selectInput("gravity", "Gravity", choices = NULL),
            selectInput("inoculum", "Inoculum", choices = NULL),
            selectInput("temperature", "Temperature", choices = NULL),
            actionButton(
              inputId = "clear_filters",
              label   = "Clear filters",
              class   = "btn-sm btn-default",
              width   = "100%"
            ),
            hr(),
            selectInput("experiment", "Experiment", choices = NULL),

            # Note: The label shown is "Experiment #N"; the underlying value
            # passed to server.R is the actual filename of the Excel file.

            hr(),

            # Small text box showing whether a file was successfully loaded or not.
            # Its content is set by output$file_status in server.R.
            verbatimTextOutput("file_status")
          ),
          mainPanel(
            width = 10,

            # -------------------------------------------------------------------
            # Analytical tabs — one per measurement type.
            # All plot IDs must match the output$ names in server.R exactly.
            # Using plotlyOutput() instead of plotOutput() makes every chart
            # interactive (zoom, hover, legend toggle).
            # -------------------------------------------------------------------
            tabsetPanel(
              tabPanel(
                "HPLC",
                # Two Plotly charts side by side: TT1 on the left, TT2 on the right.
                fluidRow(
                  column(
                    6,
                    div(
                      style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
                      span("TT1"),
                      downloadButton("download_hplc_tt1", "Export CSV")
                    ),
                    spinner(plotlyOutput("hplcTT1Plot", height = "400px"))
                  ),
                  column(
                    6,
                    div(
                      style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
                      span("TT2"),
                      downloadButton("download_hplc_tt2", "Export CSV")
                    ),
                    spinner(plotlyOutput("hplcTT2Plot", height = "400px"))
                  )
                )
              ),
              tabPanel(
                "GC Esters",
                fluidRow(
                  column(6, spinner(plotlyOutput("gcEstersTT1Plot", height = "400px"))),
                  column(6, spinner(plotlyOutput("gcEstersTT2Plot", height = "400px")))
                )
              ),
              tabPanel(
                "GC Ketones",
                fluidRow(
                  column(6, spinner(plotlyOutput("gcKetonesTT1Plot", height = "400px"))),
                  column(6, spinner(plotlyOutput("gcKetonesTT2Plot", height = "400px")))
                )
              ),
              tabPanel(
                "Attenuation & pH",
                fluidRow(
                  column(6, spinner(plotlyOutput("attPlot", height = "400px"))),
                  column(6, spinner(plotlyOutput("phPlot", height = "400px")))
                )
              ),
              tabPanel(
                "Cell Count & Viability",
                fluidRow(
                  column(6, spinner(plotlyOutput("cellCountPlot", height = "400px"))),
                  column(6, spinner(plotlyOutput("viabilityPlot", height = "400px")))
                )
              ),
              tabPanel(
                "CO\u2082",
                fluidRow(
                  column(6, spinner(plotlyOutput("CO2Plot", height = "400px"))),
                )
              )              
            ),

            # -------------------------------------------------------------------
            # Selection summary table — below the graphs.
            # Shows the metadata row for the currently loaded experiment.
            # -------------------------------------------------------------------
            hr(),
            h4("Selected experiment"),
            DTOutput("single_summary_table")
          )
        )
      ),


      # ===========================================================================
      # PAGE 2: COMPARE
      # The user narrows down the experiment pool using multi-select filter inputs
      # (one or more values per dimension), then picks which of the matching
      # experiments to overlay from the resulting list.
      # Each analytical tab shows the same chart types as Single Experiment but
      # with multiple experiments overlaid.
      #
      # Filter logic (server.R Step 13):
      # - Leaving a filter empty = no filter applied for that dimension (all pass).
      # - Selecting one or more values = only experiments matching ANY of those
      #   values are included (OR within a dimension, AND across dimensions).
      #
      # Visual encoding per chart type:
      # - HPLC / GC: colour = compound, linetype = experiment; TT1 and TT2
      #   remain on separate side-by-side charts.
      # - Att / pH / Cell Count / Viability: one chart per metric containing
      #   BOTH tubes — colour = experiment, linetype = tube (solid = TT1,
      #   dashed = TT2). This lets the user see within-experiment tube
      #   agreement and cross-experiment differences in a single view.
      #
      # All plotlyOutput() IDs must match output$ names in server.R exactly.
      # ===========================================================================
      tabPanel(
        title = span(
          "Plot Replicates - Multiple Experiments",
          tooltip(
            trigger = bs_icon(
              "info-circle"
            ),
            "Overlay multiple experiments.
      For HPLC and GC, colour = compound and linetype = experiment.
      For Attenuation, pH, Cell Count, and Viability, colour = experiment and linetype = tall tube."
          )
        ),
        sidebarLayout(
          sidebarPanel(
            width = 3,

            # -------------------------------------------------------------------
            # Multi-select filter inputs
            # Choices start as NULL — server.R populates them at startup.
            # Leaving a filter empty shows all experiments for that dimension.
            # Selecting multiple values shows experiments matching ANY of them.
            # -------------------------------------------------------------------
            selectizeInput(
              inputId  = "cmp_species",
              label    = "Species",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All species...")
            ),
            selectizeInput(
              inputId  = "cmp_strain",
              label    = "Strain",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All strains...")
            ),
            selectizeInput(
              inputId  = "cmp_gravity",
              label    = "Gravity",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All gravities...")
            ),
            selectizeInput(
              inputId  = "cmp_inoculum",
              label    = "Inoculum",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All inocula...")
            ),
            selectizeInput(
              inputId  = "cmp_temperature",
              label    = "Temperature",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All temperatures...")
            ),
            actionButton(
              inputId = "cmp_clear_filters",
              label   = "Clear filters",
              class   = "btn-sm btn-default",
              width   = "100%"
            ),
            hr(),

            # -------------------------------------------------------------------
            # Experiment multi-select
            # Populated dynamically by server.R (Step 14) with only the
            # experiments that pass the active filters above.
            # The user picks which of those to actually overlay on the plots.
            # -------------------------------------------------------------------
            selectizeInput(
              inputId  = "cmp_experiments",
              label    = "Experiments to compare:",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "Choose experiments...")
            ),
            hr(),

            # Short status line: how many experiments are currently loaded.
            # Content set by output$cmp_status in server.R.
            verbatimTextOutput("cmp_status")
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "HPLC",
                fluidRow(
                  column(12, spinner(plotlyOutput("cmpHplcPlot", height = "500px")))
                )
              ),
              tabPanel(
                "GC Esters",
                fluidRow(
                  column(12, spinner(plotlyOutput("cmpGcEstersPlot", height = "500px")))
                )
              ),
              tabPanel(
                "GC Ketones",
                fluidRow(
                  column(12, spinner(plotlyOutput("cmpGcKetonesPlot", height = "500px")))
                )
              ),
              tabPanel(
                "Attenuation & pH",
                # One chart per metric; each contains both TT1 (solid) and
                # TT2 (dashed) for all selected experiments.
                fluidRow(
                  column(6, spinner(plotlyOutput("cmpAttPlot", height = "450px"))),
                  column(6, spinner(plotlyOutput("cmpPhPlot", height = "450px")))
                )
              ),
              tabPanel(
                "Cell Count & Viability",
                # Same encoding: solid = TT1, dashed = TT2, colour = experiment.
                fluidRow(
                  column(6, spinner(plotlyOutput("cmpCellCountPlot", height = "450px"))),
                  column(6, spinner(plotlyOutput("cmpViabilityPlot", height = "450px")))
                )
              ),
              tabPanel(
                "CO\u2082",
                # Same encoding: solid = TT1, dashed = TT2, colour = experiment.
                fluidRow(
                  column(12, spinner(plotlyOutput("cmpCO2Plot", height = "500px"))),
                )
              )
            ),

            # -------------------------------------------------------------------
            # Selection summary table — below the graphs.
            # One row per selected experiment.
            # -------------------------------------------------------------------
            hr(),
            h4("Selected experiments"),
            DTOutput("cmp_summary_table")
          )
        )
      ),


      # ===========================================================================
      # PAGE 3: AVERAGES
      # Line-based plots (sugars&ethanol, diketones, attenuation, pH, cell count, viability)
      # are fully interactive with clickable legends for toggling experiments.
      # Bar charts (GC esters, higher alcohols) and cone viability are also
      # interactive for hover/zoom, though their legend clicking is less relevant.
      #
      # All plotlyOutput() IDs must match output$ names in server.R exactly.
      # ===========================================================================
      tabPanel(
        title = span(
          "Plot Averages - Multiple Experiments",
          tooltip(
            trigger = bs_icon(
              "info-circle"
            ),
            "Plot averaged data from TT1 and TT2."
          )
        ),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectizeInput(
              inputId  = "avg_species",
              label    = "Species",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All species...")
            ),
            selectizeInput(
              inputId  = "avg_strain",
              label    = "Strain",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All strains...")
            ),
            selectizeInput(
              inputId  = "avg_gravity",
              label    = "Gravity",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All gravities...")
            ),
            selectizeInput(
              inputId  = "avg_inoculum",
              label    = "Inoculum",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All inocula...")
            ),
            selectizeInput(
              inputId  = "avg_temperature",
              label    = "Temperature",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All temperatures...")
            ),
            actionButton(
              inputId = "avg_clear_filters",
              label   = "Clear filters",
              class   = "btn-sm btn-default",
              width   = "100%"
            ),
            hr(),
            selectizeInput(
              inputId  = "avg_experiments",
              label    = "Select experiments to overlay:",
              choices  = NULL,
              multiple = TRUE,
              options  = list(placeholder = "Choose one or more experiments...")
            )
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              type = "pills",
              tabPanel(
                "Sugars & Ethanol",
                fluidRow(
                  column(12, spinner(plotlyOutput("avgHplcPlot", height = "450px")))
                )
              ),
              tabPanel(
                "GC Esters",
                fluidRow(
                  column(6, spinner(plotlyOutput("avgEthylEstersBarPlot", height = "450px"))),
                  column(6, spinner(plotlyOutput("avgAcetateEstersBarPlot", height = "450px")))
                )
              ),
              tabPanel(
                "Higher Alcohols",
                fluidRow(
                  column(12, spinner(plotlyOutput("avgHigherAlcoholsBarPlot", height = "450px")))
                )
              ),
              tabPanel(
                "Vicinal Diketones",
                fluidRow(
                  column(12, spinner(plotlyOutput("avgDiketonesPlot", height = "450px")))
                )
              ),
              tabPanel(
                "GC Ratio",
                fluidRow(
                  column(12, spinner(plotlyOutput("avg_gc_ratio_plot", height = "450px")))
                )
              ),
              tabPanel(
                "Attenuation",
                fluidRow(
                  column(12, spinner(plotlyOutput("avgAttenuationPlot", height = "450px")))
                )
              ),
              tabPanel(
                "pH",
                fluidRow(
                  column(12, spinner(plotlyOutput("avgPhPlot", height = "450px")))
                )
              ),
              tabPanel(
                "Cell Count",
                fluidRow(
                  column(12, spinner(plotlyOutput("avgCellCountPlot", height = "450px")))
                )
              ),
              tabPanel(
                "Viability",
                fluidRow(
                  column(12, spinner(plotlyOutput("avgViabilityPlot", height = "450px")))
                )
              ),
              tabPanel(
                "Cone Viability",
                fluidRow(
                  column(12, spinner(plotlyOutput("avgConeViabilityPlot", height = "450px")))
                )
              )
            ),

            # -------------------------------------------------------------------
            # Selection summary table — below the graphs.
            # One row per selected experiment.
            # -------------------------------------------------------------------
            hr(),
            h4("Selected experiments"),
            DTOutput("avg_summary_table")
          )
        )
      )
    )
  )
)
