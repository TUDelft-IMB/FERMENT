# =============================================================================
# global.R
# Loaded once when the app starts. Makes libraries, config, data-loading
# functions, colour palettes, and plot functions available to both ui.R
# and server.R.
# =============================================================================

# -----------------------------------------------------------------------------
# LIBRARIES
# Each package adds specific functionality:
#   shiny     - the web-app framework itself
#   readxl    - reading .xlsx Excel files
#   dplyr     - data manipulation (filter, mutate, etc.)
#   tidyr     - reshaping data between wide and long formats
#   ggplot2   - creating plots
#   janitor   - cleaning data frames
#   purrr     - functional programming helpers (map, etc.)
# -----------------------------------------------------------------------------

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(purrr)

# -----------------------------------------------------------------------------
# CONFIG
# EXCEL_DIR points to the folder that holds all .xlsx files.
# It is read from an environment variable (TT_EXCEL_DIR) so that no file
# path is ever hard-coded in the source code.
# -----------------------------------------------------------------------------

# Change path accordingly
EXCEL_DIR <- Sys.getenv("TT_EXCEL_DIR", "/Users/eknibbe1/Ferment_local_datavis/excel_files")

# -----------------------------------------------------------------------------
# LOAD DATA
# read_tt_workbook() reads every sheet from a single Excel workbook into a
# named list, so each sheet becomes one data frame accessible by name.
# The "Attenuation" and "pH" sheets need special treatment: their first
# data row is actually a second header, so row_to_names() promotes it and
# type.convert() makes all columns the correct data type automatically.
# -----------------------------------------------------------------------------

read_tt_workbook <- function(file_path) {
  # Get the names of all sheets in the workbook
  sheets <- excel_sheets(file_path)
  # Read every sheet into a list of data frames
  data_list <- lapply(sheets, function(x) read_excel(file_path, sheet = x))
  # Name each element of the list after its sheet
  names(data_list) <- sheets
  
  # Fix the Attenuation sheet: promote row 1 to column names, then
  # automatically convert all columns to the right type (numeric, etc.)
  if ("Attenuation" %in% names(data_list)) {
    data_list[["Attenuation"]] <- data_list[["Attenuation"]] |>
      row_to_names(row_number = 1) |>
      type.convert(as.is = TRUE)
  }
  
  # Fix the pH sheet: promote row 1 to column names, then
  # automatically convert all columns to the right type (numeric, etc.)
  if ("pH" %in% names(data_list)) {
    data_list[["pH"]] <- data_list[["pH"]] |>
      row_to_names(row_number = 1) |>
      type.convert(as.is = TRUE)
  }
  # Return the complete named list of data frames
  data_list
}

# -----------------------------------------------------------------------------
# COLOUR HELPERS
# These vectors map colours to metabolite/compound names and are reused
# across all plot functions to keep the legend consistent everywhere.
# Each colour at position [i] corresponds to the label at position [i].
# -----------------------------------------------------------------------------

# HPLC: 6 metabolites
hplc_colours <- c("skyblue", "maroon", "gold", "forestgreen", "grey", "sienna")
hplc_labels  <- c("Maltotriose", "Maltose", "Glucose", "Fructose", "Glycerol", "Ethanol")

# GC Esters: 10 compounds
gc_ester_colours <- c("skyblue","maroon","gold","forestgreen","grey","sienna","darkred","darkgreen","lavender","darkblue")
gc_ester_labels  <- c("Ethyl acetate","Ethanol","Isobutyl acetate","Ethyl butyrate","Isobutanol",
                      "Isoamyl acetate","Isoamyl alcohol","Ethyl hexanoate","Ethyl octanoate","Ethyl decanoate")

# GC Ketones: 2 ketones
gc_ketone_colours <- c("skyblue", "sienna")
gc_ketone_labels  <- c("Diacetyl", "2,3-Pentanedione")

# TT1 vs TT2 (used for Attenuation, pH, Cell Count, Viability)
tt_colours <- c("skyblue", "sienna")
tt_labels  <- c("TT1", "TT2")


# -----------------------------------------------------------------------------
# PLOT FUNCTIONS
# Each function takes a single data frame (one sheet from the workbook) and
# returns a ggplot object. All plots follow the same pattern:
#   1. geom_line  — draw connecting lines between time points
#   2. geom_point — draw dots at each measured time point
#   3. geom_errorbar — draw ± standard deviation bars around each point
#   4. scale_colour_identity — tell ggplot the colour strings ARE the colours
#      (not a factor), and build a legend from the breaks/labels vectors above
#   5. labs — set title and axis labels
#   6. theme_minimal + legend.position — clean look, legend on the right
#
# TT1 = Tall Tube 1, TT2 = Tall Tube 2
#
# Column names prefixed with "1 " belong to TT1, "2 " to TT2.
# "StDev" columns hold the standard deviation used for the error bars.
# -----------------------------------------------------------------------------

## HPLC TT1 & TT2 --------------------------------------------------------------
plot_hplc_tube <- function(df, tube_num) {
  # Map base metabolite names to their colors
  metabolites_map <- c(
    "Maltotriose (g/L)" = "skyblue",
    "Maltose (g/L)"     = "maroon",
    "Glucose (g/L)"     = "gold",
    "Fructose (g/L)"    = "forestgreen",
    "Glycerol (g/L)"    = "grey",
    "Ethanol (g/L)"     = "sienna"
  )
  
  # Initialize the base plot
  p <- ggplot(df, aes(x = `Time (h)`))
  
  # Iterate over each base metabolite name
  for (base_metab in names(metabolites_map)) {
    
    # Construct the column names based on the tube number
    val_col <- paste(tube_num, base_metab) 
    stdev_col <- paste("StDev", val_col) 
    
    # Add the layers. 
    # Use !! (bang-bang) to inject the literal string of the metabolite name 
    # into the aesthetic mapping right now, rather than evaluating it later.
    p <- p +
      geom_line(aes(y = .data[[val_col]], color = !!base_metab)) +
      geom_point(aes(y = .data[[val_col]], color = !!base_metab)) +
      geom_errorbar(aes(
        ymin = .data[[val_col]] - .data[[stdev_col]],
        ymax = .data[[val_col]] + .data[[stdev_col]],
        color = !!base_metab
      ), width = 3)
  }
  
  # Add the final formatting
  p <- p +
    # Use scale_color_manual to map the names we injected above to their respective colors
    scale_color_manual(
      name = "Metabolites", 
      values = metabolites_map
    ) +
    labs(
      title = paste("HPLC TT", tube_num, sep = ""), 
      x = "Time (h)", 
      y = "Concentration (g/L)"
    ) +
    theme_minimal() + 
    theme(legend.position = "right")
  
  return(p)
}

## GC Esters TT1 & TT2 --------------------------------------------------------------
plot_gc_esters_tube <- function(df, tube_num) {
  # Map base metabolite names to their colors
  metabolites_map <- c("Ethyl acetate"     = "skyblue",
                       "Ethanol"           = "maroon",
                       "Isobutyl acetate"  = "gold",
                       "Ethyl butyrate"    = "forestgreen",
                       "Isobutanol"        = "grey",
                       "Isoamyl acetate"   = "sienna",
                       "Isoamyl alcohol"   = "darkred",
                       "Ethyl hexanoate"   = "darkgreen",
                       "Ethyl octanoate"   = "lavender",
                       "Ethyl decanoate"   = "darkblue"
                       )
  # Initialize the base plot
  p <- ggplot(df, aes(x = `Time (h)`))
  
  # Iterate over each base metabolite name
  for (base_metab in names(metabolites_map)) {
    
    # Construct the column names based on the tube number
    val_col <- paste(tube_num, base_metab) 
    stdev_col <- paste("StDev", val_col) 
    
    # Add the layers. 
    # Use !! (bang-bang) to inject the literal string of the metabolite name 
    # into the aesthetic mapping right now, rather than evaluating it later.
    p <- p +
      geom_line(aes(y = .data[[val_col]], color = !!base_metab)) +
      geom_point(aes(y = .data[[val_col]], color = !!base_metab)) +
      geom_errorbar(aes(
        ymin = .data[[val_col]] - .data[[stdev_col]],
        ymax = .data[[val_col]] + .data[[stdev_col]],
        color = !!base_metab
      ), width = 3)
  }
  
  # Add the final formatting
  p <- p +
    # Use scale_color_manual to map the names we injected above to their respective colors
    scale_color_manual(
      name = "Metabolites", 
      values = metabolites_map
    ) +
    labs(
      title = paste("GC Esters TT", tube_num, sep = ""), 
      x = "Time (h)", 
      y = "Concentration (mg/L)"
    ) +
    theme_minimal() + 
    theme(legend.position = "right")
  
  return(p)
}


## GC Ketones TT1 & TT2 --------------------------------------------------------------
plot_gc_ketones_tube <- function(df, tube_num) {
  # Map base metabolite names to their colors
  metabolites_map <- c("Diacetyl"          = "skyblue",
                       "2,3-Pentanedione"  = "sienna"
                       )
  # Initialize the base plot
  p <- ggplot(df, aes(x = `Time (h)`))
  
  # Iterate over each base metabolite name
  for (base_metab in names(metabolites_map)) {
    
    # Construct the column names based on the tube number
    val_col <- paste(tube_num, base_metab) 
    stdev_col <- paste("StDev", val_col) 
    
    # Add the layers. 
    # Use !! (bang-bang) to inject the literal string of the metabolite name 
    # into the aesthetic mapping right now, rather than evaluating it later.
    p <- p +
      geom_line(aes(y = .data[[val_col]], color = !!base_metab)) +
      geom_point(aes(y = .data[[val_col]], color = !!base_metab)) +
      geom_errorbar(aes(
        ymin = .data[[val_col]] - .data[[stdev_col]],
        ymax = .data[[val_col]] + .data[[stdev_col]],
        color = !!base_metab
      ), width = 3)
  }
  
  # Add the final formatting
  p <- p +
    # Use scale_color_manual to map the names we injected above to their respective colors
    scale_color_manual(
      name = "Metabolites", 
      values = metabolites_map
    ) +
    labs(
      title = paste("GC Ketones TT", tube_num, sep = ""), 
      x = "Time (h)", 
      y = "Concentration (mg/L)"
    ) +
    theme_minimal() + 
    theme(legend.position = "right")
  
  return(p)
  }


## Attenuation -----------------------------------------------------------

plot_att <- function(att) {
  ggplot(att) +
    geom_line(aes(x = `Time (h)`, y = `TT1`, colour = "skyblue")) +
    geom_point(aes(x = `Time (h)`, y = `TT1`, colour = "skyblue")) +
    geom_line(aes(x = `Time (h)`, y = `TT2`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `TT2`, colour = "sienna")) +
    scale_colour_identity(name = "Attenuation", guide = "legend", breaks = tt_colours, labels = tt_labels) +
    labs(title = "Attenuation", x = "Time (h)", y = "Attenuation (degrees P)") +
    theme_minimal() + theme(legend.position = "right")
}

## pH --------------------------------------------------------------------

plot_ph <- function(ph) {
  ggplot(ph) +
    geom_line(aes(x = `Time (h)`, y = `TT1`, colour = "yellow")) +
    geom_point(aes(x = `Time (h)`, y = `TT1`, colour = "yellow")) +
    geom_line(aes(x = `Time (h)`, y = `TT2`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `TT2`, colour = "sienna")) +
    scale_colour_identity(name = "pH", guide = "legend", breaks = c("yellow", "sienna"), labels = tt_labels) +
    scale_y_continuous(limits = c(0, 7)) +
    labs(title = "pH", x = "Time (h)", y = "pH") +
    theme_minimal() + theme(legend.position = "right")
}

## Cell count ------------------------------------------------------------

plot_cell_count <- function(viability) {
  ggplot(viability) +
    geom_line(aes(x = `Time (h)`, y = `1 Total cells`, colour = "skyblue")) +
    geom_point(aes(x = `Time (h)`, y = `1 Total cells`, colour = "skyblue")) +
    geom_line(aes(x = `Time (h)`, y = `2 Total cells`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `2 Total cells`, colour = "sienna")) +
    scale_colour_identity(name = "Cell count", guide = "legend", breaks = tt_colours, labels = tt_labels) +
    labs(title = "Cell Count", x = "Time (h)", y = "Cell count (cells/ml)") +
    theme_minimal() + theme(legend.position = "right")
}

## Viability ------------------------------------------------------------

plot_viability <- function(viability) {
  ggplot(viability) +
    geom_line(aes(x = `Time (h)`, y = `1 Viability (%)`, colour = "skyblue")) +
    geom_point(aes(x = `Time (h)`, y = `1 Viability (%)`, colour = "skyblue")) +
    geom_line(aes(x = `Time (h)`, y = `2 Viability (%)`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `2 Viability (%)`, colour = "sienna")) +
    scale_colour_identity(name = "Viability", guide = "legend", breaks = tt_colours, labels = tt_labels) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Viability", x = "Time (h)", y = "Viability (fraction)") +
    theme_minimal() + theme(legend.position = "right")
}

