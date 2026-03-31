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
EXCEL_DIR <- Sys.getenv("TT_EXCEL_DIR", "/PATH/PATH")

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

## HPLC TT1 --------------------------------------------------------------

plot_hplc_tt1 <- function(df) {
  ggplot(df) +
    geom_line(aes(x = `Time (h)`, y = `1 Maltotriose (g/L)`, colour = "skyblue")) +
    geom_point(aes(x = `Time (h)`, y = `1 Maltotriose (g/L)`, colour = "skyblue")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Maltotriose (g/L)` - `StDev 1 Maltotriose (g/L)`, ymax = `1 Maltotriose (g/L)` + `StDev 1 Maltotriose (g/L)`, width = 3, colour = "skyblue")) +
    geom_line(aes(x = `Time (h)`, y = `1 Maltose (g/L)`, colour = "maroon")) +
    geom_point(aes(x = `Time (h)`, y = `1 Maltose (g/L)`, colour = "maroon")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Maltose (g/L)` - `StDev 1 Maltose (g/L)`, ymax = `1 Maltose (g/L)` + `StDev 1 Maltose (g/L)`, width = 3, colour = "maroon")) +
    geom_line(aes(x = `Time (h)`, y = `1 Glucose (g/L)`, colour = "gold")) +
    geom_point(aes(x = `Time (h)`, y = `1 Glucose (g/L)`, colour = "gold")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Glucose (g/L)` - `StDev 1 Glucose (g/L)`, ymax = `1 Glucose (g/L)` + `StDev 1 Glucose (g/L)`, width = 3, colour = "gold")) +
    geom_line(aes(x = `Time (h)`, y = `1 Fructose (g/L)`, colour = "forestgreen")) +
    geom_point(aes(x = `Time (h)`, y = `1 Fructose (g/L)`, colour = "forestgreen")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Fructose (g/L)` - `StDev 1 Fructose (g/L)`, ymax = `1 Fructose (g/L)` + `StDev 1 Fructose (g/L)`, width = 3, colour = "forestgreen")) +
    geom_line(aes(x = `Time (h)`, y = `1 Glycerol (g/L)`, colour = "grey")) +
    geom_point(aes(x = `Time (h)`, y = `1 Glycerol (g/L)`, colour = "grey")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Glycerol (g/L)` - `StDev 1 Glycerol (g/L)`, ymax = `1 Glycerol (g/L)` + `StDev 1 Glycerol (g/L)`, width = 3, colour = "grey")) +
    geom_line(aes(x = `Time (h)`, y = `1 Ethanol (g/L)`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `1 Ethanol (g/L)`, colour = "sienna")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethanol (g/L)` - `StDev 1 Ethanol (g/L)`, ymax = `1 Ethanol (g/L)` + `StDev 1 Ethanol (g/L)`, width = 3, colour = "sienna")) +
    scale_colour_identity(name = "Metabolites", guide = "legend", breaks = hplc_colours, labels = hplc_labels) +
    labs(title = "HPLC TT1", x = "Time (h)", y = "Concentration (g/L)") +
    theme_minimal() + theme(legend.position = "right")
}

## HPLC TT2 --------------------------------------------------------------

plot_hplc_tt2 <- function(df) {
  ggplot(df) +
    geom_line(aes(x = `Time (h)`, y = `2 Maltotriose (g/L)`, colour = "skyblue")) +
    geom_point(aes(x = `Time (h)`, y = `2 Maltotriose (g/L)`, colour = "skyblue")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Maltotriose (g/L)` - `StDev 2 Maltotriose (g/L)`, ymax = `2 Maltotriose (g/L)` + `StDev 2 Maltotriose (g/L)`, width = 3, colour = "skyblue")) +
    geom_line(aes(x = `Time (h)`, y = `2 Maltose (g/L)`, colour = "maroon")) +
    geom_point(aes(x = `Time (h)`, y = `2 Maltose (g/L)`, colour = "maroon")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Maltose (g/L)` - `StDev 2 Maltose (g/L)`, ymax = `2 Maltose (g/L)` + `StDev 2 Maltose (g/L)`, width = 3, colour = "maroon")) +
    geom_line(aes(x = `Time (h)`, y = `2 Glucose (g/L)`, colour = "gold")) +
    geom_point(aes(x = `Time (h)`, y = `2 Glucose (g/L)`, colour = "gold")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Glucose (g/L)` - `StDev 2 Glucose (g/L)`, ymax = `2 Glucose (g/L)` + `StDev 2 Glucose (g/L)`, width = 3, colour = "gold")) +
    geom_line(aes(x = `Time (h)`, y = `2 Fructose (g/L)`, colour = "forestgreen")) +
    geom_point(aes(x = `Time (h)`, y = `2 Fructose (g/L)`, colour = "forestgreen")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Fructose (g/L)` - `StDev 2 Fructose (g/L)`, ymax = `2 Fructose (g/L)` + `StDev 2 Fructose (g/L)`, width = 3, colour = "forestgreen")) +
    geom_line(aes(x = `Time (h)`, y = `2 Glycerol (g/L)`, colour = "grey")) +
    geom_point(aes(x = `Time (h)`, y = `2 Glycerol (g/L)`, colour = "grey")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Glycerol (g/L)` - `StDev 2 Glycerol (g/L)`, ymax = `2 Glycerol (g/L)` + `StDev 2 Glycerol (g/L)`, width = 3, colour = "grey")) +
    geom_line(aes(x = `Time (h)`, y = `2 Ethanol (g/L)`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `2 Ethanol (g/L)`, colour = "sienna")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethanol (g/L)` - `StDev 2 Ethanol (g/L)`, ymax = `2 Ethanol (g/L)` + `StDev 2 Ethanol (g/L)`, width = 3, colour = "sienna")) +
    scale_colour_identity(name = "Metabolites", guide = "legend", breaks = hplc_colours, labels = hplc_labels) +
    labs(title = "HPLC TT2", x = "Time (h)", y = "Concentration (g/L)") +
    theme_minimal() + theme(legend.position = "right")
}

##GC Esters TT1 ---------------------------------------------------------

plot_gc_esters_tt1 <- function(gc) {
  ggplot(gc) +
    geom_line(aes(x = `Time (h)`, y = `1 Ethyl acetate`, colour = "skyblue")) +
    geom_point(aes(x = `Time (h)`, y = `1 Ethyl acetate`, colour = "skyblue")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl acetate` - `StDev 1 Ethyl acetate`, ymax = `1 Ethyl acetate` + `StDev 1 Ethyl acetate`, width = 3, colour = "skyblue")) +
    geom_line(aes(x = `Time (h)`, y = `1 Ethanol`, colour = "maroon")) +
    geom_point(aes(x = `Time (h)`, y = `1 Ethanol`, colour = "maroon")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethanol` - `StDev 1 Ethanol`, ymax = `1 Ethanol` + `StDev 1 Ethanol`, width = 3, colour = "maroon")) +
    geom_line(aes(x = `Time (h)`, y = `1 Isobutyl acetate`, colour = "gold")) +
    geom_point(aes(x = `Time (h)`, y = `1 Isobutyl acetate`, colour = "gold")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Isobutyl acetate` - `StDev 1 Isobutyl acetate`, ymax = `1 Isobutyl acetate` + `StDev 1 Isobutyl acetate`, width = 3, colour = "gold")) +
    geom_line(aes(x = `Time (h)`, y = `1 Ethyl butyrate`, colour = "forestgreen")) +
    geom_point(aes(x = `Time (h)`, y = `1 Ethyl butyrate`, colour = "forestgreen")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl butyrate` - `StDev 1 Ethyl butyrate`, ymax = `1 Ethyl butyrate` + `StDev 1 Ethyl butyrate`, width = 3, colour = "forestgreen")) +
    geom_line(aes(x = `Time (h)`, y = `1 Isobutanol`, colour = "grey")) +
    geom_point(aes(x = `Time (h)`, y = `1 Isobutanol`, colour = "grey")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Isobutanol` - `StDev 1 Isobutanol`, ymax = `1 Isobutanol` + `StDev 1 Isobutanol`, width = 3, colour = "grey")) +
    geom_line(aes(x = `Time (h)`, y = `1 Isoamyl acetate`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `1 Isoamyl acetate`, colour = "sienna")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Isoamyl acetate` - `StDev 1 Isoamyl acetate`, ymax = `1 Isoamyl acetate` + `StDev 1 Isoamyl acetate`, width = 3, colour = "sienna")) +
    geom_line(aes(x = `Time (h)`, y = `1 Isoamyl alcohol`, colour = "darkred")) +
    geom_point(aes(x = `Time (h)`, y = `1 Isoamyl alcohol`, colour = "darkred")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Isoamyl alcohol` - `StDev 1 Isoamyl alcohol`, ymax = `1 Isoamyl alcohol` + `StDev 1 Isoamyl alcohol`, width = 3, colour = "darkred")) +
    geom_line(aes(x = `Time (h)`, y = `1 Ethyl hexanoate`, colour = "darkgreen")) +
    geom_point(aes(x = `Time (h)`, y = `1 Ethyl hexanoate`, colour = "darkgreen")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl hexanoate` - `StDev 1 Ethyl hexanoate`, ymax = `1 Ethyl hexanoate` + `StDev 1 Ethyl hexanoate`, width = 3, colour = "darkgreen")) +
    geom_line(aes(x = `Time (h)`, y = `1 Ethyl octanoate`, colour = "lavender")) +
    geom_point(aes(x = `Time (h)`, y = `1 Ethyl octanoate`, colour = "lavender")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl octanoate` - `StDev 1 Ethyl octanoate`, ymax = `1 Ethyl octanoate` + `StDev 1 Ethyl octanoate`, width = 3, colour = "lavender")) +
    geom_line(aes(x = `Time (h)`, y = `1 Ethyl decanoate`, colour = "darkblue")) +
    geom_point(aes(x = `Time (h)`, y = `1 Ethyl decanoate`, colour = "darkblue")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl decanoate` - `StDev 1 Ethyl decanoate`, ymax = `1 Ethyl decanoate` + `StDev 1 Ethyl decanoate`, width = 3, colour = "darkblue")) +
    scale_colour_identity(name = "Metabolites", guide = "legend", breaks = gc_ester_colours, labels = gc_ester_labels) +
    labs(title = "GC Esters TT1", x = "Time (h)", y = "Concentration (mg/L)") +
    theme_minimal() + theme(legend.position = "right")
}

## GC Esters TT2 -----------------------------------------------------------

plot_gc_esters_tt2 <- function(gc) {
  ggplot(gc) +
    geom_line(aes(x = `Time (h)`, y = `2 Ethyl acetate`, colour = "skyblue")) +
    geom_point(aes(x = `Time (h)`, y = `2 Ethyl acetate`, colour = "skyblue")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl acetate` - `StDev 2 Ethyl acetate`, ymax = `2 Ethyl acetate` + `StDev 2 Ethyl acetate`, width = 3, colour = "skyblue")) +
    geom_line(aes(x = `Time (h)`, y = `2 Ethanol`, colour = "maroon")) +
    geom_point(aes(x = `Time (h)`, y = `2 Ethanol`, colour = "maroon")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethanol` - `StDev 2 Ethanol`, ymax = `2 Ethanol` + `StDev 2 Ethanol`, width = 3, colour = "maroon")) +
    geom_line(aes(x = `Time (h)`, y = `2 Isobutyl acetate`, colour = "gold")) +
    geom_point(aes(x = `Time (h)`, y = `2 Isobutyl acetate`, colour = "gold")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Isobutyl acetate` - `StDev 2 Isobutyl acetate`, ymax = `2 Isobutyl acetate` + `StDev 2 Isobutyl acetate`, width = 3, colour = "gold")) +
    geom_line(aes(x = `Time (h)`, y = `2 Ethyl butyrate`, colour = "forestgreen")) +
    geom_point(aes(x = `Time (h)`, y = `2 Ethyl butyrate`, colour = "forestgreen")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl butyrate` - `StDev 2 Ethyl butyrate`, ymax = `2 Ethyl butyrate` + `StDev 2 Ethyl butyrate`, width = 3, colour = "forestgreen")) +
    geom_line(aes(x = `Time (h)`, y = `2 Isobutanol`, colour = "grey")) +
    geom_point(aes(x = `Time (h)`, y = `2 Isobutanol`, colour = "grey")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Isobutanol` - `StDev 2 Isobutanol`, ymax = `2 Isobutanol` + `StDev 2 Isobutanol`, width = 3, colour = "grey")) +
    geom_line(aes(x = `Time (h)`, y = `2 Isoamyl acetate`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `2 Isoamyl acetate`, colour = "sienna")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Isoamyl acetate` - `StDev 2 Isoamyl acetate`, ymax = `2 Isoamyl acetate` + `StDev 2 Isoamyl acetate`, width = 3, colour = "sienna")) +
    geom_line(aes(x = `Time (h)`, y = `2 Isoamyl alcohol`, colour = "darkred")) +
    geom_point(aes(x = `Time (h)`, y = `2 Isoamyl alcohol`, colour = "darkred")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Isoamyl alcohol` - `StDev 2 Isoamyl alcohol`, ymax = `2 Isoamyl alcohol` + `StDev 2 Isoamyl alcohol`, width = 3, colour = "darkred")) +
    geom_line(aes(x = `Time (h)`, y = `2 Ethyl hexanoate`, colour = "darkgreen")) +
    geom_point(aes(x = `Time (h)`, y = `2 Ethyl hexanoate`, colour = "darkgreen")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl hexanoate` - `StDev 2 Ethyl hexanoate`, ymax = `2 Ethyl hexanoate` + `StDev 2 Ethyl hexanoate`, width = 3, colour = "darkgreen")) +
    geom_line(aes(x = `Time (h)`, y = `2 Ethyl octanoate`, colour = "lavender")) +
    geom_point(aes(x = `Time (h)`, y = `2 Ethyl octanoate`, colour = "lavender")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl octanoate` - `StDev 2 Ethyl octanoate`, ymax = `2 Ethyl octanoate` + `StDev 2 Ethyl octanoate`, width = 3, colour = "lavender")) +
    geom_line(aes(x = `Time (h)`, y = `2 Ethyl decanoate`, colour = "darkblue")) +
    geom_point(aes(x = `Time (h)`, y = `2 Ethyl decanoate`, colour = "darkblue")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl decanoate` - `StDev 2 Ethyl decanoate`, ymax = `2 Ethyl decanoate` + `StDev 2 Ethyl decanoate`, width = 3, colour = "darkblue")) +
    scale_colour_identity(name = "Metabolites", guide = "legend", breaks = gc_ester_colours, labels = gc_ester_labels) +
    labs(title = "GC Esters TT2", x = "Time (h)", y = "Concentration (mg/L)") +
    theme_minimal() + theme(legend.position = "right")
}

## GC Ketones TT1 --------------------------------------------------------

plot_gc_ketones_tt1 <- function(gck) {
  ggplot(gck) +
    geom_line(aes(x = `Time (h)`, y = `1 Diacetyl`, colour = "skyblue")) +
    geom_point(aes(x = `Time (h)`, y = `1 Diacetyl`, colour = "skyblue")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 Diacetyl` - `StDev 1 Diacetyl`, ymax = `1 Diacetyl` + `StDev 1 Diacetyl`, width = 3, colour = "skyblue")) +
    geom_line(aes(x = `Time (h)`, y = `1 2,3-Pentanedione`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `1 2,3-Pentanedione`, colour = "sienna")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `1 2,3-Pentanedione` - `StDev 1 2,3-Pentanedione`, ymax = `1 2,3-Pentanedione` + `StDev 1 2,3-Pentanedione`, width = 3, colour = "sienna")) +
    scale_colour_identity(name = "Diketones", guide = "legend", breaks = gc_ketone_colours, labels = gc_ketone_labels) +
    labs(title = "GC Ketones TT1", x = "Time (h)", y = "Concentration (mg/L)") +
    theme_minimal() + theme(legend.position = "right")
}

## GC Ketones TT2 --------------------------------------------------------

plot_gc_ketones_tt2 <- function(gck) {
  ggplot(gck) +
    geom_line(aes(x = `Time (h)`, y = `2 Diacetyl`, colour = "skyblue")) +
    geom_point(aes(x = `Time (h)`, y = `2 Diacetyl`, colour = "skyblue")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 Diacetyl` - `StDev 2 Diacetyl`, ymax = `2 Diacetyl` + `StDev 2 Diacetyl`, width = 3, colour = "skyblue")) +
    geom_line(aes(x = `Time (h)`, y = `2 2,3-Pentanedione`, colour = "sienna")) +
    geom_point(aes(x = `Time (h)`, y = `2 2,3-Pentanedione`, colour = "sienna")) +
    geom_errorbar(aes(x = `Time (h)`, ymin = `2 2,3-Pentanedione` - `StDev 2 2,3-Pentanedione`, ymax = `2 2,3-Pentanedione` + `StDev 2 2,3-Pentanedione`, width = 3, colour = "sienna")) +
    scale_colour_identity(name = "Diketones", guide = "legend", breaks = gc_ketone_colours, labels = gc_ketone_labels) +
    labs(title = "GC Ketones TT2", x = "Time (h)", y = "Concentration (mg/L)") +
    theme_minimal() + theme(legend.position = "right")
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

