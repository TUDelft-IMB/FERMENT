# Libraries ---------------------------------------------------------------

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(purrr)

#  CONFIG (server-side only, set via env var, never hardcode in repo) ----

# Change path accordingly
EXCEL_DIR <- Sys.getenv("TT_EXCEL_DIR", "/Users/PATH/PATH")

# Load data ---------------------------------------------------------------

read_tt_workbook <- function(file_path) {
  sheets <- excel_sheets(file_path)
  data_list <- lapply(sheets, function(x) read_excel(file_path, sheet = x))
  names(data_list) <- sheets
  
  if ("Attenuation" %in% names(data_list)) {
    data_list[["Attenuation"]] <- data_list[["Attenuation"]] |>
      row_to_names(row_number = 1) |>
      type.convert(as.is = TRUE)
  }
  if ("pH" %in% names(data_list)) {
    data_list[["pH"]] <- data_list[["pH"]] |>
      row_to_names(row_number = 1) |>
      type.convert(as.is = TRUE)
  }
  data_list
}

# Colour helpers ----------------------------------------------------------

hplc_colours <- c("skyblue", "maroon", "gold", "forestgreen", "grey", "sienna")
hplc_labels  <- c("Maltotriose", "Maltose", "Glucose", "Fructose", "Glycerol", "Ethanol")


gc_ester_colours <- c("skyblue","maroon","gold","forestgreen","grey","sienna","darkred","darkgreen","lavender","darkblue")
gc_ester_labels  <- c("Ethyl acetate","Ethanol","Isobutyl acetate","Ethyl butyrate","Isobutanol",
                      "Isoamyl acetate","Isoamyl alcohol","Ethyl hexanoate","Ethyl octanoate","Ethyl decanoate")

gc_ketone_colours <- c("skyblue", "sienna")
gc_ketone_labels  <- c("Diacetyl", "2,3-Pentanedione")

tt_colours <- c("skyblue", "sienna")
tt_labels  <- c("TT1", "TT2")


# Plot functions ----------------------------------------------------------

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

