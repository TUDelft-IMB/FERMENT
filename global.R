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
library(scales)   # percent_format() used in cone viability plot

# -----------------------------------------------------------------------------
# CONFIG
# EXCEL_DIR points to the folder that holds all .xlsx files.
# It is read from an environment variable (TT_EXCEL_DIR) so that no file
# path is ever hard-coded in the source code.
# -----------------------------------------------------------------------------

# Change path accordingly

#EXCEL_DIR <- Sys.getenv("TT_EXCEL_DIR", "PATH/PATH")


# -----------------------------------------------------------------------------
# LOAD DATA
# read_tt_workbook() reads every sheet from a single workbook into a named list.
# Attenuation and pH sheets have a second header row that needs promoting.
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

# read_avg_sheet() reads only the averages_to_plot sheet.
# Returns NULL silently if the sheet is missing
# [TODO] - decide on this functionality!
read_avg_sheet <- function(file_path) {
  tryCatch(
    read_excel(file_path, sheet = "averages_to_plot", col_names = TRUE),
    error = function(e) NULL
  )
}

# -----------------------------------------------------------------------------
# COLOUR HELPERS — single-experiment plots
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
# COLOUR HELPERS — averages plots
# -----------------------------------------------------------------------------
# One linetype per selected experiment; compounds stay colour-coded
exp_linetypes_palette <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")

# Ethyl esters stacked bar
ethyl_ester_colours <- c("darkblue", "sienna", "darkgreen", "skyblue")
ethyl_ester_labels  <- c("Ethyl butyrate", "Ethyl hexanoate", "Ethyl octanoate", "Ethyl decanoate")

# Acetate esters stacked bar
acetate_colours <- c("skyblue", "orange", "forestgreen")
acetate_labels  <- c("Ethyl acetate", "Isobutyl acetate", "Isoamyl acetate")

# Higher alcohols stacked bar
alcohol_colours <- c("darkblue", "orange")
alcohol_labels  <- c("Isobutanol", "Isoamyl alcohol")

# -----------------------------------------------------------------------------
# PLOT FUNCTIONS — SINGLE EXPERIMENT
# Each function takes one data frame and returns a ggplot object.
# TT1/TT2 column names are constructed from tube_num inside the loop.
# -----------------------------------------------------------------------------

## HPLC -----------------------------------------------------------------------

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

## GC Esters ------------------------------------------------------------------
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


## GC Ketones -----------------------------------------------------------------
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

# =============================================================================
# PLOT FUNCTIONS — AVERAGES (averages_to_plot sheet)
# Accept a list of data frames (one per selected experiment) plus a matching
# vector of labels. Colour = compound, linetype = experiment.
# =============================================================================

# -----------------------------------------------------------------------------
# Generic line overlay helper
# -----------------------------------------------------------------------------

plot_averages <- function(df_list, exp_labels,
                          avg_cols, sd_cols,
                          comp_colours, comp_labels,
                          title = "", y_label = "", y_limits = NULL) {
  
  linetypes <- exp_linetypes_palette[seq_along(df_list)]
  
  plot_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]]
    do.call(rbind, lapply(seq_along(avg_cols), function(i) {
      avg_col <- avg_cols[i]
      sd_col  <- if (!is.null(sd_cols)) sd_cols[i] else NA_character_
      data.frame(
        time       = df[["Time (h)"]],
        value      = if (avg_col %in% names(df)) as.numeric(df[[avg_col]]) else NA_real_,
        sd         = if (!is.na(sd_col) && sd_col %in% names(df)) as.numeric(df[[sd_col]]) else NA_real_,
        compound   = comp_labels[i],
        colour     = comp_colours[i],
        experiment = exp_labels[e],
        linetype   = linetypes[e],
        stringsAsFactors = FALSE
      )
    }))
  }))
  plot_data <- plot_data[!is.na(plot_data$value), ]
  
  p <- ggplot(plot_data,
              aes(x = time, y = value,
                  colour   = colour,
                  linetype = linetype,
                  group    = interaction(compound, experiment))) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 3, na.rm = TRUE) +
    scale_colour_identity(name = "Compound", guide = "legend",
                          breaks = comp_colours, labels = comp_labels) +
    scale_linetype_identity(name = "Experiment", guide = "legend",
                            breaks = linetypes[seq_along(df_list)], labels = exp_labels) +
    labs(title = title, x = "Time (h)", y = y_label) +
    theme_minimal() + theme(legend.position = "right")
  
  if (!is.null(y_limits)) p <- p + scale_y_continuous(limits = y_limits)
  p
}

# -----------------------------------------------------------------------------
# Generic stacked bar helper — uses last non-NA value (fermentation endpoint)
# -----------------------------------------------------------------------------

plot_avg_stacked_bar <- function(df_list, exp_labels,
                                 avg_cols, comp_colours, comp_labels,
                                 title = "", y_label = "") {
  
  bar_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]]
    do.call(rbind, lapply(seq_along(avg_cols), function(i) {
      vals     <- as.numeric(df[[avg_cols[i]]])
      last_val <- if (any(!is.na(vals))) tail(vals[!is.na(vals)], 1) else NA_real_
      data.frame(experiment = exp_labels[e],
                 compound   = comp_labels[i],
                 value      = last_val,
                 stringsAsFactors = FALSE)
    }))
  }))
  bar_data <- bar_data[!is.na(bar_data$value), ]
  bar_data$experiment <- factor(bar_data$experiment, levels = exp_labels)
  bar_data$compound   <- factor(bar_data$compound,   levels = comp_labels)
  
  ggplot(bar_data, aes(x = "", y = value, fill = compound)) +
    geom_col(position = "stack", width = 0.6) +
    facet_wrap(~ experiment, nrow = 1) +
    scale_fill_manual(name   = "Compound",
                      values = setNames(comp_colours, comp_labels)) +
    labs(title = title, x = NULL, y = y_label) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x     = element_blank(),
          axis.ticks.x    = element_blank(),
          strip.text      = element_text(size = 10))
}

# -----------------------------------------------------------------------------
# Specific averages plot functions
# -----------------------------------------------------------------------------

plot_avg_cell_count <- function(df_list, exp_labels) {
  plot_averages(df_list, exp_labels,
                avg_cols     = "CellCount_average",
                sd_cols      = "CellCount_stdev",
                comp_colours = "skyblue",
                comp_labels  = "Cell Count",
                title        = "Cell Count",
                y_label      = "Cell count (cells/ml)")
}

plot_avg_viability <- function(df_list, exp_labels) {
  plot_averages(df_list, exp_labels,
                avg_cols     = "Viability_average",
                sd_cols      = "Viability_stdev",
                comp_colours = "forestgreen",
                comp_labels  = "Viability",
                title        = "Viability",
                y_label      = "Viability (fraction)",
                y_limits     = c(0, 1))
}

plot_avg_cone_viability <- function(df_list, exp_labels) {
  bar_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]]
    data.frame(
      experiment = exp_labels[e],
      value      = as.numeric(df[["Cone_viability_average"]][1]),
      sd         = as.numeric(df[["Stdev_cone_viability"]][1]),
      stringsAsFactors = FALSE
    )
  }))
  bar_data <- bar_data[!is.na(bar_data$value), ]
  bar_data$experiment <- factor(bar_data$experiment, levels = exp_labels)
  
  ggplot(bar_data, aes(x = experiment, y = value, fill = experiment)) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 0.15) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(title = "Cone Viability", x = "Experiment", y = "Cone Viability (%)") +
    theme_minimal() + theme(legend.position = "none")
}

plot_avg_attenuation <- function(df_list, exp_labels) {
  plot_averages(df_list, exp_labels,
                avg_cols     = "Attenuation_average",
                sd_cols      = "Attenuation_stdev",
                comp_colours = "skyblue",
                comp_labels  = "Attenuation",
                title        = "Attenuation",
                y_label      = "Attenuation (degrees P)")
}

plot_avg_ph <- function(df_list, exp_labels) {
  plot_averages(df_list, exp_labels,
                avg_cols     = "pH_average",
                sd_cols      = "pH_stdev",
                comp_colours = "gold",
                comp_labels  = "pH",
                title        = "pH",
                y_label      = "pH",
                y_limits     = c(0, 7))
}

plot_avg_hplc <- function(df_list, exp_labels) {
  plot_averages(df_list, exp_labels,
                avg_cols     = c("Maltotriose_avg","Maltose_avg","Glucose_avg",
                                 "Fructose_avg","Glycerol_avg","Ethanol_avg"),
                sd_cols      = c("Maltotriose_stdev","Maltose_stdev","Glucose_stdev",
                                 "Fructose_stdev","Glycerol_stdev","Ethanol_stdev"),
                comp_colours = hplc_colours,
                comp_labels  = hplc_labels,
                title        = "Sugars & Ethanol",
                y_label      = "Concentration (g/L)")
}

plot_avg_diketones <- function(df_list, exp_labels) {
  plot_averages(df_list, exp_labels,
                avg_cols     = c("Diacetyl_avg","2,3-pentanedione_avg"),
                sd_cols      = c("Diacetyl_stdev","2,3-pentanedione_stdev"),
                comp_colours = gc_ketone_colours,
                comp_labels  = gc_ketone_labels,
                title        = "Vicinal Diketones",
                y_label      = "Concentration (mg/L)")
}

plot_avg_ethyl_esters_bar <- function(df_list, exp_labels) {
  plot_avg_stacked_bar(df_list, exp_labels,
                       avg_cols     = c("Ethyl_butyrate_avg_normalized","Ethyl_hexanoate_avg_normalized",
                                        "Ethyl_octanoate_avg_normalized","Ethyl_decanoate_avg_normalized"),
                       comp_colours = ethyl_ester_colours,
                       comp_labels  = ethyl_ester_labels,
                       title        = "Ethyl Esters",
                       y_label      = "Concentration (mg/L, normalised)")
}

plot_avg_acetate_esters_bar <- function(df_list, exp_labels) {
  plot_avg_stacked_bar(df_list, exp_labels,
                       avg_cols     = c("Ethyl_acetate_avg_normalized","Isobutyl_acetate_avg_normalized",
                                        "Isoamyl_acetate_avg_normalized"),
                       comp_colours = acetate_colours,
                       comp_labels  = acetate_labels,
                       title        = "Acetates",
                       y_label      = "Concentration (mg/L, normalised)")
}

plot_avg_higher_alcohols_bar <- function(df_list, exp_labels) {
  plot_avg_stacked_bar(df_list, exp_labels,
                       avg_cols     = c("Isobutanol_avg_normalized","Isoamyl_alcohol_avg_normalized"),
                       comp_colours = alcohol_colours,
                       comp_labels  = alcohol_labels,
                       title        = "Higher Alcohols",
                       y_label      = "Concentration (mg/L, normalised)")
}
