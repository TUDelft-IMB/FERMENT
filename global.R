# =============================================================================
# global.R
# Loaded once when the app starts, before ui.R and server.R are evaluated.
# Everything defined here is available to both ui.R and server.R.
#
# This file contains:
#   1. Library imports
#   2. Config (path to Excel folder)
#   3. Data-loading functions (workbook + averages sheet)
#   4. Colour palettes for all plot types
#   5. Plot functions — single experiment (return ggplot objects)
#   6. Plot functions — averages (return ggplot objects)
#
# All plot functions return plain ggplot objects. server.R wraps every one
# in ggplotly() so the charts are interactive in the browser. Keeping the
# plotting logic in ggplot2 here means the functions are easy to read,
# tweak, and test independently of Shiny.
#
# COLOUR CONVENTION:
# Section 4 is the single source of truth for all colours and labels.
# Every plot function builds its colour map from those vectors. To change
# a compound colour, edit section 4 only.
# =============================================================================


# =============================================================================
# 1. LIBRARIES
# =============================================================================
# shiny   - the web-app framework
# readxl  - reading .xlsx Excel files
# dplyr   - data manipulation (filter, mutate, select, etc.)
# tidyr   - reshaping data between wide and long formats
# ggplot2 - building plots (all plot functions return ggplot objects)
# janitor - row_to_names() for fixing double-header sheets
# purrr   - functional helpers (map, etc.)
# scales  - percent_format() used in the cone viability plot
# plotly  - ggplotly() is called in server.R to make every chart interactive;
#           loading it here ensures it is available at startup

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(purrr)
library(scales)
library(plotly)
library(bslib)
library(DT)
library(shinycssloaders)
library(styler)
library(bsicons)


# =============================================================================
# 2. CONFIG
# =============================================================================
# EXCEL_DIR is the folder that holds all .xlsx experiment files.
# Set the environment variable TT_EXCEL_DIR in .Renviron before running the app
# so that no file path is hard-coded in the source.

# If .Renviron cannot be set or does not work, set the path in line 70 below
# replace PATH/PATH with the path and save the changes
# DO NOT commit nor push the change so your path does not become public

# Initialize EXCEL_DIR from environment variable or fallback
# check for TT_EXCEL_DIR availability from .Renviron
if (Sys.getenv("TT_EXCEL_DIR") == "") {
  EXCEL_DIR <- Sys.getenv("TT_EXCEL_DIR", "PATH/PATH") # replace PATH/PATH inside quotation marks
} else {
  EXCEL_DIR <- Sys.getenv("TT_EXCEL_DIR")
}

# The fallback "PATH/PATH" will cause an informative error if the variable
# is not set, rather than rendering an empty dashboard later.
if (EXCEL_DIR == "PATH/PATH") { # Do not change this PATH/PATH
  stop("Environment variable TT_EXCEL_DIR is not set.")
}

# Caching paths

os_tag <- switch(
  Sys.info()[["sysname"]],
  "Darwin"  = "macos",
  "Windows" = "windows",
  "Linux"   = "linux",
  "unknown"
)

metadata_cache_path <- file.path(EXCEL_DIR, paste0(".metadata_cache_", os_tag, ".rds"))
# =============================================================================
# 3. DATA-LOADING FUNCTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# read_tt_workbook()
# Returns a lightweight "tt_workbook" handle for a given experiment file —
# it does NOT read any sheets immediately. Each sheet is only read (and
# cached) the first time it is accessed via `[[` or `$`, e.g. wb$HPLC or
# wb[["HPLC"]]. This avoids reading all 6 sheets when only 1-2 are needed.
#
# Sheet access is delegated to read_tt_sheet_cached(), which also applies
# the header-row fix for "Attenuation" and "pH" (their Excel layout has a
# second header row that readxl would otherwise treat as a data row).
#
# Returns: an object of class "tt_workbook" (path only; sheets load on demand).
# -----------------------------------------------------------------------------

# Lazy, cached version — returns a lightweight handle instead of reading
# all sheets immediately. Sheets are read (and cached) only on first access.
read_tt_workbook <- function(file_path) {
  structure(list(path = file_path), class = "tt_workbook")
}

`[[.tt_workbook` <- function(x, sheet_name) {
  path <- .subset2(x, "path")
  read_tt_sheet_cached(path, sheet_name)
}

`$.tt_workbook` <- function(x, name) {
  x[[name]]
}

# In-memory cache shared across all sessions (defined once in global.R).
# Keyed by file path + sheet name + modification time, so edited files
# automatically invalidate their cached sheet.
tt_sheet_cache <- new.env(parent = emptyenv())

read_tt_sheet_cached <- function(file_path, sheet_name) {
  mtime <- as.character(file.mtime(file_path))
  key <- paste(file_path, sheet_name, mtime, sep = "||")
  
  if (!is.null(tt_sheet_cache[[key]])) {
    return(tt_sheet_cache[[key]])
  }
  
  df <- tryCatch(
    read_excel(file_path, sheet = sheet_name),
    error = function(e) NULL
  )
  
  # Preserve the original header-row fix for these two sheets
  if (!is.null(df) && sheet_name %in% c("Attenuation", "pH")) {
    df <- df |>
      row_to_names(row_number = 1) |>
      type.convert(as.is = TRUE)
  }
  
  tt_sheet_cache[[key]] <- df
  df
}

# -----------------------------------------------------------------------------
# read_avg_sheet()
# Reads only the "averages_to_plot" sheet from a workbook.
# Returns NULL silently if the sheet is missing, so that server.R can drop
# files that are not yet complete without crashing.
# [TODO_LATER] - decide on the behaviour for this functionality!
# -----------------------------------------------------------------------------
read_avg_sheet <- function(file_path) {
  tryCatch(
    read_excel(file_path, sheet = "averages_to_plot", col_names = TRUE),
    error = function(e) NULL
  )
}


# =============================================================================
# 4. COLOUR PALETTES — single source of truth
# =============================================================================

# Okabe-Ito palette was constructed to have reasonable perceptual properties, 
# including accommodation for color vision deficiencies.
# See: https://journal.r-project.org/articles/RJ-2023-071/
#
# Okabe-Ito has 9 colours; for 10 GC esters we recycle and replace the 10th
# with a wine red instead of the recycled black.
okabe <- unname(palette.colors(9, palette = "Okabe-Ito"))
okabe10    <- unname(palette.colors(10, palette = "Okabe-Ito", recycle = TRUE))
okabe10[10] <- "#722F37"   # wine red

# --- HPLC: 6 metabolites -----------------------------------------------------
hplc_labels <- c(
  "Maltotriose", "Maltose", "Glucose",
  "Fructose", "Glycerol", "Ethanol"
)

hplc_avg_labels <- c(
  "Maltotriose", "Maltose", "Glucose",
  "Fructose", "Glycerol", "Ethanol"
)

hplc_colours <- okabe[c(2, 3, 4, 6, 8, 7)]

# --- GC Esters: 10 compounds -------------------------------------------------
gc_ester_labels <- c(
  "Ethyl acetate", "Ethanol", "Isobutyl acetate",
  "Ethyl butyrate", "Isobutanol", "Isoamyl acetate",
  "Isoamyl alcohol", "Ethyl hexanoate",
  "Ethyl octanoate", "Ethyl decanoate"
)

gc_ester_colours <- okabe10

# --- GC Ketones: 2 compounds -------------------------------------------------
gc_ketone_labels   <- c("Diacetyl", "2,3-Pentanedione")
gc_ketone_colours  <- okabe[c(3, 7)]

# --- TT1 vs TT2: Attenuation, Cell Count, Viability --------------------------
tt_colours <- setNames(okabe[c(3, 7)], c("TT1", "TT2"))

# --- TT1 vs TT2: pH ----------------------------------------------------------
ph_colours <- setNames(okabe[c(3, 7)], c("TT1", "TT2"))

# --- Averages: single-series line plots --------------------------------------
# One colour per metric; used by the averages wrappers that plot a single
# compound over time (attenuation, pH, cell count, viability).
att_colour        <- okabe[7]
avg_ph_colour     <- okabe[7]
cell_count_colour <- okabe[7]
viability_colour  <- okabe[7]

# --- Averages: line linetypes (one per selected experiment) ------------------
# Compounds are colour-coded; experiments are distinguished by linetype.
# Up to 6 experiments can be overlaid before linetypes repeat.
# [TODO_LATER] decide on behaviour when > 6 experiments are selected
exp_linetypes_palette <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")

# --- Averages: Ethyl esters stacked bar (4 compounds) -----------------------
ethyl_ester_labels <- c(
  "Ethyl butyrate", "Ethyl hexanoate",
  "Ethyl octanoate", "Ethyl decanoate"
)

ethyl_ester_colours <- okabe10[c(2, 3, 4, 10)]

# --- Averages: Acetate esters stacked bar (3 compounds) ---------------------
acetate_labels <- c("Ethyl acetate", "Isobutyl acetate", "Isoamyl acetate")
acetate_colours <- okabe[c(6, 5, 7)]

# --- Averages: Higher alcohols stacked bar (2 compounds) --------------------
alcohol_labels <- c("Isobutanol", "Isoamyl alcohol")
alcohol_colours <- okabe[c(3, 7)]

# --- Compare tab: experiment colours ----------------------------------------
# Start with the customized Okabe-Ito 10, then extend with Polychrome 36
# for larger multi-experiment overlays.
polychrome36 <- unname(palette.colors(36, palette = "Polychrome 36"))
# Remove any colours already present in okabe10, just in case of overlap
polychrome_extra <- polychrome36[!polychrome36 %in% okabe10]
# Used by attenuation, pH, cell count, and viability compare plots.
cmp_exp_colours <- c(okabe10, polychrome_extra)

# =============================================================================
# 5. PLOT FUNCTIONS — SINGLE EXPERIMENT
# =============================================================================
# Each function accepts one data frame (one sheet from a loaded workbook)
# and returns a ggplot object. server.R wraps the return value in ggplotly().
#
# HPLC, GC Esters, and GC Ketones share the same loop pattern:
#   1. Build a named colour map from the section 4 vectors using setNames().
#   2. Loop over compound labels; construct column names at runtime using
#      paste(tube_num, label) to select the correct TT1/TT2 columns.
#   3. Use .data[[col]] to reference string column names inside aes().
#   4. Use !! (bang-bang) to inject the label string as the colour aesthetic
#      value so ggplot registers it as a legend key.
#   5. Pass the named colour map to scale_color_manual() so ggplotly()
#      renders the correct label (not the raw colour string) in the legend.
#
# Attenuation, pH, Cell Count, and Viability are simpler (2 fixed columns).
# They use aes(colour = "TT1") / aes(colour = "TT2") with scale_colour_manual()
# and the named tt_colours / ph_colours vectors from section 4.

# -----------------------------------------------------------------------------
# plot_hplc_tube() — sugars and ethanol over fermentation time
# df       : the HPLC sheet data frame
# tube_num : 1 or 2 (selects the TT1 or TT2 columns)
# -----------------------------------------------------------------------------
plot_hplc_tube <- function(df, tube_num) {
  # Build named colour map from section 4 vectors.
  # Names = compound labels (used as colour aesthetic keys and legend text).
  # Values = colour strings (used by scale_color_manual).
  metabolites_map <- setNames(hplc_colours, hplc_labels)

  p <- ggplot(df, aes(x = `Time (h)`))

  for (base_metab in names(metabolites_map)) {
    # Construct full column names: e.g. "1 Maltotriose (g/L)", "StDev 1 Maltotriose (g/L)"
    val_col <- paste(tube_num, base_metab)
    stdev_col <- paste("StDev", val_col)

    p <- p +
      geom_line(aes(y = .data[[val_col]], colour = !!base_metab)) +
      geom_point(aes(y = .data[[val_col]], colour = !!base_metab)) +
      geom_errorbar(aes(
        ymin   = .data[[val_col]] - .data[[stdev_col]],
        ymax   = .data[[val_col]] + .data[[stdev_col]],
        colour = !!base_metab
      ), width = 3)
  }

  p +
    scale_color_manual(name = "Metabolites", values = metabolites_map) +
    labs(
      title = paste0("HPLC TT", tube_num),
      x     = "Time (h)",
      y     = "Concentration (g/L)"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_gc_esters_tube() — volatile esters over fermentation time
# df       : the GC_esters sheet data frame
# tube_num : 1 or 2
# -----------------------------------------------------------------------------
plot_gc_esters_tube <- function(df, tube_num) {
  metabolites_map <- setNames(gc_ester_colours, gc_ester_labels)

  p <- ggplot(df, aes(x = `Time (h)`))

  for (base_metab in names(metabolites_map)) {
    val_col <- paste(tube_num, base_metab)
    stdev_col <- paste("StDev", val_col)

    p <- p +
      geom_line(aes(y = .data[[val_col]], colour = !!base_metab)) +
      geom_point(aes(y = .data[[val_col]], colour = !!base_metab)) +
      geom_errorbar(aes(
        ymin   = .data[[val_col]] - .data[[stdev_col]],
        ymax   = .data[[val_col]] + .data[[stdev_col]],
        colour = !!base_metab
      ), width = 3)
  }

  p +
    scale_color_manual(name = "Metabolites", values = metabolites_map) +
    labs(
      title = paste0("GC Esters TT", tube_num),
      x     = "Time (h)",
      y     = "Concentration (mg/L)"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_gc_ketones_tube() — diacetyl and 2,3-pentanedione over time
# df       : the GC_ketones sheet data frame
# tube_num : 1 or 2
# -----------------------------------------------------------------------------
plot_gc_ketones_tube <- function(df, tube_num) {
  metabolites_map <- setNames(gc_ketone_colours, gc_ketone_labels)

  p <- ggplot(df, aes(x = `Time (h)`))

  for (base_metab in names(metabolites_map)) {
    val_col <- paste(tube_num, base_metab)
    stdev_col <- paste("StDev", val_col)

    p <- p +
      geom_line(aes(y = .data[[val_col]], colour = !!base_metab)) +
      geom_point(aes(y = .data[[val_col]], colour = !!base_metab)) +
      geom_errorbar(aes(
        ymin   = .data[[val_col]] - .data[[stdev_col]],
        ymax   = .data[[val_col]] + .data[[stdev_col]],
        colour = !!base_metab
      ), width = 3)
  }

  p +
    scale_color_manual(name = "Metabolites", values = metabolites_map) +
    labs(
      title = paste0("GC Ketones TT", tube_num),
      x     = "Time (h)",
      y     = "Concentration (mg/L)"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_att() — attenuation (degrees P) over time, TT1 vs TT2
# Uses tt_colours from section 4 via scale_colour_manual().
# -----------------------------------------------------------------------------
plot_att <- function(att) {
  ggplot(att) +
    geom_line(aes(x = `Time (h)`, y = `TT1`, colour = "TT1")) +
    geom_point(aes(x = `Time (h)`, y = `TT1`, colour = "TT1")) +
    geom_line(aes(x = `Time (h)`, y = `TT2`, colour = "TT2")) +
    geom_point(aes(x = `Time (h)`, y = `TT2`, colour = "TT2")) +
    scale_colour_manual(name = "Tube", values = tt_colours) +
    labs(title = "Attenuation", x = "Time (h)", y = "Attenuation (degrees P)") +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_ph() — pH over time, TT1 vs TT2
# Y-axis fixed at 0-7. Uses ph_colours (gold/sienna) from section 4.
# -----------------------------------------------------------------------------
plot_ph <- function(ph) {
  ggplot(ph) +
    geom_line(aes(x = `Time (h)`, y = `TT1`, colour = "TT1")) +
    geom_point(aes(x = `Time (h)`, y = `TT1`, colour = "TT1")) +
    geom_line(aes(x = `Time (h)`, y = `TT2`, colour = "TT2")) +
    geom_point(aes(x = `Time (h)`, y = `TT2`, colour = "TT2")) +
    scale_colour_manual(name = "Tube", values = ph_colours) +
    scale_y_continuous(limits = c(0, 7)) +
    labs(title = "pH", x = "Time (h)", y = "pH") +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_cell_count() — total cell count (cells/ml) over time, TT1 vs TT2
# Uses tt_colours from section 4 via scale_colour_manual().
# -----------------------------------------------------------------------------
plot_cell_count <- function(viability) {
  ggplot(viability) +
    geom_line(aes(x = `Time (h)`, y = `1 Total cells`, colour = "TT1")) +
    geom_point(aes(x = `Time (h)`, y = `1 Total cells`, colour = "TT1")) +
    geom_line(aes(x = `Time (h)`, y = `2 Total cells`, colour = "TT2")) +
    geom_point(aes(x = `Time (h)`, y = `2 Total cells`, colour = "TT2")) +
    scale_colour_manual(name = "Tube", values = tt_colours) +
    labs(title = "Cell Count", x = "Time (h)", y = "Cell count (cells/ml)") +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_viability() — cell viability (fraction 0-1) over time, TT1 vs TT2
# Y-axis fixed at 0-1. Uses tt_colours from section 4 via scale_colour_manual().
# -----------------------------------------------------------------------------
plot_viability <- function(viability) {
  ggplot(viability) +
    geom_line(aes(x = `Time (h)`, y = `1 Viability (%)`, colour = "TT1")) +
    geom_point(aes(x = `Time (h)`, y = `1 Viability (%)`, colour = "TT1")) +
    geom_line(aes(x = `Time (h)`, y = `2 Viability (%)`, colour = "TT2")) +
    geom_point(aes(x = `Time (h)`, y = `2 Viability (%)`, colour = "TT2")) +
    scale_colour_manual(name = "Tube", values = tt_colours) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Viability", x = "Time (h)", y = "Viability (fraction)") +
    theme_minimal() +
    theme(legend.position = "right")
}
# -----------------------------------------------------------------------------
# plot_CO2() — CO2 production (ml/min over time, TT1 vs TT2
# Uses tt_colours from section 4 via scale_colour_manual().
# --------------------------------------------------------------------------
plot_CO2 <- function(CO2) {
  CO2[["Time (h)"]] <- CO2[["Time (days)"]] * 24
  ggplot(CO2) +
    geom_line(aes(x = `Time (h)`, y = `TT1`, colour = "TT1")) +
    geom_line(aes(x = `Time (h)`, y = `TT2`, colour = "TT2")) +
    scale_colour_manual(name = "Tube", values = tt_colours) +
    labs(title = "CO\u2082 production", x = "Time (h)", y = "CO2 (ml/min)") +
    theme_minimal() +
    theme(legend.position = "right")
}

# =============================================================================
# 6. PLOT FUNCTIONS — AVERAGES
# =============================================================================
# Each function accepts:
#   df_list    : named list of data frames, one per selected experiment
#   exp_labels : character vector of "Experiment #N" labels, same length
#
# Line plots: colour = compound, linetype = experiment. Both scales use named
# vectors built from section 4 so ggplotly() renders correct legend labels.
#
# scale_colour_manual() and scale_linetype_manual()
# are used with named vectors built from section 4, so ggplotly() renders
# the correct label text in both legends.
# (scale_colour_identity / scale_linetype_identity with breaks/labels does
# not converse properly from ggplot -> plotly)
#
# Bar plots use facet_wrap(~ experiment) so each experiment gets its own
# panel; compounds are stacked within each panel.

# -----------------------------------------------------------------------------
# plot_averages() — generic line overlay helper used by HPLC, diketones,
#                   attenuation, pH, cell count, and viability averages.
#
# avg_cols    : character vector of column names in the averages sheet
# sd_cols     : matching SD column names (or NULL for no error bars)
# comp_colours: colour string per compound — from section 4 (same order as avg_cols)
# comp_labels : display label per compound — from section 4 (same order as avg_cols)
# title       : plot title
# y_label     : y-axis label
# y_limits    : optional c(min, max) to fix the y-axis range
# -----------------------------------------------------------------------------
plot_averages <- function(df_list, exp_labels,
                          avg_cols, sd_cols,
                          comp_colours, comp_labels,
                          title = "", y_label = "", y_limits = NULL) {
  # Named vectors for the two scales — names are display labels, values are
  # the visual encoding. Built from section 4 vectors passed as arguments.
  linetypes <- exp_linetypes_palette[seq_along(df_list)]
  colour_map <- setNames(comp_colours, comp_labels)
  linetype_map <- setNames(linetypes, exp_labels)

  # Stack all experiments and compounds into one long data frame.
  # 'compound' and 'experiment' columns hold display labels so they map
  # directly into aes(colour = compound, linetype = experiment).
  plot_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]]
    do.call(rbind, lapply(seq_along(avg_cols), function(i) {
      avg_col <- avg_cols[i]
      sd_col <- if (!is.null(sd_cols)) sd_cols[i] else NA_character_
      data.frame(
        time = df[["Time (h)"]],
        value = if (avg_col %in% names(df)) as.numeric(df[[avg_col]]) else NA_real_,
        sd = if (!is.na(sd_col) && sd_col %in% names(df)) as.numeric(df[[sd_col]]) else NA_real_,
        compound = comp_labels[i], # colour aesthetic key
        experiment = exp_labels[e], # linetype aesthetic key
        stringsAsFactors = FALSE
      )
    }))
  }))
  plot_data <- plot_data[!is.na(plot_data$value), ]

  p <- ggplot(
    plot_data,
    aes(
      x = time,
      y = value,
      colour = compound,
      linetype = experiment,
      group = interaction(compound, experiment)
    )
  ) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 3, na.rm = TRUE) +
    scale_colour_manual(name = "Compound", values = colour_map) +
    scale_linetype_manual(name = "Experiment", values = linetype_map) +
    labs(title = title, x = "Time (h)", y = y_label) +
    theme_minimal() +
    theme(legend.position = "right")

  if (!is.null(y_limits)) p <- p + scale_y_continuous(limits = y_limits)
  p
}

# -----------------------------------------------------------------------------
# plot_avg_by_experiment()
# Single-series averages plot, coloured by experiment (no compound/linetype
# split needed, since each experiment only has one averaged reading).
# Same colour encoding as the Compare tab (cmp_exp_colours).
# -----------------------------------------------------------------------------
plot_avg_by_experiment <- function(df_list, exp_labels,
                                   avg_col, sd_col,
                                   title = "", y_label = "", y_limits = NULL) {
  if (length(df_list) == 0 || length(exp_labels) == 0) {
    return(ggplot() + theme_minimal() + labs(title = title, x = "Time (h)", y = y_label))
  }
  
  colour_map <- setNames(cmp_exp_colours[seq_along(df_list)], exp_labels)
  
  plot_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]]
    data.frame(
      time = as.numeric(df[["Time (h)"]]),
      value = if (avg_col %in% names(df)) as.numeric(df[[avg_col]]) else NA_real_,
      sd = if (!is.null(sd_col) && sd_col %in% names(df)) as.numeric(df[[sd_col]]) else NA_real_,
      experiment = exp_labels[e],
      stringsAsFactors = FALSE
    )
  }))
  plot_data <- plot_data[!is.na(plot_data$value), ]
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + theme_minimal() + labs(title = title, x = "Time (h)", y = y_label))
  }
  
  p <- ggplot(
    plot_data,
    aes(x = time, y = value, colour = experiment, group = experiment)
  ) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 3, na.rm = TRUE) +
    scale_colour_manual(name = "Experiment", values = colour_map) +
    labs(title = title, x = "Time (h)", y = y_label) +
    theme_minimal() +
    theme(legend.position = "right")
  
  if (!is.null(y_limits)) p <- p + scale_y_continuous(limits = y_limits)
  p
}

# -----------------------------------------------------------------------------
# plot_avg_stacked_bar() — generic stacked bar helper used by GC esters and
#                          higher alcohols averages.
#
# Uses the last non-NA value of each compound column as the bar height
# (fermentation endpoint concentration).
# avg_cols, comp_colours, comp_labels all come from section 4 vectors passed
# by the specific wrapper functions below.
# -----------------------------------------------------------------------------
plot_avg_stacked_bar <- function(df_list, exp_labels,
                                 avg_cols, comp_colours, comp_labels,
                                 title = "", y_label = "") {
  bar_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]]
    do.call(rbind, lapply(seq_along(avg_cols), function(i) {
      vals <- as.numeric(df[[avg_cols[i]]])
      last_val <- if (any(!is.na(vals))) tail(vals[!is.na(vals)], 1) else NA_real_
      data.frame(
        experiment = exp_labels[e],
        compound = comp_labels[i],
        value = last_val,
        stringsAsFactors = FALSE
      )
    }))
  }))
  bar_data <- bar_data[!is.na(bar_data$value), ]
  bar_data$experiment <- factor(bar_data$experiment, levels = exp_labels)
  bar_data$compound <- factor(bar_data$compound, levels = comp_labels)

  ggplot(bar_data, aes(x = "", y = value, fill = compound)) +
    geom_col(position = "stack", width = 0.6) +
    facet_wrap(~experiment, nrow = 1) +
    scale_fill_manual(
      name   = "Compound",
      values = setNames(comp_colours, comp_labels)
    ) +
    labs(title = title, x = NULL, y = y_label) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x     = element_blank(),
      axis.ticks.x    = element_blank(),
      strip.text      = element_text(size = 10)
    )
}

# -----------------------------------------------------------------------------
# Specific averages plot functions.
# Each passes the relevant section 4 vectors to one of the two helpers above.
# -----------------------------------------------------------------------------

# Sugars & Ethanol: uses hplc_avg_labels (plain names) and hplc_colours
plot_avg_hplc <- function(df_list, exp_labels) {
  plot_averages(df_list, exp_labels,
    avg_cols = c(
      "Maltotriose_avg", "Maltose_avg", "Glucose_avg",
      "Fructose_avg", "Glycerol_avg", "Ethanol_avg"
    ),
    sd_cols = c(
      "Maltotriose_stdev", "Maltose_stdev", "Glucose_stdev",
      "Fructose_stdev", "Glycerol_stdev", "Ethanol_stdev"
    ),
    comp_colours = hplc_colours,
    comp_labels = hplc_avg_labels, # plain names (no unit suffix)
    title = "Sugars & Ethanol",
    y_label = "Concentration (g/L)"
  )
}

# Vicinal Diketones: uses gc_ketone_labels and gc_ketone_colours
plot_avg_diketones <- function(df_list, exp_labels) {
  plot_averages(df_list, exp_labels,
    avg_cols     = c("Diacetyl_avg", "2,3-pentanedione_avg"),
    sd_cols      = c("Diacetyl_stdev", "2,3-pentanedione_stdev"),
    comp_colours = gc_ketone_colours,
    comp_labels  = gc_ketone_labels,
    title        = "Vicinal Diketones",
    y_label      = "Concentration (mg/L)"
  )
}

# Attenuation over time
plot_avg_attenuation <- function(df_list, exp_labels) {
  plot_avg_by_experiment(
    df_list, exp_labels,
    avg_col = "Attenuation_average",
    sd_col = "Attenuation_stdev",
    title = "Attenuation",
    y_label = "Attenuation (degrees P)"
  )
}

# pH over time — y-axis fixed at 0-7
plot_avg_ph <- function(df_list, exp_labels) {
  plot_avg_by_experiment(
    df_list, exp_labels,
    avg_col = "pH_average",
    sd_col = "pH_stdev",
    title = "pH",
    y_label = "pH",
    y_limits = c(0, 7)
  )
}

# Cell Count over time
plot_avg_cell_count <- function(df_list, exp_labels) {
  plot_avg_by_experiment(
    df_list, exp_labels,
    avg_col = "CellCount_average",
    sd_col = "CellCount_stdev",
    title = "Cell Count",
    y_label = "Cell count (cells/ml)"
  )
}

# Viability over time — y-axis fixed at 0-1 (fraction)
plot_avg_viability <- function(df_list, exp_labels) {
  plot_avg_by_experiment(
    df_list, exp_labels,
    avg_col = "Viability_average",
    sd_col = "Viability_stdev",
    title = "Viability",
    y_label = "Viability (fraction)",
    y_limits = c(0, 1)
  )
}

# Ethyl esters stacked bar — uses ethyl_ester_labels and ethyl_ester_colours
plot_avg_ethyl_esters_bar <- function(df_list, exp_labels) {
  plot_avg_stacked_bar(df_list, exp_labels,
    avg_cols = c(
      "Ethyl_butyrate_avg_normalized", "Ethyl_hexanoate_avg_normalized",
      "Ethyl_octanoate_avg_normalized", "Ethyl_decanoate_avg_normalized"
    ),
    comp_colours = ethyl_ester_colours,
    comp_labels = ethyl_ester_labels,
    title = "Ethyl Esters",
    y_label = "Concentration (mg/L, normalised)"
  )
}

# Acetate esters stacked bar — uses acetate_labels and acetate_colours
plot_avg_acetate_esters_bar <- function(df_list, exp_labels) {
  plot_avg_stacked_bar(df_list, exp_labels,
    avg_cols = c(
      "Ethyl_acetate_avg_normalized", "Isobutyl_acetate_avg_normalized",
      "Isoamyl_acetate_avg_normalized"
    ),
    comp_colours = acetate_colours,
    comp_labels = acetate_labels,
    title = "Acetates",
    y_label = "Concentration (mg/L, normalised)"
  )
}

# Higher alcohols stacked bar — uses alcohol_labels and alcohol_colours
plot_avg_higher_alcohols_bar <- function(df_list, exp_labels) {
  plot_avg_stacked_bar(df_list, exp_labels,
    avg_cols     = c("Isobutanol_avg_normalized", "Isoamyl_alcohol_avg_normalized"),
    comp_colours = alcohol_colours,
    comp_labels  = alcohol_labels,
    title        = "Higher Alcohols",
    y_label      = "Concentration (mg/L, normalised)"
  )
}

# -----------------------------------------------------------------------------
# plot_avg_cone_viability()
# Single-value bar chart: one bar per experiment showing cone viability at
# pitching, with error bars. Reads row 1 of the averages sheet only (the
# cone viability value is a single measurement, not a time series).
# Y-axis uses percent_format() from the scales package.
# -----------------------------------------------------------------------------
plot_avg_cone_viability <- function(df_list, exp_labels) {
  bar_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]]
    data.frame(
      experiment = exp_labels[e],
      value = as.numeric(df[["Cone_viability_average"]][1]),
      sd = as.numeric(df[["Stdev_cone_viability"]][1]),
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
    theme_minimal() +
    theme(legend.position = "none")
}

# Ethyl Acetate / Isoamyl Acetate ratio — one bar per experiment (final value)
plot_avg_gc_ratio_bar <- function(df_list, exp_labels) {
  bar_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]]
    vals <- as.numeric(df[["Ratio_Ethylacetate_isoamyl_acetate"]])
    last_val <- if (any(!is.na(vals))) tail(vals[!is.na(vals)], 1) else NA_real_
    data.frame(
      experiment = exp_labels[e],
      value = last_val,
      stringsAsFactors = FALSE
    )
  }))
  bar_data <- bar_data[!is.na(bar_data$value), ]
  bar_data$experiment <- factor(bar_data$experiment, levels = exp_labels)

  ggplot(bar_data, aes(x = experiment, y = value, fill = experiment)) +
    geom_col(width = 0.6) +
    scale_fill_manual(values = setNames(cmp_exp_colours[seq_along(exp_labels)], exp_labels)) +
    labs(title = "GC Ratio (Ethyl Acetate / Isoamyl Acetate)", x = "Experiment", y = "Ratio") +
    theme_minimal() +
    theme(legend.position = "none")
}

# =============================================================================
# 7. PLOT FUNCTIONS — COMPARE (multiple raw experiments overlaid)
# =============================================================================
# Each function accepts a named list of loaded workbooks (raw sheets, not
# averages) and a matching character vector of experiment labels, and returns
# a ggplot object. server.R wraps the return value in ggplotly().

# The visual encoding:
# HPLC / GC Esters / GC Ketones:
#   colour   = compound  (from section 4 palettes)
#   linetype = experiment (from exp_linetypes_palette)
#   Both TT1 and TT2 appear on separate side-by-side charts, mirroring the
#   Single Experiment layout.

# Attenuation / pH / Cell Count / Viability:
#   ONE chart per metric — both TT1 and TT2 are plotted together so the
#   user can see within-experiment tube agreement alongside cross-experiment
#   differences.
#   colour   = experiment (from cmp_exp_colours, colourblind-safe)
#   linetype = tube       (solid = TT1, dashed = TT2)
#   Legend key = "Experiment #N TT1" / "Experiment #N TT2"

# -----------------------------------------------------------------------------
# Helper plot_tube_overlay() — a helper function for plotting/overlaying
# multiple TT1/TT2 experiments for HPLC, GC esters and GC ketones
# -----------------------------------------------------------------------------
plot_cmp_tube_overlay <- function(df_list, exp_labels,
                                  sheet_name,
                                  compound_labels,
                                  compound_colours,
                                  title = "",
                                  y_label = "") {
  metabolites_map <- setNames(compound_colours, compound_labels)

  plot_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]][[sheet_name]]

    do.call(rbind, lapply(compound_labels, function(base_metab) {
      do.call(rbind, lapply(c(1, 2), function(tube_num) {
        val_col <- paste(tube_num, base_metab)
        stdev_col <- paste("StDev", val_col)

        data.frame(
          time = df[["Time (h)"]],
          value = if (val_col %in% names(df)) as.numeric(df[[val_col]]) else NA_real_,
          sd = if (stdev_col %in% names(df)) as.numeric(df[[stdev_col]]) else NA_real_,
          compound = base_metab,
          experiment = exp_labels[e],
          tube = paste0("TT", tube_num),
          exp_tube = paste0(exp_labels[e], " ", paste0("TT", tube_num)),
          trace_id = paste(exp_labels[e], paste0("TT", tube_num), base_metab),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }))

  plot_data <- plot_data[!is.na(plot_data$value), ]

  exp_tube_levels <- as.vector(t(outer(exp_labels, c("TT1", "TT2"), paste)))
  plot_data$exp_tube <- factor(plot_data$exp_tube, levels = exp_tube_levels)

  linetype_values <- rep(exp_linetypes_palette[seq_along(exp_labels)], each = 2)
  names(linetype_values) <- exp_tube_levels

  ggplot(
    plot_data,
    aes(
      x = time,
      y = value,
      colour = compound,
      linetype = exp_tube,
      group = trace_id
    )
  ) +
    geom_line() +
    geom_errorbar(
      aes(ymin = value - sd, ymax = value + sd),
      width = 3,
      na.rm = TRUE
    ) +
    geom_point(
      data = subset(plot_data, tube == "TT1"),
      shape = 16,
      size = 2,
      show.legend = FALSE
    ) +
    geom_point(
      data = subset(plot_data, tube == "TT2"),
      shape = 1,
      size = 2,
      show.legend = FALSE
    ) +
    scale_colour_manual(name = "Compound", values = metabolites_map) +
    scale_linetype_manual(name = "Experiment", values = linetype_values) +
    labs(title = title, x = "Time (h)", y = y_label) +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_cmp_hplc() — HPLC, TT1 and TT2 overlaid in one chart
# -----------------------------------------------------------------------------
plot_cmp_hplc <- function(df_list, exp_labels) {
  plot_cmp_tube_overlay(
    df_list = df_list,
    exp_labels = exp_labels,
    sheet_name = "HPLC",
    compound_labels = hplc_labels,
    compound_colours = hplc_colours,
    title = "HPLC",
    y_label = "Concentration (g/L)"
  )
}

# -----------------------------------------------------------------------------
# plot_cmp_gc_esters() — GC Esters, TT1 and TT2 overlaid in one chart
# -----------------------------------------------------------------------------
plot_cmp_gc_esters <- function(df_list, exp_labels) {
  plot_cmp_tube_overlay(
    df_list = df_list,
    exp_labels = exp_labels,
    sheet_name = "GC_esters",
    compound_labels = gc_ester_labels,
    compound_colours = gc_ester_colours,
    title = "GC Esters",
    y_label = "Concentration (mg/L)"
  )
}

# -----------------------------------------------------------------------------
# plot_cmp_gc_ketones() — GC Ketones, TT1 and TT2 overlaid in one chart
# -----------------------------------------------------------------------------
plot_cmp_gc_ketones <- function(df_list, exp_labels) {
  plot_cmp_tube_overlay(
    df_list = df_list,
    exp_labels = exp_labels,
    sheet_name = "GC_ketones",
    compound_labels = gc_ketone_labels,
    compound_colours = gc_ketone_colours,
    title = "GC Ketones",
    y_label = "Concentration (mg/L)"
  )
}

# -----------------------------------------------------------------------------
# plot_cmp_att()
# Attenuation over time — BOTH tubes for ALL experiments in one chart.
# colour   = experiment (cmp_exp_colours)
# linetype = tube       (solid = TT1, dashed = TT2)
# Legend labels: "Experiment #N TT1", "Experiment #N TT2"
# -----------------------------------------------------------------------------
plot_cmp_att <- function(df_list, exp_labels) {
  tube_cols <- c("TT1", "TT2")
  tube_lty <- c("TT1" = "solid", "TT2" = "dashed")
  colour_map <- setNames(cmp_exp_colours[seq_along(df_list)], exp_labels)

  plot_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]][["Attenuation"]]
    do.call(rbind, lapply(tube_cols, function(tc) {
      data.frame(
        time = as.numeric(df[["Time (h)"]]),
        value = if (tc %in% names(df)) as.numeric(df[[tc]]) else NA_real_,
        experiment = exp_labels[e],
        tube = tc,
        # trace_id used for the group aesthetic so lines don't cross tubes
        trace_id = paste(exp_labels[e], tc),
        stringsAsFactors = FALSE
      )
    }))
  }))
  plot_data <- plot_data[!is.na(plot_data$value), ]

  ggplot(
    plot_data,
    aes(
      x = time, y = value,
      colour = experiment,
      linetype = tube,
      group = trace_id
    )
  ) +
    geom_line() +
    geom_point() +
    scale_colour_manual(name = "Experiment", values = colour_map) +
    scale_linetype_manual(name = "TT", values = tube_lty) +
    labs(title = "Attenuation", x = "Time (h)", y = "Attenuation (degrees P)") +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_cmp_ph()
# Y-axis fixed at 0-7. Same encoding as plot_cmp_att().
# -----------------------------------------------------------------------------
plot_cmp_ph <- function(df_list, exp_labels) {
  tube_cols <- c("TT1", "TT2")
  tube_lty <- c("TT1" = "solid", "TT2" = "dashed")
  colour_map <- setNames(cmp_exp_colours[seq_along(df_list)], exp_labels)

  plot_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]][["pH"]]
    do.call(rbind, lapply(tube_cols, function(tc) {
      data.frame(
        time = as.numeric(df[["Time (h)"]]),
        value = if (tc %in% names(df)) as.numeric(df[[tc]]) else NA_real_,
        experiment = exp_labels[e],
        tube = tc,
        trace_id = paste(exp_labels[e], tc),
        stringsAsFactors = FALSE
      )
    }))
  }))
  plot_data <- plot_data[!is.na(plot_data$value), ]

  ggplot(
    plot_data,
    aes(
      x = time, y = value,
      colour = experiment,
      linetype = tube,
      group = trace_id
    )
  ) +
    geom_line() +
    geom_point() +
    scale_colour_manual(name = "Experiment", values = colour_map) +
    scale_linetype_manual(name = "TT", values = tube_lty) +
    scale_y_continuous(limits = c(0, 7)) +
    labs(title = "pH", x = "Time (h)", y = "pH") +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_cmp_cell_count()
# Total cell count over time — BOTH tubes for ALL experiments in one chart.
# tube_col_tt1 / tube_col_tt2: column names in the CellCount_Viability sheet.
# -----------------------------------------------------------------------------
plot_cmp_cell_count <- function(df_list, exp_labels) {
  tube_cols <- c("TT1" = "1 Total cells", "TT2" = "2 Total cells")
  tube_lty <- c("TT1" = "solid", "TT2" = "dashed")
  colour_map <- setNames(cmp_exp_colours[seq_along(df_list)], exp_labels)

  plot_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]][["CellCount_Viability"]]
    do.call(rbind, lapply(names(tube_cols), function(tube_label) {
      col <- tube_cols[[tube_label]]
      data.frame(
        time = as.numeric(df[["Time (h)"]]),
        value = if (col %in% names(df)) as.numeric(df[[col]]) else NA_real_,
        experiment = exp_labels[e],
        tube = tube_label,
        trace_id = paste(exp_labels[e], tube_label),
        stringsAsFactors = FALSE
      )
    }))
  }))
  plot_data <- plot_data[!is.na(plot_data$value), ]

  ggplot(
    plot_data,
    aes(
      x = time, y = value,
      colour = experiment,
      linetype = tube,
      group = trace_id
    )
  ) +
    geom_line() +
    geom_point() +
    scale_colour_manual(name = "Experiment", values = colour_map) +
    scale_linetype_manual(name = "TT", values = tube_lty) +
    labs(title = "Cell Count", x = "Time (h)", y = "Cell count (cells/ml)") +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_cmp_viability()
# Y-axis fixed at 0-1.
# -----------------------------------------------------------------------------
plot_cmp_viability <- function(df_list, exp_labels) {
  tube_cols <- c("TT1" = "1 Viability (%)", "TT2" = "2 Viability (%)")
  tube_lty <- c("TT1" = "solid", "TT2" = "dashed")
  colour_map <- setNames(cmp_exp_colours[seq_along(df_list)], exp_labels)

  plot_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]][["CellCount_Viability"]]
    
    do.call(rbind, lapply(names(tube_cols), function(tube_label) {
      col <- tube_cols[[tube_label]]
      data.frame(
        time = as.numeric(df[["Time (h)"]]),
        value = if (col %in% names(df)) as.numeric(df[[col]]) else NA_real_,
        experiment = exp_labels[e],
        tube = tube_label,
        trace_id = paste(exp_labels[e], tube_label),
        stringsAsFactors = FALSE
      )
    }))
  }))
  plot_data <- plot_data[!is.na(plot_data$value), ]

  ggplot(
    plot_data,
    aes(
      x = time, y = value,
      colour = experiment,
      linetype = tube,
      group = trace_id
    )
  ) +
    geom_line() +
    geom_point() +
    scale_colour_manual(name = "Experiment", values = colour_map) +
    scale_linetype_manual(name = "TT", values = tube_lty) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Viability", x = "Time (h)", y = "Viability (fraction)") +
    theme_minimal() +
    theme(legend.position = "right")
}

# -----------------------------------------------------------------------------
# plot_cmp_CO2() — CO2 production over time, all experiments overlaid.
# colour = experiment, linetype = tube (solid = TT1, dashed = TT2).
# Experiments whose workbook has no CO2 sheet (or an empty one) are silently
# skipped instead of breaking the whole plot.
# -----------------------------------------------------------------------------
plot_cmp_CO2 <- function(df_list, exp_labels) {
  tube_cols <- c("TT1", "TT2")
  tube_lty <- c("TT1" = "solid", "TT2" = "solid")
  colour_map <- setNames(cmp_exp_colours[seq_along(df_list)], exp_labels)
  
  plot_data <- do.call(rbind, lapply(seq_along(df_list), function(e) {
    df <- df_list[[e]][["CO2"]]
    
    # Guard: skip this experiment if the sheet is missing, empty, or
    # missing the time column — returning NULL makes do.call(rbind, ...)
    # drop it cleanly instead of erroring on mismatched row counts.
    if (is.null(df) || nrow(df) == 0 || !("Time (days)" %in% names(df))) {
      return(NULL)
    }
    
    time_h <- as.numeric(df[["Time (days)"]]) * 24
    
    do.call(rbind, lapply(tube_cols, function(tc) {
      data.frame(
        time = time_h,
        value = if (tc %in% names(df)) as.numeric(df[[tc]]) else rep(NA_real_, length(time_h)),
        experiment = exp_labels[e],
        tube = tc,
        # trace_id used for the group aesthetic so lines don't cross tubes
        trace_id = paste(exp_labels[e], tc),
        stringsAsFactors = FALSE
      )
    }))
  }))
  
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(
      ggplot() + theme_minimal() +
        labs(title = "CO\u2082 production", x = "Time (h)", y = "CO2 (ml/min)")
    )
  }
  
  plot_data <- plot_data[!is.na(plot_data$value), ]
  
  ggplot(
    plot_data,
    aes(
      x = time, y = value,
      colour = experiment,
      linetype = tube,
      group = trace_id
    )
  ) +
    geom_line() +
#    geom_point() +
    scale_colour_manual(name = "Experiment", values = colour_map) +
    scale_linetype_manual(name = "TT", values = tube_lty) +
    labs(title = "CO\u2082 production", x = "Time (h)", y = "CO\u2082 production (ml/min)") +
    theme_minimal() +
    theme(legend.position = "right")
}