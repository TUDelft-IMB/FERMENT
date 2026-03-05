# Libraries ---------------------------------------------------------------

library(tidyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)

file_path <- "..\\excel_files\\TT_Template_example_1_102.xlsx"
sheets <- excel_sheets(file_path)
data_list <- lapply(sheets, function(x) read_excel(file_path, sheet = x))
names(data_list) <- sheets
View(data_list)

# Remove junk rows
data_list[["Attenuation"]] <- data_list[["Attenuation"]] %>%
  row_to_names(row_number = 1) # Promotes the first row to header
data_list[["Attenuation"]] <- type.convert(data_list[["Attenuation"]], as.is = TRUE)

data_list[["pH"]] <- data_list[["pH"]] %>%
  row_to_names(row_number = 1) # Promotes the first row to header
data_list[["pH"]] <- type.convert(data_list[["pH"]], as.is = TRUE)


View(data_list[["Notes"]])
View(data_list[["Experimental_parameters"]])
View(data_list[["Planning_preculture"]])
View(data_list[["CellCount_Viability"]])
View(data_list[["Cone_viability"]])
View(data_list[["Attenuation"]])
View(data_list[["pH"]])
View(data_list[["HPLC_raw"]])
View(data_list[["HPLC"]])
View(data_list[["GC"]])




# Plots -------------------------------------------------------------------
# HPLC TT1

df <- data_list[["HPLC"]]

ggplot(df) +
  geom_line(aes(x = `Time (h)`, y = `1 Maltotriose (g/L)`, colour = "skyblue")) +
  geom_point(aes(x = `Time (h)`, y = `1 Maltotriose (g/L)`, colour = "skyblue")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Maltotriose (g/L)` - `StDev 1 Maltotriose (g/L)`, ymax = `1 Maltotriose (g/L)` + `StDev 1 Maltotriose (g/L)`,
                width = 3, colour = "skyblue")) + 
  geom_line(aes(x = `Time (h)`, y = `1 Maltose (g/L)`, colour = "maroon")) +
  geom_point(aes(x = `Time (h)`, y = `1 Maltose (g/L)`, colour = "maroon")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Maltose (g/L)` - `StDev 1 Maltose (g/L)`, ymax = `1 Maltose (g/L)` + `StDev 1 Maltose (g/L)`, 
                width = 3, colour = "maroon")) +
  geom_line(aes(x = `Time (h)`, y = `1 Glucose (g/L)`, colour = "gold")) +
  geom_point(aes(x = `Time (h)`, y = `1 Glucose (g/L)`, colour = "gold")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Glucose (g/L)` - `StDev 1 Glucose (g/L)`, ymax = `1 Glucose (g/L)` + `StDev 1 Glucose (g/L)`,
                width = 3, colour = "gold")) + 
  geom_line(aes(x = `Time (h)`, y = `1 Fructose (g/L)`, colour = "forestgreen")) +
  geom_point(aes(x = `Time (h)`, y = `1 Fructose (g/L)`, colour = "forestgreen")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Fructose (g/L)` - `StDev 1 Fructose (g/L)`, ymax = `1 Fructose (g/L)` + `StDev 1 Fructose (g/L)`,
                width = 3, colour = "forestgreen")) + 
  geom_line(aes(x = `Time (h)`, y = `1 Glycerol (g/L)`, colour = "grey")) +
  geom_point(aes(x = `Time (h)`, y = `1 Glycerol (g/L)`, colour = "grey")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Glycerol (g/L)` - `StDev 1 Glycerol (g/L)`, ymax = `1 Glycerol (g/L)` + `StDev 1 Glycerol (g/L)`,
                width = 3, colour = "grey")) + 
  geom_line(aes(x = `Time (h)`, y = `1 Ethanol (g/L)`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `1 Ethanol (g/L)`, colour = "sienna")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethanol (g/L)` - `StDev 1 Ethanol (g/L)`, ymax = `1 Ethanol (g/L)` + `StDev 1 Ethanol (g/L)`,
                width = 3, colour = "sienna")) + 
  scale_colour_identity(name = "Metabolites",
                        guide = "legend",
                        breaks = c("skyblue",
                                   "maroon",
                                   "gold",
                                   "forestgreen",
                                   "grey",
                                   "sienna"),
                        labels = c("Maltotriose",
                                   "Maltose",
                                   "Glucose",
                                   "Fructose",
                                   "Glycerol",
                                   "Ethanol")) +
  labs(x = "Time (h)", y = "Concentration (g/L)") +
  theme(legend.position = "right") +
  theme_minimal()
  
# HPLC TT2

ggplot(df) +
  geom_line(aes(x = `Time (h)`, y = `2 Maltotriose (g/L)`, colour = "skyblue")) +
  geom_point(aes(x = `Time (h)`, y = `2 Maltotriose (g/L)`, colour = "skyblue")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Maltotriose (g/L)` - `StDev 2 Maltotriose (g/L)`, ymax = `2 Maltotriose (g/L)` + `StDev 2 Maltotriose (g/L)`,
                    width = 3, colour = "skyblue")) + 
  geom_line(aes(x = `Time (h)`, y = `2 Maltose (g/L)`, colour = "maroon")) +
  geom_point(aes(x = `Time (h)`, y = `2 Maltose (g/L)`, colour = "maroon")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Maltose (g/L)` - `StDev 2 Maltose (g/L)`, ymax = `2 Maltose (g/L)` + `StDev 2 Maltose (g/L)`, 
                    width = 3, colour = "maroon")) +
  geom_line(aes(x = `Time (h)`, y = `2 Glucose (g/L)`, colour = "gold")) +
  geom_point(aes(x = `Time (h)`, y = `2 Glucose (g/L)`, colour = "gold")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Glucose (g/L)` - `StDev 2 Glucose (g/L)`, ymax = `2 Glucose (g/L)` + `StDev 2 Glucose (g/L)`,
                    width = 3, colour = "gold")) + 
  geom_line(aes(x = `Time (h)`, y = `2 Fructose (g/L)`, colour = "forestgreen")) +
  geom_point(aes(x = `Time (h)`, y = `2 Fructose (g/L)`, colour = "forestgreen")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Fructose (g/L)` - `StDev 2 Fructose (g/L)`, ymax = `2 Fructose (g/L)` + `StDev 2 Fructose (g/L)`,
                    width = 3, colour = "forestgreen")) + 
  geom_line(aes(x = `Time (h)`, y = `2 Glycerol (g/L)`, colour = "grey")) +
  geom_point(aes(x = `Time (h)`, y = `2 Glycerol (g/L)`, colour = "grey")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Glycerol (g/L)` - `StDev 2 Glycerol (g/L)`, ymax = `2 Glycerol (g/L)` + `StDev 2 Glycerol (g/L)`,
                    width = 3, colour = "grey")) + 
  geom_line(aes(x = `Time (h)`, y = `2 Ethanol (g/L)`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `2 Ethanol (g/L)`, colour = "sienna")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethanol (g/L)` - `StDev 2 Ethanol (g/L)`, ymax = `2 Ethanol (g/L)` + `StDev 2 Ethanol (g/L)`,
                    width = 3, colour = "sienna")) + 
  scale_colour_identity(name = "Metabolites",
                        guide = "legend",
                        breaks = c("skyblue",
                                   "maroon",
                                   "gold",
                                   "forestgreen",
                                   "grey",
                                   "sienna"),
                        labels = c("Maltotriose",
                                   "Maltose",
                                   "Glucose",
                                   "Fructose",
                                   "Glycerol",
                                   "Ethanol")) +
  labs(x = "Time (h)", y = "Concentration (g/L)") +
  theme(legend.position = "right") +
  theme_minimal()

# GC_esters
## TT1

gc <- data_list[["GC_esters"]]

ggplot(gc) +
  geom_line(aes(x = `Time (h)`, y = `1 Ethyl acetate`, colour = "skyblue")) +
  geom_point(aes(x = `Time (h)`, y = `1 Ethyl acetate`, colour = "skyblue")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl acetate` - `StDev 1 Ethyl acetate`, ymax = `1 Ethyl acetate` + `StDev 1 Ethyl acetate`,
                    width = 3, colour = "skyblue")) + 
  geom_line(aes(x = `Time (h)`, y = `1 Ethanol`, colour = "maroon")) +
  geom_point(aes(x = `Time (h)`, y = `1 Ethanol`, colour = "maroon")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethanol` - `StDev 1 Ethanol`, ymax = `1 Ethanol` + `StDev 1 Ethanol`, 
                    width = 3, colour = "maroon")) +
  geom_line(aes(x = `Time (h)`, y = `1 Isobutyl acetate`, colour = "gold")) +
  geom_point(aes(x = `Time (h)`, y = `1 Isobutyl acetate`, colour = "gold")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Isobutyl acetate` - `StDev 1 Isobutyl acetate`, ymax = `1 Isobutyl acetate` + `StDev 1 Isobutyl acetate`,
                    width = 3, colour = "gold")) + 
  geom_line(aes(x = `Time (h)`, y = `1 Ethyl butyrate`, colour = "forestgreen")) +
  geom_point(aes(x = `Time (h)`, y = `1 Ethyl butyrate`, colour = "forestgreen")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl butyrate` - `StDev 1 Ethyl butyrate`, ymax = `1 Ethyl butyrate` + `StDev 1 Ethyl butyrate`,
                    width = 3, colour = "forestgreen")) + 
  geom_line(aes(x = `Time (h)`, y = `1 Isobutanol`, colour = "grey")) +
  geom_point(aes(x = `Time (h)`, y = `1 Isobutanol`, colour = "grey")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Isobutanol` - `StDev 1 Isobutanol`, ymax = `1 Isobutanol` + `StDev 1 Isobutanol`,
                    width = 3, colour = "grey")) +
  geom_line(aes(x = `Time (h)`, y = `1 Isoamyl acetate`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `1 Isoamyl acetate`, colour = "sienna")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Isoamyl acetate` - `StDev 1 Isoamyl acetate`, ymax = `1 Isoamyl acetate` + `StDev 1 Isoamyl acetate`,
                    width = 3, colour = "sienna")) + 
  geom_line(aes(x = `Time (h)`, y = `1 Isoamyl alcohol`, colour = "darkred")) +
  geom_point(aes(x = `Time (h)`, y = `1 Isoamyl alcohol`, colour = "darkred")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Isoamyl alcohol` - `StDev 1 Isoamyl alcohol`, ymax = `1 Isoamyl alcohol` + `StDev 1 Isoamyl alcohol`,
                    width = 3, colour = "darkred")) +
  geom_line(aes(x = `Time (h)`, y = `1 Ethyl hexanoate`, colour = "darkgreen")) +
  geom_point(aes(x = `Time (h)`, y = `1 Ethyl hexanoate`, colour = "darkgreen")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl hexanoate` - `StDev 1 Ethyl hexanoate`, ymax = `1 Ethyl hexanoate` + `StDev 1 Ethyl hexanoate`,
                    width = 3, colour = "darkgreen")) + 
  geom_line(aes(x = `Time (h)`, y = `1 Ethyl octanoate`, colour = "lavender")) +
  geom_point(aes(x = `Time (h)`, y = `1 Ethyl octanoate`, colour = "lavender")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl octanoate` - `StDev 1 Ethyl octanoate`, ymax = `1 Ethyl octanoate` + `StDev 1 Ethyl octanoate`,
                    width = 3, colour = "lavender")) +
  geom_line(aes(x = `Time (h)`, y = `1 Ethyl decanoate`, colour = "darkblue")) +
  geom_point(aes(x = `Time (h)`, y = `1 Ethyl decanoate`, colour = "darkblue")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Ethyl decanoate` - `StDev 1 Ethyl decanoate`, ymax = `1 Ethyl decanoate` + `StDev 1 Ethyl decanoate`,
                    width = 3, colour = "darkblue")) +
  scale_colour_identity(name = "Metabolites",
                        guide = "legend",
                        breaks = c("skyblue",
                                   "maroon",
                                   "gold",
                                   "forestgreen",
                                   "grey",
                                   "sienna",
                                   "darkred",
                                   "darkgreen",
                                   "lavender",
                                   "darkblue"),
                        labels = c("Ethyl acetate",
                                   "Ethanol",
                                   "Isobutyl acetate",
                                   "Ethyl butyrate",
                                   "Isobutanol",
                                   "Isoamyl acetate",
                                   "Isoamyl alcohol",
                                   "Ethyl hexanoate",
                                   "Ethyl octanoate",
                                   "Ethyl decanoate")) +
  labs(x = "Time (h)", y = "Concentration (mg/L)") +
  theme(legend.position = "right") +
  theme_minimal()

# GC_esters 
## TT2

gc <- data_list[["GC_esters"]]

ggplot(gc) +
  geom_line(aes(x = `Time (h)`, y = `2 Ethyl acetate`, colour = "skyblue")) +
  geom_point(aes(x = `Time (h)`, y = `2 Ethyl acetate`, colour = "skyblue")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl acetate` - `StDev 2 Ethyl acetate`, ymax = `2 Ethyl acetate` + `StDev 2 Ethyl acetate`,
                    width = 3, colour = "skyblue")) + 
  geom_line(aes(x = `Time (h)`, y = `2 Ethanol`, colour = "maroon")) +
  geom_point(aes(x = `Time (h)`, y = `2 Ethanol`, colour = "maroon")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethanol` - `StDev 2 Ethanol`, ymax = `2 Ethanol` + `StDev 2 Ethanol`, 
                    width = 3, colour = "maroon")) +
  geom_line(aes(x = `Time (h)`, y = `2 Isobutyl acetate`, colour = "gold")) +
  geom_point(aes(x = `Time (h)`, y = `2 Isobutyl acetate`, colour = "gold")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Isobutyl acetate` - `StDev 2 Isobutyl acetate`, ymax = `2 Isobutyl acetate` + `StDev 2 Isobutyl acetate`,
                    width = 3, colour = "gold")) + 
  geom_line(aes(x = `Time (h)`, y = `2 Ethyl butyrate`, colour = "forestgreen")) +
  geom_point(aes(x = `Time (h)`, y = `2 Ethyl butyrate`, colour = "forestgreen")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl butyrate` - `StDev 2 Ethyl butyrate`, ymax = `2 Ethyl butyrate` + `StDev 2 Ethyl butyrate`,
                    width = 3, colour = "forestgreen")) + 
  geom_line(aes(x = `Time (h)`, y = `2 Isobutanol`, colour = "grey")) +
  geom_point(aes(x = `Time (h)`, y = `2 Isobutanol`, colour = "grey")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Isobutanol` - `StDev 2 Isobutanol`, ymax = `2 Isobutanol` + `StDev 2 Isobutanol`,
                    width = 3, colour = "grey")) +
  geom_line(aes(x = `Time (h)`, y = `2 Isoamyl acetate`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `2 Isoamyl acetate`, colour = "sienna")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Isoamyl acetate` - `StDev 2 Isoamyl acetate`, ymax = `2 Isoamyl acetate` + `StDev 2 Isoamyl acetate`,
                    width = 3, colour = "sienna")) + 
  geom_line(aes(x = `Time (h)`, y = `2 Isoamyl alcohol`, colour = "darkred")) +
  geom_point(aes(x = `Time (h)`, y = `2 Isoamyl alcohol`, colour = "darkred")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Isoamyl alcohol` - `StDev 2 Isoamyl alcohol`, ymax = `2 Isoamyl alcohol` + `StDev 2 Isoamyl alcohol`,
                    width = 3, colour = "darkred")) +
  geom_line(aes(x = `Time (h)`, y = `2 Ethyl hexanoate`, colour = "darkgreen")) +
  geom_point(aes(x = `Time (h)`, y = `2 Ethyl hexanoate`, colour = "darkgreen")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl hexanoate` - `StDev 2 Ethyl hexanoate`, ymax = `2 Ethyl hexanoate` + `StDev 2 Ethyl hexanoate`,
                    width = 3, colour = "darkgreen")) + 
  geom_line(aes(x = `Time (h)`, y = `2 Ethyl octanoate`, colour = "lavender")) +
  geom_point(aes(x = `Time (h)`, y = `2 Ethyl octanoate`, colour = "lavender")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl octanoate` - `StDev 2 Ethyl octanoate`, ymax = `2 Ethyl octanoate` + `StDev 2 Ethyl octanoate`,
                    width = 3, colour = "lavender")) +
  geom_line(aes(x = `Time (h)`, y = `2 Ethyl decanoate`, colour = "darkblue")) +
  geom_point(aes(x = `Time (h)`, y = `2 Ethyl decanoate`, colour = "darkblue")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Ethyl decanoate` - `StDev 2 Ethyl decanoate`, ymax = `2 Ethyl decanoate` + `StDev 2 Ethyl decanoate`,
                    width = 3, colour = "darkblue")) +
  scale_colour_identity(name = "Metabolites",
                        guide = "legend",
                        breaks = c("skyblue",
                                   "maroon",
                                   "gold",
                                   "forestgreen",
                                   "grey",
                                   "sienna",
                                   "darkred",
                                   "darkgreen",
                                   "lavender",
                                   "darkblue"),
                        labels = c("Ethyl acetate",
                                   "Ethanol",
                                   "Isobutyl acetate",
                                   "Ethyl butyrate",
                                   "Isobutanol",
                                   "Isoamyl acetate",
                                   "Isoamyl alcohol",
                                   "Ethyl hexanoate",
                                   "Ethyl octanoate",
                                   "Ethyl decanoate")) +
  labs(x = "Time (h)", y = "Concentration (mg/L)") +
  theme(legend.position = "right") +
  theme_minimal()

# GC ketones
## TT1

gck <- data_list[["GC_ketones"]]

ggplot(gck) +
  geom_line(aes(x = `Time (h)`, y = `1 Diacetyl`, colour = "skyblue")) +
  geom_point(aes(x = `Time (h)`, y = `1 Diacetyl`, colour = "skyblue")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 Diacetyl` - `StDev 1 Diacetyl`, ymax = `1 Diacetyl` + `StDev 1 Diacetyl`,
                    width = 3, colour = "skyblue")) + 
  geom_line(aes(x = `Time (h)`, y = `1 2,3-Pentanedione`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `1 2,3-Pentanedione`, colour = "sienna")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `1 2,3-Pentanedione` - `StDev 1 2,3-Pentanedione`, ymax = `1 2,3-Pentanedione` + `StDev 1 2,3-Pentanedione`, 
                    width = 3, colour = "sienna")) +
  scale_colour_identity(name = "Diketones",
                        guide = "legend",
                        breaks = c("skyblue",
                                   "sienna"),
                        labels = c("Diacetyl",
                                   "2,3-Pentanedione")) +
  labs(x = "Time (h)", y = "Concentration (mg/L)") +
  theme(legend.position = "right") +
  theme_minimal()

## TT2

gck <- data_list[["GC_ketones"]]

ggplot(gck) +
  geom_line(aes(x = `Time (h)`, y = `2 Diacetyl`, colour = "skyblue")) +
  geom_point(aes(x = `Time (h)`, y = `2 Diacetyl`, colour = "skyblue")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 Diacetyl` - `StDev 2 Diacetyl`, ymax = `2 Diacetyl` + `StDev 2 Diacetyl`,
                    width = 3, colour = "skyblue")) + 
  geom_line(aes(x = `Time (h)`, y = `2 2,3-Pentanedione`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `2 2,3-Pentanedione`, colour = "sienna")) +
  geom_errorbar(aes(x = `Time (h)`, ymin = `2 2,3-Pentanedione` - `StDev 2 2,3-Pentanedione`, ymax = `2 2,3-Pentanedione` + `StDev 2 2,3-Pentanedione`, 
                    width = 3, colour = "sienna")) +
  scale_colour_identity(name = "Diketones",
                        guide = "legend",
                        breaks = c("skyblue",
                                   "sienna"),
                        labels = c("Diacetyl",
                                   "2,3-Pentanedione")) +
  labs(x = "Time (h)", y = "Concentration (mg/L)") +
  theme(legend.position = "right") +
  theme_minimal()

# Att

att <- data_list[["Attenuation"]]

ggplot(att) +
  geom_line(aes(x = `Time (h)`, y = `TT1`, colour = "skyblue")) +
  geom_point(aes(x = `Time (h)`, y = `TT1`, colour = "skyblue")) +
  geom_line(aes(x = `Time (h)`, y = `TT2`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `TT2`, colour = "sienna")) +
  scale_colour_identity(name = "Attenuation",
                        guide = "legend",
                        breaks = c("skyblue",
                                   "sienna"),
                        labels = c("TT1",
                                   "TT2")) +
  labs(x = "Time (h)", y = "Attenuation (°P)") +
  theme(legend.position = "right") +
  theme_minimal()

# pH

ph <- data_list[["pH"]]

ggplot(ph) +
  geom_line(aes(x = `Time (h)`, y = `TT1`, colour = "yellow")) +
  geom_point(aes(x = `Time (h)`, y = `TT1`, colour = "yellow")) +
  geom_line(aes(x = `Time (h)`, y = `TT2`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `TT2`, colour = "sienna")) +
  scale_colour_identity(name = "pH",
                        guide = "legend",
                        breaks = c("yellow",
                                   "sienna"),
                        labels = c("TT1",
                                   "TT2")) +
  scale_y_continuous(limits = c(0, 7)) +
  labs(x = "Time (h)", y = "pH") +
  theme(legend.position = "right") +
  theme_minimal()

viability <- data_list[["CellCount_Viability"]]

ggplot(viability) +
  geom_line(aes(x = `Time (h)`, y = `1 Total cells`, colour = "skyblue")) +
  geom_point(aes(x = `Time (h)`, y = `1 Total cells`, colour = "skyblue")) +
  geom_line(aes(x = `Time (h)`, y = `2 Total cells`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `2 Total cells`, colour = "sienna")) +
  scale_colour_identity(name = "Cell count",
                        guide = "legend",
                        breaks = c("skyblue",
                                   "sienna"),
                        labels = c("TT1",
                                   "TT2")) +
  #scale_y_continuous(limits = c(0, 7)) +
  labs(x = "Time (h)", y = "Cell count (cells/ml)") +
  theme(legend.position = "right") +
  theme_minimal()

ggplot(viability) +
  geom_line(aes(x = `Time (h)`, y = `1 Viability (%)`, colour = "skyblue")) +
  geom_point(aes(x = `Time (h)`, y = `1 Viability (%)`, colour = "skyblue")) +
  geom_line(aes(x = `Time (h)`, y = `2 Viability (%)`, colour = "sienna")) +
  geom_point(aes(x = `Time (h)`, y = `2 Viability (%)`, colour = "sienna")) +
  scale_colour_identity(name = "Cell count",
                        guide = "legend",
                        breaks = c("skyblue",
                                   "sienna"),
                        labels = c("TT1",
                                   "TT2")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Time (h)", y = "Viability (fraction)") +
  theme(legend.position = "right") +
  theme_minimal()

