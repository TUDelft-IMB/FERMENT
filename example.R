# Libraries ---------------------------------------------------------------

library(tidyr)
library(tidyverse)
library(ggplot2)
library(readxl)

file_path <- "..\\TT_Template_try2.xlsx"
sheets <- excel_sheets(file_path)
data_list <- lapply(sheets, function(x) read_excel(file_path, sheet = x))
names(data_list) <- sheets
View(data_list)

View(data_list[["Notes"]])
View(data_list[["Experimental_parameters"]])
View(data_list[["Planning_preculture"]])
View(data_list[["CellCount_Viability"]])
View(data_list[["Cone_viability"]])
View(data_list[["Attenuation"]])
View(data_list[["pH"]])
View(data_list[["HPLC"]])
View(data_list[["GC_esters"]])
View(data_list[["GC_ketones"]])


# Plots -------------------------------------------------------------------
# HPLC 1

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

# HPLC 2

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
