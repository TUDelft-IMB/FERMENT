# Libraries -----------------------c----------------------------------------

library(tidyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)

file_path <- "..\\excel_files\\TT_Template_example_1_102.xlsx"
sheets <- excel_sheets(file_path)
data_list <- lapply(sheets, function(x) read_excel(file_path, sheet = x))
names(data_list) <- sheets
#View(data_list)

# Remove junk rows
data_list[["Attenuation"]] <- data_list[["Attenuation"]] %>%
  row_to_names(row_number = 1) # Promotes the first row to header
data_list[["Attenuation"]] <- type.convert(data_list[["Attenuation"]], as.is = TRUE)
data_list[["Attenuation"]]$Date <- as.POSIXct(data_list[["Attenuation"]]$Date * 86400, 
                                              origin = "1899-12-30", 
                                              tz = "Europe/Amsterdam")

data_list[["pH"]] <- data_list[["pH"]] %>%
  row_to_names(row_number = 1) # Promotes the first row to header
data_list[["pH"]] <- type.convert(data_list[["pH"]], as.is = TRUE)
data_list[["pH"]]$Date <- as.POSIXct(data_list[["pH"]]$Date * 86400, 
                                              origin = "1899-12-30", 
                                              tz = "Europe/Amsterdam")

#Initialize df
transformed_data_list <- list(Notes=data_list[["Notes"]],
                              Experimental_parameters=data_list[["Experimental_parameters"]],
                              CellCount_Viability=data_list[["CellCount_Viability"]]
                              )
#Average+stdev cell count
df <-data_list[["CellCount_Viability"]]
df <- df %>%
  mutate("avg_total_cells" = rowMeans(across(c("1 Total cells", "2 Total cells"))),"stdev_total_cells"= sqrt((.data[["1 Total cells"]]-.data[["2 Total cells"]])^2/2))
# Average+stdev of viabilities
df <- df %>%
  mutate("avg_viability" = rowMeans(across(c("1 Viability (%)", "2 Viability (%)"))),"stdev_viability"= sqrt((.data[["1 Viability (%)"]]-.data[["2 Viability (%)"]])^2/2))
transformed_data_list[["CellCount_Viability"]] <- df

#Average+stdev cone viability
df <-data_list[["Cone_viability"]]
df <- df %>% 
  mutate("avg_both"=mean(c(df[["TT1 average"]][1],df[["TT2 average"]][1])),"stdev_both"=sd(c(df[["TT1 average"]][1],df[["TT2 average"]][1])))
transformed_data_list[["Cone_viability"]] <- df

df <-data_list[["Attenuation"]]
df <- df %>%
  mutate("avg_attenuation" = rowMeans(across(c("TT1", "TT2"))),"stdev_attenuation"= sqrt((.data[["TT1"]]-.data[["TT2"]])^2/2))
transformed_data_list[["Attenuation"]] <- df

df <-data_list[["pH"]]
df <- df %>%
  mutate("avg_pH" = rowMeans(across(c("TT1", "TT2"))),"stdev_pH"= sqrt((.data[["TT1"]]-.data[["TT2"]])^2/2))
transformed_data_list[["pH"]] <- df
