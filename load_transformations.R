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

transformed_data_list <- list(Notes=data_list[["Notes"]],
                              Experimental_parameters=data_list[["Experimental_parameters"]],
                              CellCount_Viability=data_list[["CellCount_Viability"]]
                              )
df <-data_list[["CellCount_Viability"]]

df <- df %>%
  mutate("avg_total_cells" = rowMeans(across(c("1 Total cells", "2 Total cells"))),"stdev_total_cells"= sqrt((.data[["1 Total cells"]]-.data[["2 Total cells"]])^2/2))

transformed_data_list[["CellCount_Viability"]] <- df

