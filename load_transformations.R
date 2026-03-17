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

#HPLC
df <-data_list[["HPLC"]]
df <- df %>%
  mutate("avg_maltotriose" = rowMeans(across(c("1 Maltotriose (g/L)", "2 Maltotriose (g/L)"))),"stdev_maltotriose"= sqrt((.data[["1 Maltotriose (g/L)"]]-.data[["2 Maltotriose (g/L)"]])^2/2))
df <- df %>%
  mutate("avg_maltose" = rowMeans(across(c("1 Maltose (g/L)", "2 Maltose (g/L)"))),"stdev_maltose"= sqrt((.data[["1 Maltose (g/L)"]]-.data[["2 Maltose (g/L)"]])^2/2))
df <- df %>%
  mutate("avg_glucose" = rowMeans(across(c("1 Glucose (g/L)", "2 Glucose (g/L)"))),"stdev_glucose"= sqrt((.data[["1 Glucose (g/L)"]]-.data[["2 Glucose (g/L)"]])^2/2))
df <- df %>%
  mutate("avg_fructose" = rowMeans(across(c("1 Fructose (g/L)", "2 Fructose (g/L)"))),"stdev_fructose"= sqrt((.data[["1 Fructose (g/L)"]]-.data[["2 Fructose (g/L)"]])^2/2))
df <- df %>%
  mutate("avg_glycerol" = rowMeans(across(c("1 Glycerol (g/L)", "2 Glycerol (g/L)"))),"stdev_glycerol"= sqrt((.data[["1 Glycerol (g/L)"]]-.data[["2 Glycerol (g/L)"]])^2/2))
df <- df %>%
  mutate("avg_ethanol" = rowMeans(across(c("1 Ethanol (g/L)", "2 Ethanol (g/L)"))),"stdev_ethanol"= sqrt((.data[["1 Ethanol (g/L)"]]-.data[["2 Ethanol (g/L)"]])^2/2))

transformed_data_list[["HPLC"]] <- df

#GC_esters
df <-data_list[["GC_esters"]]
df <- df %>%
  mutate("avg_ethylacetate" = rowMeans(across(c("1 Ethyl acetate", "2 Ethyl acetate"))),"stdev_ethylacetate"= sqrt((.data[["1 Ethyl acetate"]]-.data[["2 Ethyl acetate"]])^2/2))
df <- df %>%
  mutate("avg_isobutylacetate" = rowMeans(across(c("1 Isobutyl acetate", "2 Isobutyl acetate"))),"stdev_isobutylacetate"= sqrt((.data[["1 Isobutyl acetate"]]-.data[["2 Isobutyl acetate"]])^2/2))
df <- df %>%
  mutate("avg_ethylbutyrate" = rowMeans(across(c("1 Ethyl butyrate", "2 Ethyl butyrate"))),"stdev_ethylbutyrate"= sqrt((.data[["1 Ethyl butyrate"]]-.data[["2 Ethyl butyrate"]])^2/2))
df <- df %>%
  mutate("avg_isobutanol" = rowMeans(across(c("1 Isobutanol", "2 Isobutanol"))),"stdev_isobutanol"= sqrt((.data[["1 Isobutanol"]]-.data[["2 Isobutanol"]])^2/2))
df <- df %>%
  mutate("avg_isoamylacetate" = rowMeans(across(c("1 Isoamyl acetate", "2 Isoamyl acetate"))),"stdev_isoamylacetate"= sqrt((.data[["1 Isoamyl acetate"]]-.data[["2 Isoamyl acetate"]])^2/2))
df <- df %>%
  mutate("avg_isoamylalcohol" = rowMeans(across(c("1 Isoamyl alcohol", "2 Isoamyl alcohol"))),"stdev_isoamylalcohol"= sqrt((.data[["1 Isoamyl alcohol"]]-.data[["2 Isoamyl alcohol"]])^2/2))
df <- df %>%
  mutate("avg_ethylhexanoate" = rowMeans(across(c("1 Ethyl hexanoate", "2 Ethyl hexanoate"))),"stdev_ethylhexanoate"= sqrt((.data[["1 Ethyl hexanoate"]]-.data[["2 Ethyl hexanoate"]])^2/2))
df <- df %>%
  mutate("avg_ethylocatanoate" = rowMeans(across(c("1 Ethyl octanoate", "2 Ethyl octanoate"))),"stdev_ethylocatanoate"= sqrt((.data[["1 Ethyl octanoate"]]-.data[["2 Ethyl octanoate"]])^2/2))
df <- df %>%
  mutate("avg_ethyldecanoate" = rowMeans(across(c("1 Ethyl decanoate", "2 Ethyl decanoate"))),"stdev_ethyldecanoate"= sqrt((.data[["1 Ethyl decanoate"]]-.data[["2 Ethyl decanoate"]])^2/2))

final_vals<-list()
final_vals_stdev <-list()

for (col_name in grep("^(avg_)", names(df), value = TRUE)) {
    # Discard all NA values, then take the last one
    final_vals[[col_name]] <- df[[col_name]] %>% 
    discard(is.na) %>%
    last()
}
for (col_name in grep("^(stdev_)", names(df), value = TRUE)) {
  # Discard all NA values, then take the last one
  final_vals_stdev[[col_name]] <- df[[col_name]] %>% 
    discard(is.na) %>%
    last()
}

transformed_data_list[["GC_esters"]] <- df

#GC_ketones
df <-data_list[["GC_ketones"]]
df <- df %>%
  mutate("avg_diacetyl" = rowMeans(across(c("1 Diacetyl", "2 Diacetyl"))),"stdev_diacetyl"= sqrt((.data[["1 Diacetyl"]]-.data[["2 Diacetyl"]])^2/2))
df <- df %>%
  mutate("avg_23pentanedione" = rowMeans(across(c("1 2,3-Pentanedione", "2 2,3-Pentanedione"))),"stdev_23pentanedione"= sqrt((.data[["1 2,3-Pentanedione"]]-.data[["2 2,3-Pentanedione"]])^2/2))

transformed_data_list[["GC_ketones"]] <- df

for (col_name in grep("^(avg_)", names(df), value = TRUE)) {
  # Discard all NA values, then take the last one
  final_vals[[col_name]] <- df[[col_name]] %>% 
    discard(is.na) %>%
    last()
}
for (col_name in grep("^(stdev_)", names(df), value = TRUE)) {
  # Discard all NA values, then take the last one
  final_vals_stdev[[col_name]] <- df[[col_name]] %>% 
    discard(is.na) %>%
    last()
}

final_ethanol <- transformed_data_list[["HPLC"]] %>%
  summarise(across(
    .cols = matches("(avg_ethanol|stdev_ethanol)"),
    .fns = ~ last(na.omit(.x))
    ))
final_ethanol_val<-final_ethanol[["avg_ethanol"]][1]
final_ethanol_stdev<-final_ethanol[["stdev_ethanol"]][1]

df <- as.data.frame(final_vals)    
final_vals_normalized <- df %>%
  mutate(across(starts_with("avg"), ~ .x * 36.6/final_ethanol_val))

final_vals_stdev<-as.data.frame(final_vals_stdev)
final_vals<-as.data.frame(final_vals)

final_stdev_normalized <- final_vals_normalized*sqrt((final_vals_stdev/final_vals)^2+(final_ethanol_stdev/final_ethanol_val)^2)

transformed_data_list[["GC_final_vals_normalized"]] <-final_vals_normalized
transformed_data_list[["GC_final_stdev_normalized"]] <-final_stdev_normalized

