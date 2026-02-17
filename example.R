library(readxl)

file_path <- "..\\TT_Template_try1_wip.xlsx" 
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
View(data_list[["HPLC_raw"]])
View(data_list[["HPLC"]])
View(data_list[["GC"]])

#this is a change
