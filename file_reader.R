if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, readxl, data.table)

##Function to paste together filepaths
locations <- function(data, filepath){
  paste(filepath, data, sep = "/")
}

##Build list of high level filepaths  
filepath_list <- sapply(c(2010:2015), locations, filepath = "L:/Prices/AMR/HORT")
folder_list <- sapply(filepath_list, list.dirs, recursive = F)

##Build list of low level files  
final_folder_list <- sapply(unlist(folder_list), list.dirs, recursive = F) %>% unlist()

##Get list of all possible current week files 
file_list <- sapply("currentweek.xls", locations, filepath = final_folder_list)

#Check if files exist, and if so read them in to a list
valid_file_list <- lapply(file_list, function(x) if(file.exists(x)) read_excel(x, col_names = F))

#Remove null values
valid_file_list <- valid_file_list[!sapply(valid_file_list, is.null)]

##Convert to one long dataframe
valid_file_dataframe <- as.data.table(rbindlist(valid_file_list, fill=T))