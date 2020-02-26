if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, readxl, data.table, tidyr)

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
hort_dataframe <- as.data.table(rbindlist(valid_file_list, fill=T))

#Find dates and put them in a separate column
hort_dataframe <- hort_dataframe %>% 
  mutate(Date = case_when(...1 == "PRICES ARE FOR THE WEEK ENDING(2):" ~ ...3)) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  fill(Date)

##Find rows where columns are shifted over by 1 and correct latest price
hort_dataframe <- hort_dataframe %>% mutate(...5 = case_when(!is.na(...9) ~ ...6,
                                                                          is.na(...9) ~ ...5))
##Delete unwanted columns and rename existing columns
hort_dataframe <- hort_dataframe[-c(4, 6:11)]
colnames(hort_dataframe) <- c("Item", "Variety", "Units", "Price", "Date")

##Omit all rows that have no price
hort_dataframe$Price <- as.numeric(hort_dataframe$Price)
hort_dataframe <- hort_dataframe[!is.na(hort_dataframe$Price),]

##Create averages for items which have two classes and split units and currency
hort_dataframe$Units <- gsub(" each", "/each", hort_dataframe$Units)
hort_dataframe <- hort_dataframe %>% 
  fill(Item) %>%
  group_by(Date, Item, Variety, Units) %>%
  summarise(Price = mean(Price, na.rm = T)) %>%
  separate(Units, c("Currency", "Units"), "/") %>%
  na.omit() %>%
  mutate(Price = case_when(Currency == "p", Price / 100,
                           Currency != "p", ~ Price)) %>%
  mutate(Currency = "£") 

##Put text in lower case and remove spaces
hort_dataframe$Item <- tolower(hort_dataframe$Item) 
hort_dataframe$Variety <- tolower(hort_dataframe$Variety)
hort_dataframe$Item <- gsub(" ", "_", hort_dataframe$Item)
hort_dataframe$Variety <- gsub(" ", "_", hort_dataframe$Variety)

##Write tidy data to CSV
write.csv(hort_dataframe, file = "L:/Prices/AMR/HORT/Machine readable/hort_backseries.csv", row.names = F)

