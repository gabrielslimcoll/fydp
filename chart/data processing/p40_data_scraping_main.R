# ================================================================================
# Future Years Defense Program
# --------------------------------------------------------------------------------
# data scraping 
# ================================================================================

# --------------------------------------------------------------------------------
# This is the P-40 main scraping script which calls functions in "p40_data_scraping_parser_functions.R" and
# "p40_data_scraping_tidy_functions.R" when running

# "p40_parsing" function: 
# Input: original excel files being converted from DOD budget materials pdf
# Output: csv files with information scraped 
# --------------------------------------------------------------------------------

require(tidyverse)
library(openxlsx)

# --------------------------------------------------------------------------------
# We're using .Rproj to manage all data and R scripts and we assume certain folder structure --- as shown below, we 
# use relative path in stead of absolute path to access anywhere in the R project folder
# --------------------------------------------------------------------------------


source("code/p40_data_scraping_parser_functions.R")
source("code/p40_data_scraping_tidy_functions.R")

p40_parsing <- function(filepath,           # e.g."excel/Air Force/Procurement/"
                        filename            # e.g."Air Force Aircraft Procurement Vol I FY19.xlsx"
){
  
  wholepathname <- paste0(filepath,filename)
  
  sheets <- getSheetNames(wholepathname)
  
  descriptions <- data.frame(
    Source.File = rep(filename, length(sheets)),
    Table = character(length(sheets)),
    Description = character(length(sheets)),
    stringsAsFactors = FALSE
  )
  
  datalist = list()
  
  for(i in seq_along(sheets)){
    
    current_sheet <- FYDP_open_sheet(wholepathname, sheets[i])

    
  # We use "Resource Summary","Total Obligation Authority" and "Exhibit P-40, Budget Line Item Justification"
  # to locate the p-40 pages of interest; we found that "Resource Summary" is usually in the fourth/fifth row, first
  # column in the excel; "Total Obligation Authority" and "Exhibit P-40, Budget Line Item Justification" are usually 
  # in first column and only appear once in P-40 pages
    
    
    if((grepl("Resource Summary", current_sheet[4,1]) ||
        grepl("Resource Summary", current_sheet [5,1])) &
       (sum(grepl("Total Obligation Authority",current_sheet[,1])) == 1) &
       (sum(grepl("Exhibit P-40, Budget Line Item Justification", current_sheet[,1]))) == 1){
      
      descriptions$Description[i] <- FYDP_copy_description(current_sheet)
      descriptions$Table[i] <- sheets[i]
      
      current_sheet <- FYDP_tidy_sheet(current_sheet)
      current_sheet <- mutate(current_sheet, Table = sheets[i])
      datalist[[i]] <- current_sheet
      cat("Found a good one, #", i, "\n")
    }
    cat("Reading", i, "\n")
  }
  
  # --------------------------------------------------------------------------------
  # Merge all the dataframes in *datalist* into one big dataframe,
  # and write it out to a .csv.
  # --------------------------------------------------------------------------------
  
  data <- bind_rows(datalist)
  data <- mutate(data, Source.File = filename)
  
  write.csv(data, paste0(filepath,"output/","outputfile ",filename,".csv"),row.names = FALSE)
  write.csv(descriptions, paste0(filepath,"descriptions/","descriptionfile ",filename,".csv"),row.names = FALSE)
}

# The follow part is funcion call
p40_parsing("excel/Army/Procurement/","PB19_AMMO.xlsx")

# ================================================================================