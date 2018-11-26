# ================================================================================
# Future Years Defense Program
# --------------------------------------------------------------------------------
# data scraping 
# ================================================================================

# --------------------------------------------------------------------------------
# This is the r-2 main scraping script which calls functions in "r2_data_scraping_parser_functions.R" and
# "r2_data_scraping_tidy_functions.R" when running

# Input: original excel files being converted from pdf
# Output: csv files with information scraped
# --------------------------------------------------------------------------------

require(tidyverse)
require(openxlsx)

# --------------------------------------------------------------------------------
# We're using .Rproj to manage all data and R scripts and we assume certain folder structure --- as shown below, we 
# use relative path in stead of absolute path to access anywhere in the R project folder
# --------------------------------------------------------------------------------

source("code/r2_data_scraping_tidy_functions.R")
source("code/r2_data_scraping_parser_functions.R")

r2_parsing <- function(filepath,           # e.g."excel/Air Force/Procurement/"
                       filename            # "Air Force Aircraft Procurement Vol I FY19.xlsx"
){
  wholepathname <- paste0(filepath,filename)
  
  # Read names of all worksheets in the workbook into character vector *sheets*
  sheets <- getSheetNames(wholepathname)
  
  # --------------------------------------------------------------------------------
  # Initialize *descriptions* dataframe:
  # Stores descriptions, with file name and table name to use as keys
  # allowing descriptions to be rejoined with data later if desired
  # --------------------------------------------------------------------------------
  descriptions <- data.frame(
    Source.File = rep(filename, length(sheets)),
    Table = character(length(sheets)),
    Description = character(length(sheets)),
    stringsAsFactors = FALSE
  )
  
  # --------------------------------------------------------------------------------
  # Initialize *datalist*:
  # A list where we'll dump dataframes full of relevant data,
  # as we find it while scanning through the worksheets.
  # --------------------------------------------------------------------------------
  datalist = list()
  
  # --------------------------------------------------------------------------------
  # Scan through worksheets one at a time, looking for relevant data.
  # Upon finding one with the format we're looking for, read it and tidy the data.
  # Then dump the tidied dataframe into *datalist*
  # --------------------------------------------------------------------------------
  for(i in seq_along(sheets)){
    
    current_sheet <- FYDP_open_sheet(wholepathname, sheets[i])
    
  # We use "COST" and "Exhibit R-2, RDT&E Budget Item Justification" to locate the 
  # r-2 pages of interest; we found that "COST" is usually in the fourth/third row, first
  # column in the excel; "Exhibit R-2, RDT&E Budget Item Justification" is usually 
  # in first column first row in excel
    
    if((grepl("COST", current_sheet[3,1]) ||
        grepl("COST", current_sheet[4,1])) & 
       grepl("Exhibit R-2, RDT&E Budget Item Justification", current_sheet[1,1])){
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
r2_parsing("excel/Air Force/RDT&E/","Air Force Research Development Test and Evaluation Vol II FY19 v2.xlsx")

# ================================================================================
