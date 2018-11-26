# ================================================================================
# Future Years Defense Program
# --------------------------------------------------------------------------------
# data scraping 
# ================================================================================

# --------------------------------------------------------------------------------
# FYDP parser functions
# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------
# Function to open a sheet
# --------------------------------------------------------------------------------
FYDP_open_sheet <- function(filename, sheetname){
  opened <- read.xlsx(
    xlsxFile = filename,
    sheet = sheetname,
    colNames = FALSE
  )
  return(opened)
}

# --------------------------------------------------------------------------------
# Function to tidy the data on one sheet
# --------------------------------------------------------------------------------
FYDP_tidy_sheet <- function(sheet){
  
  source("code/r2_data_scraping_tidy_functions.R")
  
  sheet <- FYDPt_remove_description(sheet)
  table <- suppressWarnings(FYDPt_cut_table(sheet))
  table <- suppressWarnings(FYDPt_tidy_table(table))
  table <- FYDPt_extend_table(table, sheet)
  
  return(table)
}

# --------------------------------------------------------------------------------
# Function to copy the description into a seperate dataset w/ key
# --------------------------------------------------------------------------------

FYDP_copy_description <- function(sheet){
  require(stringr)
  
  col_index <- str_detect(sheet, "Budget Item Justification")
  row_num <- grep("Budget Item Justification", sheet[,col_index])
  value_found <- str_split(sheet[row_num,col_index], "Budget Item Justification")[[1]][2]
  
  return(value_found)
}

# ================================================================================