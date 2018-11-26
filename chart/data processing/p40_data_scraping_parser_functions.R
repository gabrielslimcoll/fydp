# ================================================================================
# Future Years Defense Program
# --------------------------------------------------------------------------------
# data scraping 
# ================================================================================

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
  
  source("code/p40_data_scraping_tidy_functions.R")
  
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
  # browser()
  
  col_index <- str_detect(sheet, "Description:")
  row_num <- grep("Description:", sheet[,col_index])
  # row_num <- grep("Description", sheet[[names(sheet[,col_index])]])
  value_found <- str_split(sheet[row_num,col_index], "Description:")[[1]][2]

  return(value_found)
}

# ================================================================================
