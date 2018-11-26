# ================================================================================
# Future Years Defense Program
# --------------------------------------------------------------------------------
# data scraping 
# ================================================================================

# --------------------------------------------------------------------------------
# Functions called by FYDP_tidy_sheet
# --------------------------------------------------------------------------------


FYDPt_remove_description <- function(sheet){
  row <- grep("Description:", sheet$X1)
  if(!is.na(row[1])){
    sheet <- sheet %>% slice(-row)
  }
  return(sheet)
}

# --------------------------------------------------------------------------------
# Cut the table from the worksheet
# --------------------------------------------------------------------------------
FYDPt_cut_table <- function(sheet){
  
  # browser()
  
  # find the first and last row of the table based on first-column text
  table_start_line <- grep("Resource Summary", sheet$X1)[1]
  table_finish_line <- grep("System Unit Cost", sheet$X1)[1]
  
  if(is.na(table_finish_line)){
    table_finish_line <- nrow(sheet)
  }
  
  # cut out the table and save it as *table*
  table <- sheet %>%
    slice(table_start_line:table_finish_line)
  
  return(table)
}

# --------------------------------------------------------------------------------
# Tidy an already-cut table
# --------------------------------------------------------------------------------
FYDPt_tidy_table <- function(table){
  require(tidyr)
  
  # column names are currently in the first row - set them as column names
  colnames(table) <- as.character(unlist(table[1,]))
  
  # get rid of empty columns
#http://stackoverflow.com/questions/15968494/how-to-delete-columns-that-contain-only-nas
  table <- table[colSums(!is.na(table)) > 0]
  
  # find the empty row
  numeric_columns <- select(table, -1)
  empty <- rowSums(is.na(numeric_columns)) > 0
  
  # remove the empty row
  table <- table[!empty,]
  
  # remove the first row (column names)
  table <- table %>% slice(-1)
  
  # replace NA character "-" with NA
  switch_to_NA <- function(column){
    column <- replace(column, which(column== "-"), NA)
  }
  table <- data.frame(apply(table, 2, switch_to_NA)) 
  
  # move fiscal years into rows instead of columns
  table <- gather(table,
    key = "Year.Account",
    value = "Millions",
    -1  # to keep all columns except the first
  )
  
  # Move quantities to their own column
  table <- table %>%
    mutate(Quantity =  
      ifelse(str_detect(Resource.Summary,"Quantity"),
        Millions,
        NA
      )
    ) 
  
  # and remove quantities from the Millions column
  table$Millions <- replace(table$Millions, which(!is.na(table$Quantity)), NA)
  
  return(table)
}

# --------------------------------------------------------------------------------
# Extend an already-tidy table to include all desired columns
# --------------------------------------------------------------------------------
FYDPt_extend_table <- function(table, sheet){

  require(stringr)
  
  browser()
  
  find_value <- function(regex, source = sheet){
    
    browser()
    col_index <- str_detect(source, regex)
    if(!any(col_index)){ return(NA)}
    
    # Use the exact column name to reference the column and obtain the right index
    
    
    if(sum(col_index) == 1){
      row_num <- grep(regex, source[[names(source[,col_index])]])
      value_found <- str_split(source[row_num,col_index], ": ")[[1]][2]
    } else{
      # For FY19, key word "Date: " also appears in Description/Justification section, so we assume
      # the real "Date: " information we want to extract exists in the last column of the table
      row_num <- grep(regex, source[[names(source[,col_index])[length(names(source[,col_index]))]]])
      value_found <- str_split(source[row_num,col_index], ": ")[[1]][2]
    }

    return(value_found)
  }
  
  # browser()
  
  table <- table %>%
    # mutate(Line.Item.MDAP.MAIS.Code = find_value("Line Item MDAP/MAIS Code:")) %>%
    # mutate(Item.MDAP.MAIS.Codes = find_value("Item MDAP/MAIS Code\\(s\\):")) %>%
    mutate(Service.Ready.ID.Code = find_value("ID Code")) %>%
    mutate(PE.for.Code.B.Items = find_value("Program Elements for Code B Items:")) %>%
    mutate(Other.Related.PE = find_value("Other Related Program Elements:")) %>%
    mutate(Date = find_value("Date:")) %>%
    mutate(Document = find_value("Budget Line Item Justification:"))
  
  # split the Appropriation, Budget Activity, and Budget Sub Activity field
  col_index <- str_detect(sheet, "Appropriation / Budget Activity / Budget Sub Activity:")
  
  
  # for some reasons, the row index that grep returns is 1L instead of 2L, but if we reference the 
  # column using double brackets to reference the exact column name, we can obtain the right index

  
  row_num <- grep("Appropriation / Budget Activity / Budget Sub Activity:", 
                  sheet[[names(sheet[,col_index])]],
                  ignore.case = FALSE)
  
  value_found <- str_split(sheet[row_num, col_index], "\n")[[1]][-1]
  value_found <- paste(value_found, collapse = "")
  value_split <- str_split(value_found, "/")
  
  
  table <- table %>%
    mutate(Appropriation = value_split[[1]][1]) %>%
    mutate(Budget.Activity = value_split[[1]][2]) %>%
    mutate(Budget.Sub.Activity = paste0(value_split[[1]][-c(1,2)],collapse = "/"))
      
  # split the P1 Line Item Number and Title field
  col_index <- str_detect(sheet, "1 Line Item Number / Title:")
  
  
  # Same modification here
  row_num <- grep("1 Line Item Number / Title:", sheet[[names(sheet[,col_index])]])
  
  value_found <- str_split(sheet[row_num, col_index], "\n")[[1]][-1]
  value_found <- paste(value_found, collapse = "")
  
  
  # Found that "P-1 Line Item Number / Title" may have more than one "/", so we cannot simply use
  # "/" to split the string which may cause terms leaved uncatched. Instead, use "[A-Z0-9]* / " to
  # catch only the P1 Line Item Number
  
  P1_line_item_num <- str_extract(value_found,"[A-Z0-9]* / ")
  P1_line_item_num <- gsub(" / ","",P1_line_item_num)
  Title <- gsub("[A-Z0-9]* / ","",value_found)
  # value_split <- str_split(value_found, "/")
  
  # table <- table %>%
  #   mutate(P1.Line.Item.Number = value_split[[1]][1]) %>%
  #   mutate(Program.Title = paste0(value_split[[1]][2],"/",value_split[[1]][3]))
  
  table <- table %>%
    mutate(P1.Line.Item.Number = P1_line_item_num) %>%
    mutate(Program.Title = Title)
  
  return(table)
}

# ================================================================================

