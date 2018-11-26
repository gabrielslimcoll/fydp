# ================================================================================
# Future Years Defense Program
# --------------------------------------------------------------------------------
# data scraping 
# ================================================================================

# --------------------------------------------------------------------------------
# Functions called by FYDP_tidy_sheet
# --------------------------------------------------------------------------------

FYDPt_remove_description <- function(sheet){
  row <- grep("A\\..Mission Description and Budget Item Justification", sheet$X1)
  if(!is.na(row[1])){
    sheet <- sheet %>% slice(-row)
  }
  return(sheet)
}

# --------------------------------------------------------------------------------
# Cut the table from the worksheet
# --------------------------------------------------------------------------------

FYDPt_cut_table <- function(sheet){
  
  # y <- as.character("RDT&E Articles", sheet$X1)
  # sp <- y[is.character(y) & y== "NA"] <- "0"
  
  
  # find the first and last row of the table based on first-column text
  table_start_line <- grep("COST", sheet$X1)[1]
  table_finish_line <- grep("A\\..Mission.Description|Note|Congressional.Adds|JPALS|UNDIST|Maritime.Cyber|XM1166|Combat.Service.Support.Systems|PIM.Improvement.Program|Small.Business.Innovation.Research|NIGHTFIST|Camps", sheet$X1)[1]
  
  if(is.na(table_finish_line)){
    table_finish_line <- nrow(sheet)
  }
   
  # y <- table_finish_line
  # table_finish_line <- y[is.character(y) & y== "NA"] <- "0"

     
  # library(qdap)
  # table_finish_line <- NAer(table_finish_line)
    
  #  strings <- c ()
  # # 
  #  <- str_extract(strings, pattern=".*(?= A. Mission and Description)") 
  # # 
  #  table_finish_line <- s
  
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
  
  # browser()
  
  # column names are currently in the first row - set them as column names
  colnames(table) <- as.character(unlist(table[1,]))
  
  # get rid of empty columns
  #http://stackoverflow.com/questions/15968494/how-to-delete-columns-that-contain-only-nas
  table <- table[colSums(!is.na(table)) > 0]
  
  if(ncol(table) == 13){
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
  }else{
    # get rid of columns whose name is "NA"
    nacol <- which(is.na(colnames(table))) 
    table <- table[,-nacol]
    
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
  }
  
  return(table)
}

# --------------------------------------------------------------------------------
# Extend an already-tidy table to include all desired columns
# --------------------------------------------------------------------------------
FYDPt_extend_table <- function(table, sheet){
  
  require(stringr)
  
  find_value <- function(regex, source = sheet){
    col_index <- str_detect(source, regex)
    if(!any(col_index)){return(NA)} 
    
    row_num <- grep(regex, source[,col_index])
    value_found <- str_split(source[row_num,col_index], ": ")[[1]][2]
    
    return(value_found)
  }
  
  table <- table %>%
    mutate(Appropriation.Budget.Activity = find_value("Appropriation/Budget Activity")) %>%
    # mutate(Item.MDAP.MAIS.Codes = find_value("Item MDAP/MAIS Code\\(s\\):")) %>%
    # mutate(Service.Ready.ID.Code = find_value("ID Code")) %>%
    # mutate(PE.for.Code.B.Items = find_value("Elements for Code B Items:")) %>%
    # mutate(Other.Related.PE = find_value("Other Related Program Elements:")) %>%
    mutate(Date = find_value("Date:")) %>%
    mutate(Document = find_value("Budget Item Justification:"))
  # 
  # # split the Appropriation, Budget Activity
   col_index <- str_detect(sheet, "Appropriation/Budget Activity")
   row_num <- grep("Appropriation/Budget Activity", sheet[,col_index])
   value_found <- str_split(sheet[row_num, col_index], "\n")[[1]][-1]
   value_found <- paste(value_found, collapse = "")
  value_split <- str_split(value_found, "/")

   table <- table %>%
     mutate(Appropriation = value_split[[1]][1]) %>%
     mutate(Budget.Activity = value_split[[1]][2]) #%>%
  # #mutate(Budget.Sub.Activity = value_split[[1]][3])
  # 
  # split the R-1 Program Element (Number/Name)
  col_index <- str_detect(sheet, "R-1 Program Element")
  row_num <- grep("R-1 Program Element", sheet[,col_index])
  value_found <- str_split(sheet[row_num, col_index], "\n")[[1]][-1]
  value_found <- paste(value_found, collapse = "")
  value_split <- str_split(value_found, "/")

  table <- table %>%
    mutate(R1.Program.Item.Element.Number = value_split[[1]][1]) %>%
    mutate(R1.Program.Item.Element.Name = value_split[[1]][2])

  # split the Project(Number/Name)
  # col_index <- str_detect(sheet, "Project")
  # row_num <- grep("Project", sheet[,col_index])
  # value_found <- str_split(sheet[row_num, col_index], "\n")[[1]][-1]
  #value_found <- paste(value_found, collapse = "")
  # value_split <- str_split(value_found, "/")
  
  #table <- table %>%
  #  mutate(Project.Number = value_split[[1]][1]) %>%
  # mutate(Project.Name = value_split[[1]][2])
  
  
  return(table)
}

# ================================================================================