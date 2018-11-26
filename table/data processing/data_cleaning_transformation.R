# ================================================================================
# Future Years Defense Program
# --------------------------------------------------------------------------------
# data cleaning and transformation 
# ================================================================================

library(dplyr)
library(tidyr)
library(tidyverse)
library(forcats)
library(data.table)
library(stringr)
library(magrittr)
options(scipen = 99)

# --------------------------------------------------------------------------------

# Procurement Section

# merge_files function
# Input: all output files in a given directory
# Output: merged output file in the given directory

merge_files <- function(filepath,         # e.g."excel/Air Force/Procurement/clean_merge_output"
                        organization,     # e.g."Army"
                        year,             # e.g."FY19"
                        account           # e.g."Procurement"
                        
){

  filelist <- list.files(path = filepath, pattern = "outputfile ",all.files = FALSE)
  allfiles <- list()
  for(i in 1:length(filelist)){
    allfiles[[i]] <- read.csv(paste0(filepath,"/",filelist[i]),stringsAsFactors = FALSE)
  }
  allfiles_merge <- do.call("rbind",allfiles)
  write.csv(allfiles_merge,file = paste0(filepath,"/",year," ",organization," ",account,".csv"),row.names = FALSE)
  return(allfiles_merge)
}

army19p <- merge_files("excel/army/procurement/output","army","fy19","procurement")
navy19p <- merge_files("excel/navy/procurement/output","navy","fy19","procurement")
airforce19p <- merge_files("excel/air_force/procurement/output","air force","fy19","procurement")
fy19_proc <- rbind(army19p,navy19p,airforce19p)

# Merge all three datasets above

write.csv(fy19_proc,file = paste0("excel/2019_procurement_rdt&e/","2019_procurement_merged",".csv"),row.names = FALSE)

# --------------------------------------------------------------------------------

# FYDP_clean function - procurement
# Input: 2019_procurement_merged.csv
# Output: 2019_procurement_merged_clean.csv

FYDP_clean <- function(filepath,     
                       filename      
){

    wholepath <- paste0(filepath,filename)
    
    x <- read.csv(wholepath, stringsAsFactors = F)
    
    # Split column M "Appropriation" into 3 columns 
    x <- x %>%
      separate(Appropriation, into = c("Account", "Account.Title"), sep = ": ") %>%
      separate(Account.Title, into = c("Account.Title", "Organization"), sep = ", ")
    
    # Split column N "Budget.Activity" into two columns
    x <- x %>%
      separate(Budget.Activity, into = c("Budget.Activity", "Budget.Activity.Title"), sep = ": ") %>%
      separate(Budget.Activity, into = c("Aux", "Budget.Activity"), sep = "A ") %>%
      select(-Aux)
    
    # Split column O "Budget.Sub.Activity"
    x <- x %>%
      separate(Budget.Sub.Activity, into = c("Budget.Sub.Activity", "Budget.Sub.Activity.Title"), sep = ": ") %>%
      separate(Budget.Sub.Activity, into = c("Aux", "Budget.Sub.Activity"), sep = "A ") %>%
      select(-Aux)
    
    
    # Rename "P-1 Line Item Number" to "Line Item" using gsub instead of using absolute number to 
    # refernce a column in a dataframe since dataframe columns can change 
    
    # names(data)[19] <- "Line Item"
    names(x) <- gsub("P1.Line.Item.Number","Line Item",names(x))
    
    # Rename "Year.Account" to "Fiscal Year" (same modification here)
    # names(data)[4] <- "Fiscal Year"
    names(x) <- gsub("Year.Account","Fiscal Year",names(x))
    
    # data$Year.Account <- gsub("^.*?_",".", )
    
    # Delete "Line.Item.MDAP.MAIS.Code", "Item.MDAP.MAIS.Codes", "Service.Ready.ID.Code",
    # "PE.for.Code.B.Items", "Other.Related.PE", "x"
    # x <- x %>%
    #   select(-Service.Ready.ID.Code,-Service.Ready.ID.Code,
    #          -PE.for.Code.B.Items, -Other.Related.PE, -V1, -X)
    
    # Modification here: not sure what V1, X represent, but I guess they are row names being
    # saved in the write.csv process
    x <- x %>%
      select(-Service.Ready.ID.Code,-Service.Ready.ID.Code,
             -PE.for.Code.B.Items, -Other.Related.PE)
    
    x$'Fiscal Year' <- gsub("(.*)\\.(2.*)","\\2",x$'Fiscal Year')
    
    x$Resource.Summary <- gsub("(.*)\\((.*))", "\\1", x$Resource.Summary)
    
    x$Millions[is.na(x$Millions)] <- "0"
    x$Quantity[is.na(x$Quantity)] <- "0"
    
    
    # Split "Date" into "Month", "Year"
    x <- x %>%
      separate(Date, into = c("Month", "Year"), sep = " ") %>%
      mutate(Month, Month = substr(Month, 1, 3)) %>%
      mutate(Month, Month = match(Month, month.abb))
    
    # Mutate "Table". e.g. "Table 37" -> "37"
    x <- x %>%
      separate(Table, into = c("Aux", "Table"), sep = " ") %>%
      select(-Aux)
    
    
    # Transform the millions to dollars
    names(x) <- gsub("Millions","Amount",names(x))
    
    # There exists "Continuing" in the "Amount" column -- either transform them to all zero, or
    # drop them and fill in NAs
    # the original approach was to replace them to zero since in "Full Procurement Data", in column
    # "Expense Type", "To Complete" is associated with either NA or 0 in the "Amount" column
    x$Amount <- gsub("Continuing",0,x$Amount)
    
    
    # Separate "Fiscal Year" into "Fiscal.Year" and "Expense Type"
    # Replace "....Base" with "Base", "....OCO" with "OCO", "2018....""2022....." with "Yearlt Total"
    # Replace "Total" with "Program Total" and standardize the name of "Prior years" and "To Complete"
    
    x$`Expense.Type` <- x$`Fiscal Year`
    
    x$`Expense.Type`[grepl("Base",x$`Expense.Type`,ignore.case = FALSE)] <- "Base"
    x$`Expense.Type`[grepl("OCO",x$`Expense.Type`,ignore.case = FALSE)] <- "OCO"
    x$`Expense.Type`[x$`Expense.Type` == "Total"] <- "Program Total"
    x$`Expense.Type`[grepl("To.Complete",x$`Expense.Type`,ignore.case = FALSE)] <- "To Complete"
    x$`Expense.Type`[grepl("Prior.Years",x$`Expense.Type`,ignore.case = FALSE)] <- "Prior Years"
    x$`Expense.Type`[grepl("^\\d{4}",x$`Expense.Type`,ignore.case = FALSE)] <- "Yearly Total"
    
    # In the column "Fiscal Year", we transform all "To Complete","Program Total","Prior Years" to NA and all the others
    # to numeric data
    
    x$`Fiscal Year`[x$`Fiscal Year` %in% c("To.Complete","Prior.Years","Total")] <- NA
    x$`Fiscal Year` <- as.numeric(substr(x$`Fiscal Year`,1,4))
    
    
    # Transform Amount from millions to dollars
    x$Amount <- as.numeric(x$Amount)
    x$Amount <- x$Amount * 1000000
    
    x$Budget.Activity <- as.numeric(x$Budget.Activity)
    
    # Create several more columns: BA.Full.Title, BSA.Full.Title, Program.Full.Title
    x$BA.Full.Title <- paste0(x$Budget.Activity.Title," (",
                                                  x$Account," BA ",
                                                  x$Budget.Activity,")")
    x$BSA.Full.Title <- paste0(x$Budget.Sub.Activity.Title," (",
                                                   x$Account," BA ",
                                                   x$Budget.Activity,".",
                                                   x$Budget.Sub.Activity,")")
    x$Program.Full.Title <- paste0(x$Program.Title," (P - ",
                                                      x$Account," BA ",
                                                      x$Budget.Activity,".",
                                                      x$Budget.Sub.Activity,
                                                      " ",x$`Line Item`,
                                                      ")")
    
    
    # trimws() will remove the leading and trailing whitespaces from character strings but one downside is that
    # it will coerce the number vector into factor --- so we only apply this function to the non-numeric vector
    # here the indexes represents the index of those numeric vectors
    
    x[,c(-2,-3,-5,-11,-15)] <- 
       as.data.frame(lapply(x[,c(-2,-3,-5,-11,-15)],trimws))
    
    x[,c(-2,-3,-5,-11,-15)] <- 
      lapply(x[,c(-2,-3,-5,-11,-15)],as.character)
    # Output with suggestions
    write.csv(x, paste0(filepath,"2019_procurement_merged_clean.csv"),row.names = FALSE)
}


FYDP_clean("excel/2019_procurement_rdt&e/","2019_procurement_merged.csv")

# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------
# Input: 2019_procurement_merged_clean.csv
# Output: fydp_data_fy19_p.csv
# --------------------------------------------------------------------------------

data0 <- read.csv("excel/2019_procurement_rdt&e/2019_procurement_merged_clean.csv",stringsAsFactors = FALSE) 

data <- data0 

data <- filter(data, (Expense.Type == "Yearly Total"))  

data <- select(data, Resource.Summary, 
               Fiscal.Year, 
               Amount, 
               Year,
               Account, 
               Account.Title, 
               Organization, 
               Budget.Activity, 
               Budget.Activity.Title, 
               Budget.Sub.Activity.Title, 
               Program.Full.Title)


colnames(data) <- c("Budget.Category", 
                    "FY", 
                    "Amount",
                    "Year", 
                    "Account",
                    "Account.Title",
                    "Organization",
                    "Budget.Activity", 
                    "Budget.Activity.Title",
                    "Budget.Sub.Activity.Title",
                    "Program.Title"
)

# make sure there are no duplicates in the data
data <- data %>% unique()


data2 <- filter(data, (FY == 2011 & Year == 2012) | 
                  (FY == 2012 & Year == 2013) | 
                  (FY == 2013 & Year == 2014) | 
                  (FY == 2014 & Year == 2015) | 
                  (FY == 2015 & Year == 2016) | 
                  (FY == 2016 & Year == 2017) |
                  (FY == 2017 & Year == 2018)
) 

data2 <- mutate(data2, 
                Year = "Actual")

data3 <- filter(data, 
                (FY == 2013 & Year == 2013) | 
                  (FY == 2014 & Year == 2014) | 
                  (FY == 2015 & Year == 2015) | 
                  (FY == 2016 & Year == 2016) | 
                  (FY == 2017 & Year == 2017) |
                  (FY == 2018 & Year == 2018)
) 

data3 <- mutate(data3, 
                Year = "Enacted")

data$Year %<>% as.character()

data <- bind_rows(data, data2, data3)

data$FY <- as.character(data$FY)

# --------------------------------------------------------------------------------

data <- filter(data, Budget.Category == "Total Obligation Authority")       

data$Budget.Activity.Title <- as.character(data$Budget.Activity.Title)
data$Budget.Activity.Title <- as.factor(data$Budget.Activity.Title)    

data$Account.Title <- as.character(data$Account.Title)
data$Account.Title <- as.factor(data$Account.Title)   

levels(data$Account.Title)[levels(data$Account.Title)=="Procurement"] <- "Marine Corps"
levels(data$Account.Title)[levels(data$Account.Title)=="Aircraft Procurement"] <- "Aircraft"
levels(data$Account.Title)[levels(data$Account.Title)=="Missile Procurement"] <- "Missile"
levels(data$Account.Title)[levels(data$Account.Title)=="Other Procurement"] <- "Other"
levels(data$Account.Title)[levels(data$Account.Title)=="Procurement of Ammo"] <- "Ammunition"
levels(data$Account.Title)[levels(data$Account.Title)=="Procurement of Ammunition"] <- "Ammunition"
levels(data$Account.Title)[levels(data$Account.Title)=="Procurement of W&TCV"] <- "Weapons & Tracked Combat Vehicles"
levels(data$Account.Title)[levels(data$Account.Title)=="Shipbuilding and Conversion"] <- "Shipbuilding and Conversion"
levels(data$Account.Title)[levels(data$Account.Title)=="Space Procurement"] <- "Space"
levels(data$Account.Title)[levels(data$Account.Title)=="Weapons Procurement"] <- "Weapons"

data$Organization <- as.character(data$Organization)
data$Organization <- as.factor(data$Organization)       

levels(data$Organization)[levels(data$Organization)=="Navy & MC"] <- "Navy"
levels(data$Organization)[levels(data$Organization)=="Marine Corps"] <- "Navy"

data$Budget.Activity.Title <- as.character(data$Budget.Activity.Title)
data$Budget.Activity.Title <- as.factor(data$Budget.Activity.Title)

levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Aircraft Spares and Repair Parts"] <- "Aircraft Supt Equipment & Facilities"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Auxiliaries, Craft, and Prior- Year Program Costs"] <- "Auxiliaries, Craft"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Communications and Electronics Equipment"] <- "Communications & Electronics Equip"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Modification of Inservice Aircraft"] <- "Modification of Aircraft"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Modification of Inservice Missiles"] <- "Modification of Missiles"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Ammunition"] <- "Ammunition"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Ammunition"] <- "Ammunition"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Space Procurement, Air Force"] <- "Space Procurement"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Spares and Repair Parts"] <- "Spare and Repair Parts"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Weapons and combat vehicles"] <- "Weapons and Combat vehicles"
levels(data$Budget.Activity.Title)[levels(data$Budget.Activity.Title)=="Weapons and Other Combat Vehicles"] <- "Weapons and Combat vehicles"

data$Year <- as.character(data$Year)
data$Year <- as.factor(data$Year)

levels(data$Year)[levels(data$Year)=="2012"] <- "2013 FYDP"
levels(data$Year)[levels(data$Year)=="2013"] <- "2014 FYDP"
levels(data$Year)[levels(data$Year)=="2014"] <- "2015 FYDP"
levels(data$Year)[levels(data$Year)=="2015"] <- "2016 FYDP"
levels(data$Year)[levels(data$Year)=="2016"] <- "2017 FYDP"
levels(data$Year)[levels(data$Year)=="2017"] <- "2018 FYDP"
levels(data$Year)[levels(data$Year)=="2018"] <- "2019 FYDP"

data$Account <- as.character(data$Account)

# --------------------------------------------------------------------------------

data$Account[data$Account == "3011F"] <- "Air Force - Ammunition"
data$Account[data$Account == "3080F"] <- "Air Force - Other"
data$Account[data$Account == "3010F"] <- "Air Force - Aircraft"
data$Account[data$Account == "3020F"] <- "Air Force - Missiles"
data$Account[data$Account == "3021F"] <- "Air Force - Space"
data$Account[data$Account == "1109N"] <- "Navy - Marine Corps"
data$Account[data$Account == "1508N"] <- "Navy - Ammunition"
data$Account[data$Account == "1506N"] <- "Navy - Aircraft"
data$Account[data$Account == "1507N"] <- "Navy - Weapons"
data$Account[data$Account == "1611N"] <- "Navy - Shipbuilding"
data$Account[data$Account == "1810N"] <- "Navy - Other"
data$Account[data$Account == "2031A"] <- "Army - Aircraft"
data$Account[data$Account == "2032A"] <- "Army - Missiles"
data$Account[data$Account == "2033A"] <- "Army - Weapons/Vehicles"
data$Account[data$Account == "2034A"] <- "Army - Ammunition"
data$Account[data$Account == "2035A"] <- "Army - Other"
data$Account[data$Account == "4557N"] <- "Navy - Sealift Fund"

# --------------------------------------------------------------------------------

class(data)

data <- data %>%
  group_by(
    FY,
    Budget.Category,
    Account,
    Account.Title, 
    Organization, 
    Budget.Activity.Title,
    Program.Title, 
    Budget.Sub.Activity.Title,
    Year) %>%
  summarize(Amount = sum(Amount)) %>% ungroup

# --------------------------------------------------------------------------------

data$Budget.Activity.Title <- as.factor(data$Budget.Activity.Title)
data$Budget.Activity.Title <- fct_explicit_na(data$Budget.Activity.Title, "NA")

data$Year <- as.character(data$Year)

data <- mutate(data, 
               Type = "Procurement")

data <- select(data, 
               FY,
               Budget.Category, 
               Account, 
               Account.Title, 
               Organization,
               Budget.Activity.Title, 
               Budget.Sub.Activity.Title,
               Program.Title, 
               Year, 
               Amount,
               Type)

colnames(data) <- c("FY", 
                    "Budget.Category",
                    "Category1", 
                    "Category", 
                    "Organization", 
                    "BA", 
                    "BSA",
                    "PT", 
                    "Year", 
                    "Amount", 
                    "Account")

data <- mutate(data, 
               President = "Actual")


data$Year <- as.character(data$Year)
data$President <- as.character(data$President)

data$President[data$Year == "2013 FYDP"] <- "Obama"
data$President[data$Year == "2014 FYDP"] <- "Obama"
data$President[data$Year == "2015 FYDP"] <- "Obama"
data$President[data$Year == "2016 FYDP"] <- "Obama"
data$President[data$Year == "2017 FYDP"] <- "Obama"
data$President[data$Year == "2018 FYDP"] <- "Trump"
data$President[data$Year == "2019 FYDP"] <- "Trump"

data$Account <- as.factor(data$Account)

write.csv(data, paste0("excel/2019_procurement_rdt&e/","fydp_data_fy19_p.csv"),row.names = FALSE)

# --------------------------------------------------------------------------------

# RDT&E Section

# Merge all RDT&E outputfile data in FY 2019

merge_output <- function(filepath,         # e.g."excel/Air Force/Procurement/output"
                         organization,     # e.g."Army"
                         year,             # e.g."FY19"
                         account           # e.g."Procurement"
                        
){

  filelist <- list.files(path = filepath, pattern = "outputfile ",all.files = FALSE)
  allfiles <- list()
  for(i in 1:length(filelist)){
    allfiles[[i]] <- read.csv(paste0(filepath,"/",filelist[i]),stringsAsFactors = FALSE)
  }
  allfiles_merge <- do.call("rbind",allfiles)
  write.csv(allfiles_merge,file = paste0(filepath,"/",year," ",organization," ",account,".csv"),row.names = FALSE)
  return(allfiles_merge)
}

army19rdte <- merge_output("excel/army/rdt&e/output","army","FY19","rdt&e")
af19rdte <- merge_output("excel/air_force/rdt&e/output","air_force","FY19","rdt&e")
navy19rdte <- merge_output("excel/navy/rdt&e/output","navy","FY19","rdt&e")

# Merge all three datasets above
fy19_rdte <- rbind(army19rdte,af19rdte,navy19rdte)
write.csv(fy19_rdte,file = paste0("excel/2019_procurement_rdt&e/","2019_rdt&e_merged",".csv"),row.names = FALSE)

# --------------------------------------------------------------------------------

# First part input: 2019_rdt&e_merged.csv
# First part output/second part input: full_rdt&e_fy19_data.csv 
# Second part output: fydp_data_FY19_r.csv

rdte_data <- read.csv("excel/2019_procurement_rdt&e/2019_rdt&e_merged.csv",stringsAsFactors = FALSE)

# Split column "Appropriation" into 2 columns 
rdte_data %<>%
  separate(Appropriation, into = c("Account", "Account.Title"), sep = ":")

names(rdte_data)[which(names(rdte_data) == "COST....in.Millions.")] <- "Budget Category"

# Split column "Budget.Activity" into two columns
rdte_data %<>%
  separate(Budget.Activity, into = c("Budget.Activity", "Budget.Activity.Title"), sep = ":") %>%
  separate(Budget.Activity, into = c("Aux", "Budget.Activity"), sep = "A") %>%
  select(-Aux)

# Rename "Year.Account" to "Fiscal Year"
names(rdte_data)[which(names(rdte_data) == "Year.Account")] <- "Fiscal Year"

rdte_data$`Fiscal Year` <- gsub("(.*)\\.(2.*)","\\2", rdte_data$`Fiscal Year`)

rdte_data %<>%
  separate(Date, into = c("Month", "Year"), sep = " ") %>%
  mutate(Month, Month = substr(Month, 1, 3)) %>%
  mutate(Month, Month = match(Month, month.abb))

# trim whitespace
rdte_data %<>% sapply(trimws) %>% as_tibble()

# fix non-numeric characters in Millions and Quantity
rdte_data$Millions <- gsub("[^0-9\\.]", "", rdte_data$Millions)
rdte_data$Millions %<>% as.numeric()


# remove trash variables and reclass appropriately
rdte_data %<>%
  select(-Appropriation.Budget.Activity) %>%
  mutate(
    Millions = as.numeric(Millions),
    Budget.Activity = as.integer(Budget.Activity)) 

rdte_data$Millions[is.na(rdte_data$Millions)] <- 0

# --------------------------------------------------------------------------------
# lookup organization from account

Accounts <- c(
  "1319" = "Navy",
  "3600" = "Air Force", 
  "2040" = "Army")

rdte_data$Organization <- Accounts[rdte_data$Account] 

# add letters to account

rdte_data$Account[rdte_data$Account == "1319"] <- "1319N"
rdte_data$Account[rdte_data$Account == "3600"] <- "3600F"
rdte_data$Account[rdte_data$Account == "2040"] <- "2040A"

# fix BA
rdte_data$Budget.Activity %<>% str_extract("[0-9]") %>% as.integer()

rdte_data$Budget.Activity.Title[rdte_data$Budget.Activity.Title == "BasicResearch"] <- "Basic Research"
rdte_data$Budget.Activity.Title[rdte_data$Budget.Activity.Title == "AppliedResearch"] <- "Applied Research"
rdte_data$Budget.Activity.Title[rdte_data$Budget.Activity.Title == "AdvancedTechnology Development (ATD)"] <- "Advanced Technology Development"
rdte_data$Budget.Activity.Title[rdte_data$Budget.Activity.Title == "AdvancedComponent Development & Prototypes (ACD&P)"] <- "Advanced Component Development and Prototypes"
rdte_data$Budget.Activity.Title[rdte_data$Budget.Activity.Title == "SystemDevelopment & Demonstration (SDD)"] <- "System Development and Demonstration"
rdte_data$Budget.Activity.Title[rdte_data$Budget.Activity.Title == "RDT&EManagement Support"] <- "RDT&E Management Support"
rdte_data$Budget.Activity.Title[rdte_data$Budget.Activity.Title == "OperationalSystems Development"] <- "Operational Systems Development"

# clean Fiscal Year format
rdte_data$`Fiscal Year`[grepl(
  "Prior", rdte_data$`Fiscal Year`, ignore.case = TRUE)] <- "Prior.Years"

rdte_data$`Fiscal Year` %<>% str_replace(
  pattern = "\\.+$", 
  replacement = "")

# clean Budget category format
rdte_data$`Budget Category`[grepl(
  "Total Program", rdte_data$`Budget Category`, ignore.case = TRUE)] <-
  "Total Program Element"

# create Amount from Millions - coerces "Continuing" to NA
rdte_data %<>% mutate(
  Amount = 1e6 * as.numeric(Millions))

# rename long names and spacey names
rdte_data %<>% rename(
  PE.Number = R1.Program.Item.Element.Number,
  Program.Title = R1.Program.Item.Element.Name,
  Fiscal.Year = `Fiscal Year`,
  Budget.Category = `Budget Category`)

# clean PE number format
rdte_data$PE.Number %<>% trimws()


# Reclass appropriately
rdte_data$Month %<>% as.integer()
rdte_data$Year %<>% as.integer()

# --------------------------------------------------------------------------------
# standardize program titles based on PE Number - use the most recent year's title 

# arrange with the newest years at top
rdte_lookup <- rdte_data %>% arrange(desc(Year)) %>% as_tibble()

# subset to unique combinations of title and lookup number
rdte_lookup %<>%
  mutate(PE.Number = trimws(PE.Number)) %>%
  ungroup() %>%
  select(Program.Title, PE.Number) %>%
  distinct(PE.Number, .keep_all = TRUE)

# rename title to NewTitle and join to original data
rdte_lookup %<>% rename(NewTitle = Program.Title)
rdte_data %<>% left_join(rdte_lookup)

# replace title with NewTitle
rdte_data$Program.Title <- rdte_data$NewTitle
rdte_data %<>% select(-NewTitle)

# --------------------------------------------------------------------------------

# remove "PE " from PE numbers
str_sub(rdte_data$PE.Number, 1, 3) <- ""
rdte_data$PE.Number %<>% trimws()

# --------------------------------------------------------------------------------
# split Budget.Category into number and title

rdte_data %<>% separate(
  col = Budget.Category,
  into = c("Budget.Category.Number", "Budget.Category.Title", "Name2"),
  sep = ":")

rdte_data$Budget.Category.Title[
  rdte_data$Budget.Category.Number == "Total Program Element"] <-
  "Total Program Element"

rdte_data$Budget.Category.Number[
  rdte_data$Budget.Category.Number == "Total Program Element"] <- NA

rdte_data$Budget.Category.Number %<>% str_replace("\\.", "")
rdte_data$Budget.Category.Number %<>% trimws()

rdte_data$Budget.Category.Title[!is.na(rdte_data$Name2)] <- 
  paste0(
    rdte_data$Budget.Category.Title[!is.na(rdte_data$Name2)],
    ":",
    rdte_data$Name2[!is.na(rdte_data$Name2)])
rdte_data$Budget.Category.Title %<>% trimws()

# --------------------------------------------------------------------------------

# relabel account titles to be more consistent with procurement data
rdte_data$Account.Title[rdte_data$Account == "3600F"] <- "Air Force - RDT&E"
rdte_data$Account.Title[rdte_data$Account == "2040A"] <- "Army - RDT&E"
rdte_data$Account.Title[rdte_data$Account == "1319N"] <- "Navy - RDT&E"


# break out fiscal years into two columns, Fiscal.Year and Expense.Type
rdte_data$Expense.Type <- "Yearly Total"
rdte_data$Expense.Type[grepl("OCO", rdte_data$Fiscal.Year)] <- "OCO"
rdte_data$Expense.Type[grepl("Base", rdte_data$Fiscal.Year)] <- "Base"
rdte_data$Expense.Type[rdte_data$Fiscal.Year == "Prior.Years"] <- "Prior Years"
rdte_data$Expense.Type[rdte_data$Fiscal.Year == "Cost.To.Complete"] <- 
  "To Complete"
rdte_data$Expense.Type[rdte_data$Fiscal.Year == "Total.Cost"] <- "Program Total"

rdte_data$Fiscal.Year %<>% str_extract("[0-9]+")

# create budget category full title
rdte_data %<>% mutate(
  Budget.Category.Full.Title = paste0(
    Program.Title, " - ", Budget.Category.Title, " (", PE.Number, ".",
    Budget.Category.Number,")"))

# ditch useless variables
rdte_data %<>% select(
  -Millions, -Name2, -Document)

# since there are some data consolidation failure in the "Year" and "Month" columns, I manually set the Year to 2018 and Month to 2
rdte_data$Year <- 2018
rdte_data$Month <- 2


# write csv  
write_csv(rdte_data,  paste0("excel/2019_procurement_rdt&e/","full_rdt&e_fy19_data.csv"))

# --------------------------------------------------------------------------------

rdte_data <- read.csv("excel/2019_procurement_rdt&e/full_rdt&e_fy19_data.csv",header = TRUE,sep = ",")

data <- rdte_data 

data %<>% mutate(
  Program.Full.Title = paste0(Program.Title, " (", "R - ", PE.Number, ")"))


data <- data %>% 
  mutate(Budget.Sub.Activity.Title = "NA")

data <- filter(data, Expense.Type == "Yearly Total")

data <- select(data, Budget.Category.Title, 
               Fiscal.Year, 
               Amount, 
               Year, 
               Account.Title, 
               Organization, 
               Budget.Activity, 
               Budget.Activity.Title, 
               Budget.Sub.Activity.Title, 
               Program.Full.Title) 

colnames(data) <- c("Budget.Category", 
                    "FY", 
                    "Amount", 
                    "Year", 
                    "Account.Title",
                    "Organization",
                    "Budget.Activity", 
                    "Budget.Activity.Title",
                    "Budget.Sub.Activity.Title",
                    "Program.Name")

data <- filter(data, FY == "2011" | 
                 FY == "2012" | 
                 FY == "2013" |
                 FY == "2013.Total" |
                 FY == "2014" |
                 FY == "2014.Total" |
                 FY == "2015" |
                 FY == "2015.Total" |
                 FY == "2016" |
                 FY == "2016.Total" |
                 FY == "2017" |
                 FY == "2017.Total" |
                 FY == "2018" |
                 FY == "2018.Total" |
                 FY == "2019" |
                 FY == "2019.Total" |
                 FY == "2020" |
                 FY == "2020.Total" |
                 FY == "2021" | 
                 FY == "2021.Total" | 
                 FY == "2022" |
                 FY == "2022.Total" |
                 FY == "2023" |
                 FY == "2023.Total"
)

data2 <- filter(data, 
                # (FY == "2011" & Year == 2012) | 
                (FY == "2012" & Year == 2013) | 
                  (FY == "2013" & Year == 2014) | 
                  (FY == "2014" & Year == 2015) | 
                  (FY == "2015" & Year == 2016) | 
                  # (FY == "2015" & Year == 2016) |
                  (FY == "2016" & Year == 2017) |
                  (FY == "2017" & Year == 2018)
                # (FY == "2017" & Year == 2017)
                # (FY == "2017.Total" & Year == 2016)
) 

data2 <- mutate(data2, 
                Year = "Actual")


data3 <- filter(data, 
                (FY == "2013" & Year == 2013) | 
                  (FY == "2014" & Year == 2014) | 
                  (FY == "2015" & Year == 2015) | 
                  (FY == "2016" & Year == 2016) | 
                  (FY == "2017" & Year == 2017) | 
                  (FY == "2018" & Year == 2018)
) 

data3 <- mutate(data3, 
                Year = "Enacted")

data$Year <- as.character(data$Year)

data <- bind_rows(data, data2, data3)

data$FY <- as.factor(data$FY)

# --------------------------------------------------------------------------------

levels(data$FY)[levels(data$FY)=="2019.Total"] <- "2019"
levels(data$FY)[levels(data$FY)=="2018.Total"] <- "2018"
levels(data$FY)[levels(data$FY)=="2017.Total"] <- "2017"
levels(data$FY)[levels(data$FY)=="2016.Total"] <- "2016"
levels(data$FY)[levels(data$FY)=="2015.Total"] <- "2015"
levels(data$FY)[levels(data$FY)=="2014.Total"] <- "2014"
levels(data$FY)[levels(data$FY)=="2013.Total"] <- "2013"

data$FY <- as.character(data$FY)
data$FY <- as.numeric(data$FY)

data <- filter(data, Budget.Category == "Total Program Element")       

data$Budget.Activity.Title <- as.character(data$Budget.Activity.Title)
data$Budget.Activity.Title <- as.factor(data$Budget.Activity.Title)       

data$Budget.Activity.Title <- as.factor(data$Budget.Activity.Title)
data$Budget.Activity.Title <- fct_explicit_na(data$Budget.Activity.Title, "Operational Systems Development")

data$Year <- as.character(data$Year)
data$Year <- as.factor(data$Year)

levels(data$Year)[levels(data$Year)=="2012"] <- "2013 FYDP"
levels(data$Year)[levels(data$Year)=="2013"] <- "2014 FYDP"
levels(data$Year)[levels(data$Year)=="2014"] <- "2015 FYDP"
levels(data$Year)[levels(data$Year)=="2015"] <- "2016 FYDP"
levels(data$Year)[levels(data$Year)=="2016"] <- "2017 FYDP"
levels(data$Year)[levels(data$Year)=="2017"] <- "2018 FYDP"
levels(data$Year)[levels(data$Year)=="2018"] <- "2019 FYDP"

data <- data %>% 
  mutate(Account = Budget.Activity.Title)

data <- data %>%
  group_by(
    FY,
    Budget.Category,
    Account, 
    Account.Title, 
    Organization, 
    Budget.Activity.Title,
    Budget.Sub.Activity.Title, 
    Program.Name, 
    Year) %>%
  summarize(Amount = sum(Amount))

data$Budget.Activity.Title <- as.factor(data$Budget.Activity.Title)
data$Budget.Activity.Title <- fct_explicit_na(data$Budget.Activity.Title, "NA")

data$Year <- as.character(data$Year)

data <- data %>% 
  mutate(Type = "RDT&E")

data$Year <- as.character(data$Year)

data <- select(data, 
               FY,
               Budget.Category, 
               Account, 
               Account.Title, 
               Organization,
               Budget.Activity.Title, 
               Budget.Sub.Activity.Title,
               Program.Name, 
               Year, 
               Amount,
               Type)

colnames(data) <- c("FY", 
                    "Budget.Category",
                    "Category1", 
                    "Category", 
                    "Organization", 
                    "BA", 
                    "BSA", 
                    "PT", 
                    "Year", 
                    "Amount", 
                    "Account")

data <- mutate(data, 
               President = "Actual")


data$Year <- as.factor(data$Year)
data$President <- as.factor(data$President)

levels(data$President)[levels(data$Year)=="2013 FYDP"] <- "Obama"
levels(data$President)[levels(data$Year)=="2014 FYDP"] <- "Obama"
levels(data$President)[levels(data$Year)=="2015 FYDP"] <- "Obama"
levels(data$President)[levels(data$Year)=="2016 FYDP"] <- "Obama"
levels(data$President)[levels(data$Year)=="2017 FYDP"] <- "Obama"
levels(data$President)[levels(data$Year)=="2018 FYDP"] <- "Trump"
levels(data$President)[levels(data$Year)=="2019 FYDP"] <- "Trump"

# levels(data$Line)[levels(data$Year)=="Actual"] <- "Actual"

data$President <- as.character(data$President)
data$Account <- as.factor(data$Account)

write.csv(data, "excel/2019_procurement_rdt&e/fydp_data_FY19_r.csv")

# --------------------------------------------------------------------------------

# Combining Section

# --------------------------------------------------------------------------------

# Combine 2019 procurement/RDT&E data with all previous years' data(after Quality Assurance) and implement one-step further data transformation

# Procurement section:
# First part Input: (1) 2014~2018 FYDP: FYDP_data_pb.csv (manual input included) (2) 2019 FYDP: fydp_data_fy19_p.csv
# First part Output/second part input: fydp_p_long.csv
# Second part output: fydp_p_wide.csv

# RDT&E section:
# First part Input: (1) 2014~2018 FYDP: FYDP_data_14_to_18_r.csv (2) 2019 FYDP: fydp_data_fy19_r.csv
# First part Output/second part input: fydp_r_long.csv
# Second part output: fydp_r_wide.csv

# Combined section:
# Input: two dataframes --- proc0 and rdte0
# OUtput: fydp_wide.csv

# --------------------------------------------------------------------------------
# procurement

FY14_to_FY18_proc <- read.csv("excel/2019_procurement_rdt&e/FYDP_data_pb.csv",header = TRUE,sep = ",")
FY19_proc <- read.csv("excel/2019_procurement_rdt&e/fydp_data_fy19_p.csv",header = TRUE, sep = ",")

names(FY19_proc)[which(names(FY19_proc) == "BA")] <- "Budget_Activity" 
names(FY19_proc)[which(names(FY19_proc) == "BSA")] <- "Budget_SubActivity"
names(FY19_proc)[which(names(FY19_proc) == "PT")] <- "Line_Item" 
names(FY19_proc)[which(names(FY19_proc) == "Year")] <- "Budget.Year" 

FY14_to_FY19_proc <- rbind(FY14_to_FY18_proc,FY19_proc)

FY14_to_FY19_proc$Category <- FY14_to_FY19_proc$Category1

FY14_to_FY19_proc <- select(FY14_to_FY19_proc, 
                            FY,
                            Budget.Category, 
                            Category1,
                            Category,
                            Organization,
                            Budget_Activity,
                            Budget_SubActivity,
                            Line_Item, 
                            
                            Budget.Year, 
                            Amount, 
                            Account, 
                            President)

FY14_to_FY19_proc$Budget.Year <- as.character(FY14_to_FY19_proc$Budget.Year)

FY14_to_FY19_proc$Budget.Year[which(FY14_to_FY19_proc$Budget.Year == "2019 FYDP")] <- "PB 2019 FYDP"

write.csv(FY14_to_FY19_proc,"excel/2019_procurement_rdt&e/fydp_p_long.csv")

# --------------------------------------------------------------------------------
# spread the procurement table 

proc0 <- FY14_to_FY19_proc

proc0$Budget.Year <- as.character(proc0$Budget.Year)

proc0$PB_2019_FYDP <- NA
proc0$PB_2019_FYDP[which(proc0$Budget.Year == "PB 2019 FYDP")] <-
  proc0$Amount[which(proc0$Budget.Year == "PB 2019 FYDP")]

proc0$PB_2018_FYDP <- NA
proc0$PB_2018_FYDP[which(proc0$Budget.Year == "PB 2018 FYDP")] <- 
  proc0$Amount[which(proc0$Budget.Year == "PB 2018 FYDP")]

proc0$PB_2017_FYDP <- NA
proc0$PB_2017_FYDP[which(proc0$Budget.Year == "PB 2017 FYDP")] <- 
  proc0$Amount[which(proc0$Budget.Year == "PB 2017 FYDP")]

proc0$PB_2016_FYDP <- NA
proc0$PB_2016_FYDP[which(proc0$Budget.Year == "PB 2016 FYDP")] <- 
  proc0$Amount[which(proc0$Budget.Year == "PB 2016 FYDP")]

proc0$PB_2015_FYDP <- NA
proc0$PB_2015_FYDP[which(proc0$Budget.Year == "PB 2015 FYDP")] <- 
  proc0$Amount[which(proc0$Budget.Year == "PB 2015 FYDP")]

proc0$PB_2014_FYDP <- NA
proc0$PB_2014_FYDP[which(proc0$Budget.Year == "PB 2014 FYDP")] <- 
  proc0$Amount[which(proc0$Budget.Year == "PB 2014 FYDP")]

proc0$Actual_Amount <- NA
proc0$Actual_Amount[which(proc0$Budget.Year == "Actual")] <- 
  proc0$Amount[which(proc0$Budget.Year == "Actual")]

proc0$Enacted_Amount <- NA
proc0$Enacted_Amount[which(proc0$Budget.Year == "Enacted")] <- 
  proc0$Amount[which(proc0$Budget.Year == "Enacted")]
#proc0$Category <- proc0$Category1

proc0 <- select(proc0, Organization,
                Account,
                PB_2019_FYDP,
                Line_Item,
                Budget_SubActivity,
                Category,
                FY,
                Actual_Amount,
                Enacted_Amount,
                PB_2014_FYDP,
                PB_2015_FYDP,
                PB_2016_FYDP,
                Budget.Category,
                Category1,
                Budget_Activity,
                Budget.Year,
                PB_2017_FYDP,
                PB_2018_FYDP,
                President)

write.csv(proc0,"excel/2019_procurement_rdt&e/fydp_p_wide.csv")

# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------
# RDT&E


FY14_to_FY18_rdte <- read.csv("excel/2019_procurement_rdt&e/FYDP_data_14_to_18_r.csv",header = TRUE,sep = ",")
full_rdte_data_fy19 <- read.csv("excel/2019_procurement_rdt&e/fydp_data_fy19_r.csv",header = TRUE,sep = ",")
names(full_rdte_data_fy19)[which(names(full_rdte_data_fy19) == "BA")] <- "Budget_Activity"
names(full_rdte_data_fy19)[which(names(full_rdte_data_fy19) == "BSA")] <- "Budget_SubActivity"
names(full_rdte_data_fy19)[which(names(full_rdte_data_fy19) == "PT")] <- "Program_Element"
names(full_rdte_data_fy19)[which(names(full_rdte_data_fy19) == "Year")] <- "Budget.Year"

full_rdte_data_fy19 <- select(full_rdte_data_fy19,-X)

FY14_to_FY19_rdte <- rbind(FY14_to_FY18_rdte,full_rdte_data_fy19)

FY14_to_FY19_rdte$Budget.Year <- as.character(FY14_to_FY19_rdte$Budget.Year)
FY14_to_FY19_rdte$Budget.Year[which(FY14_to_FY19_rdte$Budget.Year == "2019 FYDP")] <- "PB 2019 FYDP"

write.csv(FY14_to_FY19_rdte,"excel/2019_procurement_rdt&e/fydp_r_long.csv")

# --------------------------------------------------------------------------------

rdte0 <- FY14_to_FY19_rdte

rdte0$Budget.Year <- as.character(rdte0$Budget.Year)

rdte0$PB_2019_FYDP <- NA
rdte0$PB_2019_FYDP[which(rdte0$Budget.Year == "PB 2019 FYDP")] <-
  rdte0$Amount[which(rdte0$Budget.Year == "PB 2019 FYDP")]

rdte0$PB_2018_FYDP <- NA
rdte0$PB_2018_FYDP[which(rdte0$Budget.Year == "PB 2018 FYDP")] <- 
  rdte0$Amount[which(rdte0$Budget.Year == "PB 2018 FYDP")]

rdte0$PB_2017_FYDP <- NA
rdte0$PB_2017_FYDP[which(rdte0$Budget.Year == "PB 2017 FYDP")] <- 
  rdte0$Amount[which(rdte0$Budget.Year == "PB 2017 FYDP")]

rdte0$PB_2016_FYDP <- NA
rdte0$PB_2016_FYDP[which(rdte0$Budget.Year == "PB 2016 FYDP")] <- 
  rdte0$Amount[which(rdte0$Budget.Year == "PB 2016 FYDP")]

rdte0$PB_2015_FYDP <- NA
rdte0$PB_2015_FYDP[which(rdte0$Budget.Year == "PB 2015 FYDP")] <- 
  rdte0$Amount[which(rdte0$Budget.Year == "PB 2015 FYDP")]

rdte0$PB_2014_FYDP <- NA
rdte0$PB_2014_FYDP[which(rdte0$Budget.Year == "PB 2014 FYDP")] <- 
  rdte0$Amount[which(rdte0$Budget.Year == "PB 2014 FYDP")]

rdte0$Actual_Amount <- NA
rdte0$Actual_Amount[which(rdte0$Budget.Year == "Actual")] <- 
  rdte0$Amount[which(rdte0$Budget.Year == "Actual")]

rdte0$Enacted_Amount <- NA
rdte0$Enacted_Amount[which(rdte0$Budget.Year == "Enacted")] <- 
  rdte0$Amount[which(rdte0$Budget.Year == "Enacted")]

rdte0 <- select(rdte0, Organization,
                Account,
                Actual_Amount,
                Program_Element,
                Budget_Activity,
                Category,
                FY,
                PB_2019_FYDP,
                PB_2018_FYDP,
                PB_2017_FYDP,
                PB_2016_FYDP,
                PB_2015_FYDP,
                PB_2014_FYDP,
                Enacted_Amount)

write.csv(rdte0,"excel/2019_procurement_rdt&e/fydp_r_wide.csv",row.names = FALSE)

# --------------------------------------------------------------------------------

rdte0$Category <- as.character(rdte0$Category)
rdte0$Organization <- as.character(rdte0$Organization)

rdte0 <- mutate(rdte0, Category1 = Category)


rdte0$Category1[rdte0$Budget_Activity == "Advanced Component Development and Prototypes" & rdte0$Organization == "Army"] <- "Army - Advanced Component Development and Prototypes"
rdte0$Category1[rdte0$Budget_Activity == "Advanced Component Development and Prototypes" & rdte0$Organization == "Navy"] <- "Navy - Advanced Component Development and Prototypes"
rdte0$Category1[rdte0$Budget_Activity == "Advanced Component Development and Prototypes" & rdte0$Organization == "Air Force"] <- "Air Force - Advanced Component Development and Prototypes"

rdte0$Category1[rdte0$Budget_Activity == "Advanced Technology Development" & rdte0$Organization == "Army"] <- "Army - Advanced Technology Development"
rdte0$Category1[rdte0$Budget_Activity == "Advanced Technology Development" & rdte0$Organization == "Navy"] <- "Navy - Advanced Technology Development"
rdte0$Category1[rdte0$Budget_Activity == "Advanced Technology Development" & rdte0$Organization == "Air Force"] <- "Air Force - Advanced Technology Development"

rdte0$Category1[rdte0$Budget_Activity == "Applied Research" & rdte0$Organization == "Army"] <- "Army - Applied Research"
rdte0$Category1[rdte0$Budget_Activity == "Applied Research" & rdte0$Organization == "Navy"] <- "Navy - Applied Research"
rdte0$Category1[rdte0$Budget_Activity == "Applied Research" & rdte0$Organization == "Air Force"] <- "Air Force - Applied Research"

rdte0$Category1[rdte0$Budget_Activity == "Basic Research" & rdte0$Organization == "Army"] <- "Army - Basic Research"
rdte0$Category1[rdte0$Budget_Activity == "Basic Research" & rdte0$Organization == "Navy"] <- "Navy - Basic Research"
rdte0$Category1[rdte0$Budget_Activity == "Basic Research" & rdte0$Organization == "Air Force"] <- "Air Force - Basic Research"

rdte0$Category1[rdte0$Budget_Activity == "Operational Systems Development" & rdte0$Organization == "Army"] <- "Army - Operational Systems Development"
rdte0$Category1[rdte0$Budget_Activity == "Operational Systems Development" & rdte0$Organization == "Navy"] <- "Navy - Operational Systems Development"
rdte0$Category1[rdte0$Budget_Activity == "Operational Systems Development" & rdte0$Organization == "Air Force"] <- "Air Force - Operational Systems Development"

rdte0$Category1[rdte0$Budget_Activity == "RDT&E Management Support" & rdte0$Organization == "Army"] <- "Army - RDT&E Management Support"
rdte0$Category1[rdte0$Budget_Activity == "RDT&E Management Support" & rdte0$Organization == "Navy"] <- "Navy - RDT&E Management Support"
rdte0$Category1[rdte0$Budget_Activity == "RDT&E Management Support" & rdte0$Organization == "Air Force"] <- "Air Force - RDT&E Management Support"

rdte0$Category1[rdte0$Budget_Activity == "System Development and Demonstration" & rdte0$Organization == "Army"] <- "Army - System Development and Demonstration"
rdte0$Category1[rdte0$Budget_Activity == "System Development and Demonstration" & rdte0$Organization == "Navy"] <- "Navy - System Development and Demonstration"
rdte0$Category1[rdte0$Budget_Activity == "System Development and Demonstration" & rdte0$Organization == "Air Force"] <- "Air Force - System Development and Demonstration"


rdte0_for_rbind <- select(rdte0,Organization,
                          Account,
                          Actual_Amount,
                          Program_Element,
                          Category1,
                          FY,
                          PB_2019_FYDP,
                          PB_2018_FYDP,
                          Enacted_Amount,
                          PB_2014_FYDP,
                          PB_2015_FYDP,
                          PB_2016_FYDP,
                          PB_2017_FYDP)


proc0_for_rbind <- select(proc0,Organization,
                          Account,
                          Actual_Amount,
                          Line_Item,
                          Category1,
                          FY,
                          PB_2019_FYDP,
                          PB_2018_FYDP,
                          Enacted_Amount,
                          PB_2014_FYDP,
                          PB_2015_FYDP,
                          PB_2016_FYDP,
                          PB_2017_FYDP)

names(proc0_for_rbind)[which(names(proc0_for_rbind) == "Line_Item")] <- "PE.LI"
names(rdte0_for_rbind)[which(names(rdte0_for_rbind) == "Program_Element")] <- "PE.LI"


FullData_2 <- rbind(proc0_for_rbind,rdte0_for_rbind)

write.csv(FullData_2,"excel/2019_procurement_rdt&e/fydp_wide.csv",row.names = FALSE)

# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------
# Input: fydp_p_long.csv and fydp_r_long.csv

# Output: fydp_long.csv
# --------------------------------------------------------------------------------

proc <- read.csv("excel/2019_procurement_rdt&e/fydp_p_long.csv")

rdte <- read.csv("excel/2019_procurement_rdt&e/fydp_r_long.csv")

data <- bind_rows(proc, rdte)

data$Line_Item <- as.character(data$Line_Item)
data$Program_Element <- as.character(data$Program_Element)
data$PT <- data$Line_Item 

data$PT[is.na(data$PT)] <- data$Program_Element[!(is.na(data$Program_Element))]

data <- select(data, 
               FY,
               Budget.Category, 
               Category1,
               
               Budget_SubActivity,
               PT, 
               
               Organization, 
               Budget.Year, 
               Amount, 
               Account, 
               President)

data <- data %>%
  group_by(
    FY,
    Budget.Category, 
    Category1, 
    
    Budget_SubActivity,
    PT, 
    
    Organization, 
    Budget.Year, 
    Account, 
    President) %>%
  summarize(Amount = sum(Amount)) %>% ungroup()


data <- filter(data, (Budget.Year == "Actual") | 
                 (Budget.Year == "Enacted") | 
                 (Budget.Year == "PB 2013 FYDP" & FY != 2011 & FY != 2012) | 
                 (Budget.Year == "PB 2014 FYDP" & FY != 2012 & FY != 2013) |
                 (Budget.Year == "PB 2015 FYDP" & FY != 2013 & FY != 2014) |
                 (Budget.Year == "PB 2016 FYDP" & FY != 2014 & FY != 2015 & FY != 2013) |
                 (Budget.Year == "PB 2017 FYDP" & FY != 2015 & FY != 2016) | 
                 (Budget.Year == "PB 2018 FYDP" & FY != 2016 & FY != 2017) |
                 (Budget.Year == "PB 2019 FYDP" & FY != 2017 & FY != 2018)
)

names(data)[which(names(data) == "Budget.Year")] <- "Year"

# --------------------------------------------------------------------------------

data$Category1 <- as.character(data$Category1)
data$Organization <- as.character(data$Organization)

data <- mutate(data, Category2 = Category1)


data$Category2[data$Category1 == "Advanced Component Development and Prototypes" & data$Organization == "Army"] <- "Army - Advanced Component Development and Prototypes"
data$Category2[data$Category1 == "Advanced Component Development and Prototypes" & data$Organization == "Navy"] <- "Navy - Advanced Component Development and Prototypes"
data$Category2[data$Category1 == "Advanced Component Development and Prototypes" & data$Organization == "Air Force"] <- "Air Force - Advanced Component Development and Prototypes"

data$Category2[data$Category1 == "Advanced Technology Development" & data$Organization == "Army"] <- "Army - Advanced Technology Development"
data$Category2[data$Category1 == "Advanced Technology Development" & data$Organization == "Navy"] <- "Navy - Advanced Technology Development"
data$Category2[data$Category1 == "Advanced Technology Development" & data$Organization == "Air Force"] <- "Air Force - Advanced Technology Development"

data$Category2[data$Category1 == "Applied Research" & data$Organization == "Army"] <- "Army - Applied Research"
data$Category2[data$Category1 == "Applied Research" & data$Organization == "Navy"] <- "Navy - Applied Research"
data$Category2[data$Category1 == "Applied Research" & data$Organization == "Air Force"] <- "Air Force - Applied Research"

data$Category2[data$Category1 == "Basic Research" & data$Organization == "Army"] <- "Army - Basic Research"
data$Category2[data$Category1 == "Basic Research" & data$Organization == "Navy"] <- "Navy - Basic Research"
data$Category2[data$Category1 == "Basic Research" & data$Organization == "Air Force"] <- "Air Force - Basic Research"

data$Category2[data$Category1 == "Operational Systems Development" & data$Organization == "Army"] <- "Army - Operational Systems Development"
data$Category2[data$Category1 == "Operational Systems Development" & data$Organization == "Navy"] <- "Navy - Operational Systems Development"
data$Category2[data$Category1 == "Operational Systems Development" & data$Organization == "Air Force"] <- "Air Force - Operational Systems Development"

data$Category2[data$Category1 == "RDT&E Management Support" & data$Organization == "Army"] <- "Army - RDT&E Management Support"
data$Category2[data$Category1 == "RDT&E Management Support" & data$Organization == "Navy"] <- "Navy - RDT&E Management Support"
data$Category2[data$Category1 == "RDT&E Management Support" & data$Organization == "Air Force"] <- "Air Force - RDT&E Management Support"

data$Category2[data$Category1 == "System Development and Demonstration" & data$Organization == "Army"] <- "Army - System Development and Demonstration"
data$Category2[data$Category1 == "System Development and Demonstration" & data$Organization == "Navy"] <- "Navy - System Development and Demonstration"
data$Category2[data$Category1 == "System Development and Demonstration" & data$Organization == "Air Force"] <- "Air Force - System Development and Demonstration"

FullData <- data

names(FullData)[which(names(FullData) == "Year")] <- "Budget.Year"
names(FullData)[which(names(FullData) == "Category1")] <- "Category"

names(FullData) <- c("FY","Budget.Category","Category1","Budget_SubActivity","PE.LI",             
                     "Organization","Budget.Year","Account","President","Amount","Category") 

write.csv(FullData, "excel/2019_procurement_rdt&e/fydp_long.csv")

# ================================================================================