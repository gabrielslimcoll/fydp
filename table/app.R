# ================================================================================
# Future Years Defense Program
# Designed and built by Gabriel Coll, Yuanjing Han, Loren Lipsey, Shivani Pandya,
# ...and Kayla Keller
# --------------------------------------------------------------------------------
# table app: user interface and server
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

require(shiny)
require(ggplot2)
library(dplyr)
require(scales)
require(Cairo)
require(grid)
require(gridExtra)
library(forcats)
library(shinyBS)
library(DT)
Sys.setlocale("LC_ALL", "C")

# --------------------------------------------------------------------------------
# begin user interface 

ui <- 
  
  fluidPage(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Open+Sans');
                    
                    body {
                    font-family: 'Open Sans',  sans-serif;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #554449;
                    }
                    
                    ")),
    
    tags$head(
      tags$style(HTML("body{background-color: #fcfcfc;}"))),
    tags$div(HTML("<div class='fusion-secondary-header'>
                <div class='fusion-row'>
              <div class='fusion-alignleft'><div class='fusion-contact-info'><center style=' padding:15px;'><a href='https://defense360.csis.org/content-type/data/' target='_blank'><img class='logo' src='https://defense360.csis.org/wp-content/uploads/2015/08/ISP_new.png' width='40%'></a></center><a href='mailto:'></a></div></div>
              </div>
              </div>")),
    tags$style(HTML(".fusion-secondary-header {border-bottom: 2.5px solid #6F828F}")),
    br(),
    tags$script(HTML(
      "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
          (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
          m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
        })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
      
      ga('create', 'UA-99363803-1', 'auto');
      ga('send', 'pageview')"
    )),
    
    br(),
    
    fluidRow(
      column(4,
             selectInput("Account", "Account", choices = c("RDT&E", "Procurement", "Combined"),
                         selected = "Combined", width = '150px')),
      column(4,
             conditionalPanel(
               condition = "input.Account == 'RDT&E'",
               checkboxGroupInput("category1", "Aggregate by",  choices = c("Organization", "Budget Activity" = "Budget_Activity",
                                                                            
                                                                            "Program Element" = "Program_Element"),
                                  selected = c( "Organization", "Budget_Activity", "Program_Element")
                                  , inline = TRUE)),
             
             conditionalPanel(
               condition = "input.Account == 'Procurement'",
               checkboxGroupInput("category2", "Aggregate by", choices = c(             
                 "Organization",
                 "Category",
                 "Budget Activity" = "Budget_Activity",
                 "Budget Sub Activity" = "Budget_SubActivity",
                 "Line Item" = "Line_Item"
               ),
               selected = c(
                 "Organization",
                 "Budget_Activity",
                 "Budget_SubActivity",
                 "Category",
                 "Line_Item"
               ), inline = TRUE)),
             conditionalPanel(
               condition = "input.Account == 'Combined'",
               checkboxGroupInput("category3", "Aggregate by", choices = c(             
                 "Organization",
                 "Category",
                 "Program Element/ Line Item" = "PE.LI",
                 "Account"
               ),
               selected = c(
                 "Organization",
                 "Category",
                 "Account",
                 "Program Element/ Line Item" ="PE.LI"
               ), inline = TRUE))),
      
      column(4,
             radioButtons("view", "View", choices = c("Difference", "Standard"), 
                          selected = "Standard", inline = TRUE))),
    
    fluidRow(
      conditionalPanel(
        condition = "input.view == 'Difference'",
        
        shinyjs::useShinyjs(),
        
        
        selectInput("Category1", "Difference between", c(#"PB_2013_FYDP",
          "PB 2014 FYDP" = "PB_2014_FYDP" , "PB 2015 FYDP" = "PB_2015_FYDP" , "PB 2016 FYDP" = "PB_2016_FYDP",
          "PB 2017 FYDP" = "PB_2017_FYDP", "PB 2018 FYDP" = "PB_2018_FYDP", "PB 2019 FYDP" = "PB_2019_FYDP" ,
          "Actual", "Enacted"), selected = "PB_2019_FYDP"),
        
        selectInput("Category2", "and", c(#"PB_2013_FYDP", 
          "PB 2014 FYDP" = "PB_2014_FYDP", "PB 2015 FYDP" = "PB_2015_FYDP" ,"PB 2016 FYDP" =  "PB_2016_FYDP" ,
          "PB 2017 FYDP" = "PB_2017_FYDP" , "PB 2018 FYDP" = "PB_2018_FYDP", "PB 2019 FYDP" = "PB_2019_FYDP", "Actual", "Enacted"), 
          selected = "PB_2018_FYDP"),
        
        radioButtons("zero", "Filter Zero Data", choices = c("Yes", "No"), selected = "No", inline = TRUE),
        bsTooltip("zero", "Filter out programs that have a zero dollar value",
                  placement = "bottom", trigger = "hover",
                  options = NULL)
        
      )),
    
    
    column(1), align = 'center',
    fluidRow(
      br(), 
      
      DT::dataTableOutput('tbl')), 
    
    fluidRow(        hr(), 
                     uiOutput("bottom_line"))
    
    )

# --------------------------------------------------------------------------------
# begin server 

server <- function(input, output, session){

  output$bottom_line <- renderUI({
    
    HTML(
      paste0(
        "<div align = 'left'>",
        "<h5> For more on our work, contact <a href='mailto:gcoll@csis.org?subject=Build%20your%20own%20Navy'>Gabriel Coll</a>. This is an ongoing project, so we welcome any recommendations. You can also visit the fydp <a href=https://analytics.csis.org/fydp/chart>chart app</a> here, and find more apps on <a href=https://defense360.csis.org>Defense360</a>.</h4>"))
  })
  
  FullDatap2 <- readRDS("data/fydp_p_wide.rds")
  FullDatar2 <- readRDS("data/fydp_r_wide.rds")
  FullData2 <- readRDS("data/fydp_wide.rds")
  FullDatap <- readRDS("data/fydp_p_long.rds")
  FullDatar <- readRDS("data/fydp_r_long.rds")
  FullData <- readRDS("data/fydp_long.rds")
  
  FullDatap <- filter(FullDatap,  !(Budget.Year == "PB 2013 FYDP"),
                        !(Budget.Year == "PB 2014 FYDP" & FY < 2014), 
                        !(Budget.Year == "PB 2015 FYDP" & FY < 2015),  !(Budget.Year == "PB 2016 FYDP" & FY < 2016), 
                        !(Budget.Year == "PB 2017 FYDP" & FY < 2017),  !(Budget.Year == "PB 2018 FYDP" & FY < 2018),
                        !(Budget.Year == "PB 2019 FYDP" & FY < 2019)
                        )
 
  FullDatar <- filter(FullDatar, !(Budget.Year == "PB 2013 FYDP"),
                      !(Budget.Year == "PB 2014 FYDP" & FY < 2014), 
                      !(Budget.Year == "PB 2015 FYDP" & FY < 2015),  !(Budget.Year == "PB 2016 FYDP" & FY < 2016), 
                      !(Budget.Year == "PB 2017 FYDP" & FY < 2017),  !(Budget.Year == "PB 2018 FYDP" & FY < 2018),
                      !(Budget.Year == "PB 2019 FYDP" & FY < 2019)
                      )
  
  
  FullData <- filter(FullData, !(Budget.Year == "PB 2013 FYDP"),
                     !(Budget.Year == "PB 2014 FYDP" & FY < 2014), 
                     !(Budget.Year == "PB 2015 FYDP" & FY < 2015),  !(Budget.Year == "PB 2016 FYDP" & FY < 2016), 
                     !(Budget.Year == "PB 2017 FYDP" & FY < 2017),  !(Budget.Year == "PB 2018 FYDP" & FY < 2018),
                     !(Budget.Year == "PB 2019 FYDP" & FY < 2019)
                     )
  
  Dataset <- reactive({
    
    if(input$view == "Standard" & input$Account == "RDT&E"){
      FullDatar %>%
        group_by_(.dots = c(input$category1, "Budget.Year", "FY")) %>% 
        summarize(Amount = sum(Amount), na.rm= TRUE) %>% ungroup() %>% 
        mutate(Amount=round(Amount,digits=0))}
    
    else if(input$Account == "Procurement" & input$view == "Standard"){
      FullDatap %>%
        group_by_(.dots = c(input$category2, "Budget.Year", "FY")) %>% 
        summarize(Amount = sum(Amount), na.rm = TRUE) %>% ungroup() %>% 
        mutate(Amount=round(Amount,digits=0))
    }
    
    else if(input$Account == "Combined" & input$view == "Standard"){
      FullData %>%
        group_by_(.dots = c(input$category3, "Budget.Year", "FY")) %>% 
        summarize(Amount = sum(Amount), na.rm = TRUE) %>% ungroup() %>% 
        mutate(Amount=round(Amount,digits=0))
    }
    
    else if(input$view == "Difference" & input$Account == "RDT&E"){
      
      mutate_call <- lazyeval::interp(~ (x - y), x = as.name(input$Category1), 
                                      y = as.name(input$Category2))
      mutate_call2 <- lazyeval::interp(~ (abs(x - y) / x), x = as.name(input$Category1), 
                                       y = as.name(input$Category2), digits = 2)
      mutate_call3 <- lazyeval::interp(~ (abs(y - x) / y), x = as.name(input$Category1), 
                                       y = as.name(input$Category2))
      FullDatar2 %>%
        group_by_(.dots = c(input$category1, "FY")) %>% 
        summarize(Actual = sum(Actual_Amount,  na.rm = TRUE), 
                  PB_2014_FYDP = sum(PB_2014_FYDP,  na.rm = TRUE), PB_2015_FYDP = sum(PB_2015_FYDP,  na.rm = TRUE),
                  PB_2016_FYDP = sum(PB_2016_FYDP,  na.rm = TRUE), PB_2017_FYDP = sum(PB_2017_FYDP,  na.rm = TRUE),
                  PB_2018_FYDP = sum(PB_2018_FYDP,  na.rm = TRUE), PB_2019_FYDP = sum(PB_2019_FYDP,  na.rm = TRUE),
                  Enacted = sum(Enacted_Amount), na.rm = TRUE) %>% ungroup() %>% 
        mutate_(.dots = setNames(list(mutate_call), "Difference"))%>%
        mutate_(.dots = setNames(list(mutate_call2), "Percent_Difference"), digits = 2) %>%
        mutate_(.dots = setNames(list(mutate_call3), "Percent_Difference2")) %>%
        mutate(Percent_Difference = pmin(Percent_Difference, Percent_Difference2, na.rm = TRUE))
    }
    
    else if(input$view == "Difference" & input$Account == "Procurement"){
      
      mutate_call <- lazyeval::interp(~ (x - y), x = as.name(input$Category1), 
                                      y = as.name(input$Category2))
      mutate_call2 <- lazyeval::interp(~ (abs(x - y) / x), x = as.name(input$Category1), 
                                       y = as.name(input$Category2))
      mutate_call3 <- lazyeval::interp(~ (abs(y - x) / y), x = as.name(input$Category1), 
                                       y = as.name(input$Category2))
      FullDatap2 %>%
        group_by_(.dots = c(input$category2, "FY")) %>% 
        summarize( Actual = sum(Actual_Amount, na.rm = TRUE), 
                   PB_2014_FYDP = sum(PB_2014_FYDP,  na.rm = TRUE), PB_2015_FYDP = sum(PB_2015_FYDP,  na.rm = TRUE),
                   PB_2016_FYDP = sum(PB_2016_FYDP,  na.rm = TRUE), PB_2017_FYDP = sum(PB_2017_FYDP,  na.rm = TRUE),
                   PB_2018_FYDP = sum(PB_2018_FYDP,  na.rm = TRUE), PB_2019_FYDP = sum(PB_2019_FYDP,  na.rm = TRUE),
                   Enacted = sum(Enacted_Amount), na.rm = TRUE) %>% ungroup() %>% 
        mutate(Actual=round(Actual,digits=0)) %>%
        mutate_(.dots = setNames(list(mutate_call), "Difference"))%>%
        
        mutate_(.dots = setNames(list(mutate_call2), "Percent_Difference")) %>%
        mutate_(.dots = setNames(list(mutate_call3), "Percent_Difference2")) %>%
        mutate(Percent_Difference = pmin(Percent_Difference, Percent_Difference2, na.rm = TRUE)) 
      
    }
    
    else if(input$view == "Difference" & input$Account == "Combined"){
      
      mutate_call <- lazyeval::interp(~ (x - y), x = as.name(input$Category1), 
                                      y = as.name(input$Category2))
      mutate_call2 <- lazyeval::interp(~ ((abs(x - y)) / x), x = as.name(input$Category1), 
                                       y = as.name(input$Category2))
      mutate_call3 <- lazyeval::interp(~ (abs(y - x) / y), x = as.name(input$Category1), 
                                       y = as.name(input$Category2))
      FullData2 %>%
        group_by_(.dots = c(input$category3, "FY")) %>% 
        
        summarize(Actual = sum(Actual_Amount, na.rm = TRUE), 
                  PB_2014_FYDP = sum(PB_2014_FYDP, na.rm = TRUE), PB_2015_FYDP = sum(PB_2015_FYDP, na.rm = TRUE),
                  PB_2016_FYDP = sum(PB_2016_FYDP, na.rm = TRUE), PB_2017_FYDP = sum(PB_2017_FYDP, na.rm = TRUE),
                  PB_2018_FYDP = sum(PB_2018_FYDP, na.rm = TRUE), PB_2019_FYDP = sum(PB_2019_FYDP,  na.rm = TRUE),
                  Enacted = sum(Enacted_Amount, na.rm = TRUE)) %>% ungroup() %>% 
        mutate_(.dots = setNames(list(mutate_call), "Difference")) %>%
        mutate_(.dots = setNames(list(mutate_call2), "Percent_Difference"), digits = 2) %>%
        mutate_(.dots = setNames(list(mutate_call3), "Percent_Difference2")) %>%
        mutate(Percent_Difference = pmin(Percent_Difference, Percent_Difference2, na.rm = TRUE))}
    
  })
  
  category <- reactive({
    if(input$Account == "RDT&E"){
      if(input$view == "Difference"){
        return(c( "FY", input$category1, input$Category1,input$Category2, "Difference", "Percent_Difference"))}
      else{
        return(c("Budget.Year", "FY", input$category1, "Amount"))}}
    
    else if(input$Account == "Procurement"){
      if(input$view == "Difference"){
        return(c( "FY", input$category2, input$Category1,input$Category2, "Difference", "Percent_Difference"))}
      else{
        return(c("Budget.Year", "FY", input$category2, "Amount"))}}
    
    else if(input$Account == "Combined"){
      if(input$view == "Difference"){
        return(c( "FY", input$category3, input$Category1,input$Category2, "Difference", "Percent_Difference"))}
      else{
        return(c("Budget.Year", "FY", input$category3, "Amount"))}}
    
  })
  
  Dataset2 <- reactive({ 
    if(input$Category1 == "Actual"|input$Category2 == "Actual" ){
      filter(Dataset(), FY >= 2012, FY <= 2017 )} 
    else{
      Dataset()
    }})
  
  Dataset3 <- reactive({
    if(input$Category1 == "Enacted"|input$Category2 == "Enacted" ){
      filter(Dataset2(),  FY >= 2013, FY <= 2018)} 
    else{
      Dataset2()}
  })
  
  Dataset4 <- reactive({
    if(input$Category1 == "PB_2014_FYDP"|input$Category2 == "PB_2014_FYDP" ){
      filter(Dataset3(),  FY >= 2014, FY <= 2018)}
    else{
      Dataset3()
    }})
  
  Dataset5 <- reactive({
    if(input$Category1 == "PB_2015_FYDP"|input$Category2 == "PB_2015_FYDP" ){
      filter(Dataset4(),  FY >= 2015, FY <= 2019)}
    else{
      Dataset4()
    }})
  
  Dataset6 <- reactive({
    if(input$Category1 == "PB_2016_FYDP"|input$Category2 == "PB_2016_FYDP" ){
      filter(Dataset5(),  FY >= 2016, FY <= 2020)}
    else{
      Dataset5()
    }})
  
  Dataset7 <- reactive({
    if(input$Category1 == "PB_2017_FYDP"|input$Category2 == "PB_2017_FYDP" ){
      filter(Dataset6(), FY >= 2017, FY <= 2021)}
    else{
      Dataset6()
    }})
  
  Dataset8 <- reactive({
    if(input$Category1 == "PB_2018_FYDP"|input$Category2 == "PB_2018_FYDP" ){
      filter(Dataset7(), FY >= 2018, FY <= 2022)}
    else{
      Dataset7() 
    }})
  
  Dataset9 <- reactive({
    if(input$Category1 == "PB_2019_FYDP"|input$Category2 == "PB_2019_FYDP" ){
      filter(Dataset7(), FY >= 2019, FY <= 2023)}
    else{
      Dataset8() 
    }})
  
  Dataset10 <- reactive({
    if(input$zero == "Yes"){

      filter_criteria <- lazyeval::interp(~ (x>0 & y >0), x = as.name(input$Category1), 
                                          y = as.name(input$Category2))
      
      filter_(Dataset9(), .dots = filter_criteria) %>%
        mutate(FY = as.factor(FY))}
    else{
      Dataset9() %>%
        mutate(FY = as.factor(FY))
    }})
  
  Dataset_2 <- reactive({mutate(Dataset(), FY = as.factor(FY))})
  
  output$tbl = DT::renderDataTable({
    if(input$view == "Standard"){
      datatable(
        Dataset_2()[,category()],
        filter = 'top', 
        width = '100%',
        colnames = c( "President's Budget Request" = "Budget.Year", "Fiscal Year" = "FY"),
        options = list("lengthChange" = TRUE,
                       "pageLength" = 10,
                       ordering = TRUE,
                       scrollX = TRUE,
                       scrolly=FALSE,
                       searchHighlight = TRUE,
                       autoWidth = FALSE),
        
        class = 'cell-border stripe'
        
      ) %>%

        formatStyle(
          'Amount',
          background = styleColorBar(Dataset()$Amount, '#63c5b8'),
          backgroundSize = '100% 75%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
          
        ) %>% 
        formatCurrency('Amount',currency = "$", interval = 3, mark = ",", digits = 0,
                       dec.mark = getOption("OutDec"))
    }
    else{
      datatable(
        Dataset10()[,category()] %>%
          
          na.omit(),
        filter = 'top', 
        width = '100%',
        colnames = c("Fiscal Year" = "FY"),
        options = list("lengthChange" = TRUE,
                       "pageLength" = 50,
                       ordering = TRUE,
                       scrollX = TRUE,
                       scrolly=FALSE,
                       searchHighlight = TRUE,
                       autoWidth = FALSE),
        
        
        class = 'cell-border stripe'
      ) %>%

        formatCurrency(c(input$Category1, input$Category2, 'Difference'), currency = "$", interval = 3, mark = ",", digits = 0,
                       dec.mark = getOption("OutDec")) %>%
        formatPercentage('Percent_Difference')
      
      
    }})

}

# --------------------------------------------------------------------------------
# start app 

shinyApp(ui= ui, server = server)