# ================================================================================
# Future Years Defense Program
# Designed and built by Gabriel Coll, Yuanjing Han, Loren Lipsey, Shivani Pandya,
# ...and Kayla Keller
# --------------------------------------------------------------------------------
# chart app: user interface and server
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(readr)
require(shiny)
require(ggplot2)
library(dplyr)
require(scales)
require(Cairo)
require(grid)
require(gridExtra)
library(forcats)
library(shinyBS)
library(shinyjs)
library(stringr)
Sys.setlocale("LC_ALL", "C")

# --------------------------------------------------------------------------------
# read data

Data <- readRDS("data/fydp_long (for chart app).rds")

# --------------------------------------------------------------------------------
# filter data

Data.r.a1 <-
  filter(Data, Account == "RDT&E" &
           Category2 == "Army - Basic Research")
Data.r.a2 <-
  filter(Data, Account == "RDT&E" &
           Category2 == "Army - Applied Research")
Data.r.a3 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Army - Advanced Technology Development")
Data.r.a4 <-
  filter(
    Data,
    Account == "RDT&E" &
      Category2 == "Army - Advanced Component Development and Prototypes"
  )
Data.r.a5 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Army - System Development and Demonstration")
Data.r.a6 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Army - RDT&E Management Support")
Data.r.a7 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Army - Operational Systems Development")

Data.p.a1 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Army - Aircraft")
Data.p.a2 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Army - Ammunition")
Data.p.a3 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Army - Missiles")
Data.p.a4 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Army - Other")
Data.p.a5 <-
  filter(Data,
         Account == "Procurement" & Category2 == "Army - Weapons/Vehicles")

Data.r.n1 <-
  filter(Data, Account == "RDT&E" &
           Category2 == "Navy - Basic Research")
Data.r.n2 <-
  filter(Data, Account == "RDT&E" &
           Category2 == "Navy - Applied Research")
Data.r.n3 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Navy - Advanced Technology Development")
Data.r.n4 <-
  filter(
    Data,
    Account == "RDT&E" &
      Category2 == "Navy - Advanced Component Development and Prototypes"
  )
Data.r.n5 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Navy - System Development and Demonstration")
Data.r.n6 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Navy - RDT&E Management Support")
Data.r.n7 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Navy - Operational Systems Development")

Data.p.n1 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Navy - Aircraft")
Data.p.n2 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Navy - Ammunition")
Data.p.n3 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Navy - Marine Corps")
Data.p.n4 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Navy - Other")
Data.p.n5 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Navy - Shipbuilding")
Data.p.n6 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Navy - Weapons")

Data.r.f1 <-
  filter(Data, Account == "RDT&E" &
           Category2 == "Air Force - Basic Research")
Data.r.f2 <-
  filter(Data,
         Account == "RDT&E" & Category2 == "Air Force - Applied Research")
Data.r.f3 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Air Force - Advanced Technology Development")
Data.r.f4 <-
  filter(
    Data,
    Account == "RDT&E" &
      Category2 == "Air Force - Advanced Component Development and Prototypes"
  )
Data.r.f5 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Air Force - System Development and Demonstration")
Data.r.f6 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Air Force - RDT&E Management Support")
Data.r.f7 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Air Force - Operational Systems Development")
Data.r.f7 <-
  filter(Data,
         Account == "RDT&E" &
           Category2 == "Air Force - Operational Systems Development")

Data.p.f1 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Air Force - Aircraft")
Data.p.f2 <-
  filter(Data,
         Account == "Procurement" & Category2 == "Air Force - Ammunition")
Data.p.f3 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Air Force - Missiles")
Data.p.f4 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Air Force - Other")
Data.p.f5 <-
  filter(Data, Account == "Procurement" &
           Category2 == "Air Force - Space")

# --------------------------------------------------------------------------------
# fct_drop

Data.r.a1$PT <- fct_drop(Data.r.a1$PT)
Data.r.a2$PT <- fct_drop(Data.r.a2$PT)
Data.r.a3$PT <- fct_drop(Data.r.a3$PT)
Data.r.a4$PT <- fct_drop(Data.r.a4$PT)
Data.r.a5$PT <- fct_drop(Data.r.a5$PT)
Data.r.a6$PT <- fct_drop(Data.r.a6$PT)
Data.r.a7$PT <- fct_drop(Data.r.a7$PT)


Data.p.a1$PT <- fct_drop(Data.p.a1$PT)
Data.p.a2$PT <- fct_drop(Data.p.a2$PT)
Data.p.a3$PT <- fct_drop(Data.p.a3$PT)
Data.p.a4$PT <- fct_drop(Data.p.a4$PT)
Data.p.a5$PT <- fct_drop(Data.p.a5$PT)

Data.r.n1$PT <- fct_drop(Data.r.n1$PT)
Data.r.n2$PT <- fct_drop(Data.r.n2$PT)
Data.r.n3$PT <- fct_drop(Data.r.n3$PT)
Data.r.n4$PT <- fct_drop(Data.r.n4$PT)
Data.r.n5$PT <- fct_drop(Data.r.n5$PT)
Data.r.n6$PT <- fct_drop(Data.r.n6$PT)
Data.r.n7$PT <- fct_drop(Data.r.n7$PT)

Data.p.n1$PT <- fct_drop(Data.p.n1$PT)
Data.p.n2$PT <- fct_drop(Data.p.n2$PT)
Data.p.n3$PT <- fct_drop(Data.p.n3$PT)
Data.p.n4$PT <- fct_drop(Data.p.n4$PT)
Data.p.n5$PT <- fct_drop(Data.p.n5$PT)
Data.p.n6$PT <- fct_drop(Data.p.n6$PT)

Data.r.f1$PT <- fct_drop(Data.r.f1$PT)
Data.r.f2$PT <- fct_drop(Data.r.f2$PT)
Data.r.f3$PT <- fct_drop(Data.r.f3$PT)
Data.r.f4$PT <- fct_drop(Data.r.f4$PT)
Data.r.f5$PT <- fct_drop(Data.r.f5$PT)
Data.r.f6$PT <- fct_drop(Data.r.f6$PT)
Data.r.f7$PT <- fct_drop(Data.r.f7$PT)

Data.p.f1$PT <- fct_drop(Data.p.f1$PT)
Data.p.f2$PT <- fct_drop(Data.p.f2$PT)
Data.p.f3$PT <- fct_drop(Data.p.f3$PT)
Data.p.f4$PT <- fct_drop(Data.p.f4$PT)
Data.p.f5$PT <- fct_drop(Data.p.f5$PT)

PT.r.a1 <- levels(as.factor(Data.r.a1$PT))
PT.r.a2 <- levels(as.factor(Data.r.a2$PT))
PT.r.a3 <- levels(as.factor(Data.r.a3$PT))
PT.r.a4 <- levels(as.factor(Data.r.a4$PT))
PT.r.a5 <- levels(as.factor(Data.r.a5$PT))
PT.r.a6 <- levels(as.factor(Data.r.a6$PT))
PT.r.a7 <- levels(as.factor(Data.r.a7$PT))

PT.p.a1 <- levels(as.factor(Data.p.a1$PT))
PT.p.a2 <- levels(as.factor(Data.p.a2$PT))
PT.p.a3 <- levels(as.factor(Data.p.a3$PT))
PT.p.a4 <- levels(as.factor(Data.p.a4$PT))
PT.p.a5 <- levels(as.factor(Data.p.a5$PT))

PT.r.n1 <- levels(as.factor(Data.r.n1$PT))
PT.r.n2 <- levels(as.factor(Data.r.n2$PT))
PT.r.n3 <- levels(as.factor(Data.r.n3$PT))
PT.r.n4 <- levels(as.factor(Data.r.n4$PT))
PT.r.n5 <- levels(as.factor(Data.r.n5$PT))
PT.r.n6 <- levels(as.factor(Data.r.n6$PT))
PT.r.n7 <- levels(as.factor(Data.r.n7$PT))

PT.p.n1 <- levels(as.factor(Data.p.n1$PT))
PT.p.n2 <- levels(as.factor(Data.p.n2$PT))
PT.p.n3 <- levels(as.factor(Data.p.n3$PT))
PT.p.n4 <- levels(as.factor(Data.p.n4$PT))
PT.p.n5 <- levels(as.factor(Data.p.n5$PT))
PT.p.n6 <- levels(as.factor(Data.p.n6$PT))

PT.r.f1 <- levels(as.factor(Data.r.f1$PT))
PT.r.f2 <- levels(as.factor(Data.r.f2$PT))
PT.r.f3 <- levels(as.factor(Data.r.f3$PT))
PT.r.f4 <- levels(as.factor(Data.r.f4$PT))
PT.r.f5 <- levels(as.factor(Data.r.f5$PT))
PT.r.f6 <- levels(as.factor(Data.r.f6$PT))
PT.r.f7 <- levels(as.factor(Data.r.f7$PT))

PT.p.f1 <- levels(as.factor(Data.p.f1$PT))
PT.p.f2 <- levels(as.factor(Data.p.f2$PT))
PT.p.f3 <- levels(as.factor(Data.p.f3$PT))
PT.p.f4 <- levels(as.factor(Data.p.f4$PT))
PT.p.f5 <- levels(as.factor(Data.p.f5$PT))

# --------------------------------------------------------------------------------
# create fields

Organization <- c("Army",
                  "Navy",
                  "Air Force")

Year <- c(
  "PB 2014 FYDP" ,
  "PB 2015 FYDP" ,
  "PB 2016 FYDP" ,
  "PB 2017 FYDP" ,
  "PB 2018 FYDP" ,
  "PB 2019 FYDP" ,
  "Actual"
)

Account <- c("Procurement",
             "RDT&E")

BSA <- levels(as.factor(Data$BSA))

PT <- levels(as.factor(Data$PT))

# --------------------------------------------------------------------------------
# create category field

Category2 <-
  c(
    "Air Force - Advanced Component Development and Prototypes",
    "Air Force - Advanced Technology Development",
    "Air Force - Aircraft",
    "Air Force - Ammunition",
    "Air Force - Applied Research",
    "Air Force - Basic Research",
    "Air Force - Missiles",
    "Air Force - Operational Systems Development",
    "Air Force - Other",
    "Air Force - RDT&E Management Support",
    "Air Force - Space",
    "Air Force - System Development and Demonstration",
    "Army - Advanced Component Development and Prototypes",
    "Army - Advanced Technology Development",
    "Army - Aircraft",
    "Army - Ammunition",
    "Army - Applied Research",
    "Army - Basic Research",
    "Army - Missiles",
    "Army - Operational Systems Development",
    "Army - Other",
    "Army - RDT&E Management Support",
    "Army - System Development and Demonstration",
    "Army - Weapons/Vehicles",
    "Navy - Advanced Component Development and Prototypes",
    "Navy - Advanced Technology Development",
    "Navy - Aircraft",
    "Navy - Ammunition",
    "Navy - Applied Research",
    "Navy - Basic Research",
    "Navy - Marine Corps",
    "Navy - Operational Systems Development",
    "Navy - Other",
    "Navy - RDT&E Management Support",
    "Navy - Sealift Fund",
    "Navy - Shipbuilding",
    "Navy - System Development and Demonstration",
    "Navy - Weapons"
  )

# --------------------------------------------------------------------------------
# begin ui section

ui <- fluidPage(
  tags$head(tags$style(
    HTML(".well{
         background-color: #FCFCFC;
         border-color: #FCFCFC;
         }")
)),

# --------------------------------------------------------------------------------
# import Google Font "Open Sans"

tags$style(
  HTML(
    "
    @import url('//fonts.googleapis.com/css?family=Source+Sans+Pro');
    
    body {
    font-family: 'Source Sans Pro',  sans-serif;
    font-weight: 500;
    line-height: 1.1;
    color: #554449;
    }
    
    "
  )
  ),

# --------------------------------------------------------------------------------
# app design style

(
  tags$style(
    ".help-block{color: #554449;
    font-size: 14px;
    font-style: normal;
    background-color: #fcfcfc;
    border-color: #C76363;
    border-style: solid;
    border-width: 4px;
    border-top: 4px #63c5b8;
    border-bottom: 4px #63c5b8;
    border-right: 4px #63c5b8;
    #border-left: 4px #63c5b8;
    border-radius: 1px
    }"
                      )
  ),

tags$style(
  type = "text/css",
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
),


tags$head(tags$style(
  HTML("body{background-color: #fcfcfc;}")
)),

# --------------------------------------------------------------------------------
# button style

tags$style(HTML(".btn {background-color: #4D7FA3}")),
tags$style(HTML(".btn {color: white}")),
tags$style(HTML(".btn {border-color: #FCFCFC}")),
tags$style(HTML(".btn:hover {border-color: #FCFCFC}")),
tags$style(HTML(".btn:hover {background-color: #2F4D63}")),
tags$style(HTML(".btn:hover {color: white}")),
tags$style(HTML(".btn:hover {font-weight: normal}")),
tags$style(HTML(
  ".btn:active:hover {background-color: #6BC6B5}"
)),

tags$style(HTML(
  ".popover({delay: {show: 500, hide: 100}})"
)),

tags$style(HTML(
  ".btn-primary {background-color: #BDD4DE}"
)),
tags$style(HTML(".btn-primary {border-color: #FCFCFC}")),
tags$style(HTML(".btn-primary {color: #554449}")),
tags$style(HTML(
  ".btn-primary:hover {background-color: #A5B9C2}"
)),
tags$style(HTML(".btn-primary:hover {color: #554449}")),
tags$style(
  HTML(".btn-primary:active:hover{background-color: #6BC6B5}")
),
tags$style(
  HTML(".btn-primary:active:focus{background-color: #90ABC2}")
),
tags$style(
  HTML(".btn-primary:dropdown-toggle {background-color: #BDD4DE}")
),
tags$style(HTML(".btn-basic {background-color: #BDD4DE}")),
tags$style(HTML(".btn-basic {border-color: #FCFCFC}")),
tags$style(HTML(
  ".btn-basic:hover {border-color: #FCFCFC}"
)),
tags$style(HTML(".btn-basic {color: #554449}")),
tags$style(HTML(
  ".btn-basic:hover {background-color: #A5B9C2}"
)),
tags$style(HTML(".btn-basic:hover {color: #FFFFFF}")),
tags$style(
  HTML(
    ".btn-basic:active,.open>.dropdown-toggle.btn-basic {background-color: #6BC6B5}"
  )
),

tags$style(
  HTML(".btn-basic:dropdown-toggle {background-color: #BDD4DE}")
),

# --------------------------------------------------------------------------------
# slider style

tags$style(HTML(".irs-bar {background: #63c5b8}")),
tags$style(HTML(".irs-bar {border-top: 1px #63c5b8}")),
tags$style(HTML(".irs-bar {border-bottom: 1px #63c5b8}")),
tags$style(HTML(
  ".irs-single, .irs-to, .irs-from {background: #628582}"
)),

tags$style(HTML(".irs-max {color: #554449}")),
tags$style(HTML(".irs-min {color: #554449}")),
tags$style(HTML(".irs-bar-edge {border: 1px #63c5b8}")),
tags$style(HTML(
  ".irs-bar-edge {border-color: 1px #63c5b8}"
)),
tags$style(HTML(
  ".irs-bar-edge {border-color: 1px #63c5b8}"
)),

# --------------------------------------------------------------------------------
# CSIS header

tags$div(
  HTML(
    "<div class='fusion-secondary-header'>
    <div class='fusion-row'>
    <div class='fusion-alignleft'><div class='fusion-contact-info'><center style=' padding:15px;'><a href='https://defense360.csis.org/content-type/data/' target='_blank'><img class='logo' src='https://defense360.csis.org/wp-content/uploads/2015/08/ISP_new.png' width='40%'></a></center><a href='mailto:'></a></div></div>
    </div>
    </div>"
  )
  ),
tags$style(
  HTML(
    ".fusion-secondary-header {border-bottom: 2.5px solid #6F828F}"
  )
),
br(),

# --------------------------------------------------------------------------------
# Google analytics script

tags$script(
  HTML(
    "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
    
    ga('create', 'UA-99363803-1', 'auto');
    ga('send', 'pageview')"
)
),

# --------------------------------------------------------------------------------
# begin side panel

fluidRow(
  column(
    4,
    shinyjs::useShinyjs(),
    id = "side-panel",
    
    # --------------------------------------------------------------------------------
    # info button
    
    bsButton(
      inputId = "info_btn",
      label = strong("Chart the FYDP >"),
      style = "default",
      type = "action",
      size = "small",
      block = TRUE
    ),
    
    bsModal("modalExample", trigger = "info_btn", htmlOutput("description")),
    
    br(),
    
    # --------------------------------------------------------------------------------
    # options button
    
    bsButton(
      "options_top_bottom",
      label = strong("More Options"),
      style = "basic",
      value = 0,
      type = "toggle",
      size = "small",
      block = TRUE
    ),
    
    # --------------------------------------------------------------------------------
    # options panel
    
    conditionalPanel(
      condition = "input.options_top_bottom == 1",
      
      sliderInput(
        'Yr',
        " ",
        min = 2012,
        max = 2023,
        value = c(2012, 2023),
        ticks = FALSE,
        step = 1,
        width = '100%',
        sep = ""
      ),
      
      radioButtons(
        "Chart",
        "Dollars",
        c("Then-Year", "Constant"),
        inline = TRUE,
        selected = "Then-Year",
        width = '100%'
      ),
      
      p(),
      
      radioButtons(
        "Axis",
        "Y-Axis Min",
        c("Zero", "Auto"),
        inline = TRUE,
        selected = "Zero"
      ),
      
      br(),
      
      downloadLink('CSVDownloadBtn',
                   "Download Displayed Data (csv)", class = NULL),
      align = "center",
      br()
      
    ),
    
    shinyjs::useShinyjs(),
    
    conditionalPanel(
      "input.view == 'impossible'",
      conditionalPanel("input.view == 'Standard View'",
                       selectizeInput(
                         "view", "",
                         c("Standard View",
                           "More Information")
                       ))
    ),
    
    br(),
    
    conditionalPanel(condition = "input.view == 'More Information'",
                     
                     hr(),
                     
                     helpText(
                       HTML(
                         "<div align = 'left'>",
                         "<h5> CSIS has embarked on a project to create a centralized FYDP database that is interpretable and universally accessible. This is part of a broader effort to create tools that make it easier to understand the defense budget and plan for defense investments.",
                         "<br/>",
                         "<br/>",
                         "This app includes FYDP data from the president's budget requests for FY 2014 to FY 2019, for the Procurement and Research, Development, Test and Evaluation (RDT&E) accounts. The data was mined from P-40s (for Procurement) and R-2s (for RDT&E), as well as from the DoD Greenbook and the OMB Budget Authority Database.",
                         "<br/>",
                         "<br/>",
                         "This is an ongoing project that we plan to build upon. If you have any questions or comments, please contact Gabriel Coll (gcoll@csis.org).",
                         "<br/>",
                         "</div>"
                         
                       )
                     ),
                     
                     align = "center"),
    
    conditionalPanel(
      condition = "input.view == 'Standard View'",
      bsButton(
        inputId = "info_btn",
        label = strong("Dive Deep"),
        style = "default",
        type = "toggle",
        size = "small",
        block = TRUE,
        width = '100%'
      ),
      align = 'center'
    ),
    
    br(),
    
    conditionalPanel(
      condition = "input.info_btn == 1 & input.view == 'Standard View'",
      bsButton(
        inputId = "info_btn2",
        label = (strong("Deeper")),
        style = "default",
        type = "toggle",
        size = "small",
        block = TRUE,
        width = '90%'
      ),
      align = 'center'
    ),
    
    br(),
    
    conditionalPanel(
      condition = "input.info_btn == 1 & input.info_btn2 == 1 & input.view == 'Standard View'",
      bsButton(
        inputId = "info_btn3",
        label = (strong("Deepest")),
        style = "default",
        type = "toggle",
        size = "small",
        block = TRUE,
        width = '80%'
      ),
      align = 'center'
    ),
    
    # --------------------------------------------------------------------------------
    # select buttons
    
    conditionalPanel(
      condition = "input.info_btn == 0 & input.view != 'Info'",
      
      selectInput(
        "Organization",
        "Organization",
        Organization,
        multiple = TRUE,
        selectize = FALSE,
        selected = Organization,
        # inline = TRUE,
        width = '90%'
      ),
      br(),
      selectInput(
        "Account",
        "Account",
        Account,
        multiple = TRUE,
        selectize = FALSE,
        selected = Account,
        # inline = TRUE,
        width = '90%'
      ),
      align = 'center'
    ),
    br(),
    conditionalPanel(condition = "input.view == 'Dark Matter'",
                     helpText(
                       HTML(
                         "<div align = 'center'>",
                         "<b> Background Information </b>",
                         "</div>",
                         "<br/>",
                         "<div align = 'left'>",
                         "P-40s (Procurement FYDP documents) and R-2s (RDT&E FYDP documents) provide some of the most comprehensive and detailed DoD data for the public. However, there are still discrepancies between aggregated P-40/R-2 numbers and the topline FYDP numbers available in the Greenbook.",
                         "<br/>",
                         "<br/>",
                         "This difference is primarily because of classified programs in future years and future programs that DoD is planning to fund, but isn't currently funding and therefore does not show in the P-40 and R-2 budget justification documents.",
                         "<br/>",
                         "<br/>",
                         "We refer to the discrepancy between these numbers as dark matter.",
                         "</div>"
                         
                       )
                     )),
    
    conditionalPanel(
      condition = "input.info_btn == 1 & input.info_btn2 == 0 & input.view == 'Standard View'",
      selectInput(
        "Category2",
        "Budget Activity",
        list(
          'RDT&E - Army' = c(
            "Army - Basic Research",
            "Army - Applied Research",
            "Army - Advanced Technology Development",
            "Army - Advanced Component Development and Prototypes",
            "Army - System Development and Demonstration",
            "Army - RDT&E Management Support",
            "Army - Operational Systems Development"
          ),
          
          'RDT&E - Navy' = c(
            "Navy - Basic Research",
            "Navy - Applied Research",
            "Navy - Advanced Technology Development",
            "Navy - Advanced Component Development and Prototypes",
            "Navy - System Development and Demonstration",
            "Navy - RDT&E Management Support",
            "Navy - Operational Systems Development"
          ),
          
          'RDT&E - Air Force' = c(
            "Air Force - Basic Research",
            "Air Force - Applied Research",
            "Air Force - Advanced Technology Development",
            "Air Force - Advanced Component Development and Prototypes",
            "Air Force - System Development and Demonstration",
            "Air Force - RDT&E Management Support",
            "Air Force - Operational Systems Development"
          ),
          
          'Procurement - Army' = c(
            "Army - Aircraft",
            "Army - Ammunition",
            "Army - Missiles",
            "Army - Other",
            "Army - Weapons/Vehicles"
          ),
          'Procurement - Navy' = c(
            "Navy - Aircraft",
            "Navy - Ammunition",
            "Navy - Marine Corps",
            "Navy - Other",
            "Navy - Shipbuilding",
            "Navy - Weapons"
          ),
          'Procurement - Air Force' = c(
            "Air Force - Aircraft",
            "Air Force - Ammunition",
            "Air Force - Missiles",
            "Air Force - Other",
            "Air Force - Space"
          )
        ),
        multiple = TRUE,
        selectize = FALSE,
        selected = Category2,
        width = '100%',
        size = 25
      ),
      align = "center"
    ),
    
    conditionalPanel(
      condition = "input.info_btn == 1 & input.info_btn2 == 1 & input.info_btn3 == 0
      & input.view == 'Standard View'",
      selectizeInput(
        "BSA",
        "Sub Activity",
        list('Procurement Only' = BSA),
        multiple = TRUE,
        # selectize = FALSE,
        selected = " ",
        width = '100%',
        size = 25
      ),
      align = "center"
    ),
    
    conditionalPanel(
      condition = "input.info_btn == 1 & input.info_btn2 == 1 & input.info_btn3 == 1
      & input.view == 'Standard View'",
      selectizeInput(
        "PT",
        "Line Item / Program Element",
        # PT,
        list(
          'RDT&E - Army - Basic Research' = PT.r.a1,
          'RDT&E - Army - Applied Research' = PT.r.a2,
          'RDT&E - Army - Advanced Technology Development' = PT.r.a3,
          'RDT&E - Army - Advanced Component Development and Prototypes' = PT.r.a4,
          'RDT&E - Army - System Development and Demonstration' = PT.r.a5,
          'RDT&E - Army - RDT&E Management Support' = PT.r.a6,
          'RDT&E - Army - Operational Systems Development' = PT.r.a7,
          'Procurement - Army - Aircraft' = PT.p.a1,
          'Procurement - Army - Ammunition' = PT.p.a2,
          'Procurement - Army - Missiles' = PT.p.a3,
          'Procurement - Army - Other' = PT.p.a4,
          'Procurement - Army - Weapons/Vehicles' = PT.p.a5,
          'RDT&E - Navy - Basic Research' = PT.r.n1,
          'RDT&E - Navy - Applied Research' = PT.r.n2,
          'RDT&E - Navy - Advanced Technology Development' = PT.r.n3,
          'RDT&E - Navy - Advanced Component Development and Prototypes' = PT.r.n4,
          'RDT&E - Navy - System Development and Demonstration' = PT.r.n5,
          'RDT&E - Navy - RDT&E Management Support' = PT.r.n6,
          'RDT&E - Navy - Operational Systems Development' = PT.r.n7,
          'Procurement - Navy - Aircraft' = PT.p.n1,
          'Procurement - Navy - Ammunition' = PT.p.n2,
          'Procurement - Navy - Marine Corps' = PT.p.n3,
          'Procurement - Navy - Other' = PT.p.n4,
          'Procurement - Navy - Shipbuilding' = PT.p.n5,
          'Procurement - Navy - Weapons' = PT.p.n6,
          'RDT&E - Air Force - Basic Research' = PT.r.f1,
          'RDT&E - Air Force - Applied Research' = PT.r.f2,
          'RDT&E - Air Force - Advanced Technology Development' = PT.r.f3,
          'RDT&E - Air Force - Advanced Component Development and Prototypes' = PT.r.f4,
          'RDT&E - Air Force - System Development and Demonstration' = PT.r.f5,
          'RDT&E - Air Force - RDT&E Management Support' = PT.r.f6,
          'RDT&E - Air Force - Operational Systems Development' = PT.r.f7,
          'Procurement - Air Force - Aircraft' = PT.p.f1,
          'Procurement - Air Force - Ammunition' = PT.p.f2,
          'Procurement - Air Force - Missiles' = PT.p.f3,
          'Procurement - Air Force - Other' = PT.p.f4,
          'Procurement - Air Force - Space' = PT.p.f5
        ),
        multiple = TRUE,
        # selectize = FALSE,
        selected = " ",
        width = '100%',
        size = 25
      ),
      align = "center"
    ),
    
    conditionalPanel(
      condition = "input.view == 'Standard View'",
      selectizeInput(
        "Checkbox",
        "Budget Year",
        Year,
        multiple = TRUE,
        # selectize = FALSE,
        selected = c(
          "PB 2014 FYDP" ,
          "PB 2015 FYDP" ,
          "PB 2016 FYDP" ,
          "PB 2017 FYDP" ,
          "PB 2018 FYDP" ,
          "PB 2019 FYDP" ,
          "Actual"
        ),
        # size = 7,
        width = '90%'
      ),
      align = "center"
    ),
    
    br(),
    
    
    conditionalPanel(
      condition = "input.info_btn != 3 & input.view == 'Standard View'",
      
      # --------------------------------------------------------------------------------
      # reset buttons
      
      bsButton(
        inputId = "reset_input",
        label = strong("Reset"),
        style = "primary",
        size = "small",
        width = '100%',
        block = TRUE
      ),
      align = 'center'
    )
    
  ),
  
  mainPanel(
    div(
      style = "position:relative",
      plotOutput(
        "plot",
        height = "500px",
        hover = hoverOpts(id = "plot_hover", delay = 30)
      ),
      uiOutput("hover_info"),
      
      conditionalPanel(condition = "input.info_btn != 3 & input.view == 'NaN'",
                       column(
                         3,
                         bsButton(
                           inputId = "Darkmatter",
                           label = strong("Dark Matter"),
                           style = "primary",
                           type = "toggle",
                           size = "default",
                           width = '100%',
                           block = TRUE
                         )
                       )),
      br(),
      br(),
      helpText(
        HTML(
          "     ",
          "The topline chart uses FYDP numbers from the DoD Greenbook (for PB 2014 to PB 2017, and PB 2019) and from the OMB Budget Authority Database (for PB 2018).",
          "<br>",
          "<br>",
          "     ",
          "The charts for budget activity, budget sub activity, and line item / program element use FYDP numbers from the P-40s (Procurement documents) and R-2s (RDT&E documents). The aggregation of these numbers will not add up the topline FYDP numbers. This is mainly because of classified programs in future years and future programs that DoD is planning to fund, but isn't currently funding.",
          "<br>",
          "<br>",
          "     ",
          "Right now, the app does not include the 'Defense-Wide' FYDP."
        )
      )
    )
  )
)

,
fluidRow(hr(),
         uiOutput("bottom_line"))

  )

# --------------------------------------------------------------------------------
# begin server section

server <- function(input, output, session) {
  # --------------------------------------------------------------------------------
  # read and clean data
  
  FullData <- readRDS("data/fydp_long (for chart app).rds")
  FullData2 <- read.csv("data/TOA_data.csv")
  
  FullData <- FullData %>%
    filter(!is.na(Year))
  
  FullData$Year <- as.character(FullData$Year)
  
  FullData2 <- FullData2 %>%
    filter(!is.na(Year))
  FullData2$Year <- as.character(FullData2$Year)
  FullData2$Year[FullData2$Year != "Actual"] <-
    paste("PB", FullData2$Year[FullData2$Year != "Actual"])
  FullData2 <- FullData2 %>%
    filter(FullData2$Year != "PB 2013 FYDP")
  
  formaty1 <- function(x) {
    x <- gsub("000", "", x)
    x <- gsub("500", ".5", x)
    x <- gsub("250", ".25", x)
    x <- gsub("750", ".75", x)
    paste("$", x, "B", sep = "")
  }
  
  label_one_value <- function(a_value, max_value, sig) {
    if (is.na(a_value))
      return(NULL)
    if (max_value > 1e7) {
      if (max_value > 1e11) {
        if (max_value > 1e13) {
          if (max_value > 1e15) {
            return(as.character(a_value))
          } else if (max_value > 1e14) {
            y_lab <- paste0("$", formatC(a_value / 1e12, max(sig, 0), format = "f"), "T")
          } else {
            y_lab <- paste0("$", formatC(a_value / 1e12, max(sig, 1), format = "f"), "T")
          }
        } else if (max_value > 1e12) {
          y_lab <- paste0("$", formatC(a_value / 1e12, max(sig, 2), format = "f"), "T")
        } else {
          y_lab <- paste0("$", formatC(a_value / 1e9, max(sig, 0), format = "f"), "B")
        }
      } else if (max_value > 1e9) {
        if (max_value > 1e10) {
          y_lab <- paste0("$", formatC(a_value / 1e9, max(sig, 1), format = "f"), "B")
        } else {
          y_lab <- paste0("$", formatC(a_value / 1e9, max(sig, 2), format = "f"), "B")
        }
      } else {
        if (max_value > 1e8) {
          y_lab <- paste0("$", formatC(a_value / 1e6, max(sig, 0), format = "f"), "M")
        } else {
          y_lab <- paste0("$", formatC(a_value / 1e6, max(sig, 1), format = "f"), "M")
        }
      }
    } else if (max_value > 1e3) {
      if (max_value > 1e5) {
        if (max_value > 1e6) {
          y_lab <- paste0("$", formatC(a_value / 1e6, max(sig, 2), format = "f"), "M")
        } else {
          y_lab <- paste0("$", formatC(a_value / 1e3, max(sig, 0), format = "f"), "k")
        }
      } else if (max_value > 1e4) {
        y_lab <- paste0("$", formatC(a_value / 1e3, max(sig, 1), format = "f"), "k")
      } else {
        y_lab <- paste0("$", formatC(a_value / 1e3, max(sig, 2), format = "f"), "k")
      }
    } else if (max_value > 10) {
      if (max_value > 100) {
        y_lab <- paste0("$", formatC(a_value, max(sig, 0), format = "f"))
      } else {
        y_lab <- paste0("$", formatC(a_value, max(sig, 1), format = "f"))
      }
    } else {
      y_lab <- paste0("$", formatC(a_value, max(sig, 2), format = "f"))
    }
    return(y_lab)
  }
  
  money_labels <- function(axis_values) {
    if (class(axis_values) == "character") {
      warning(
        paste(
          "money_labels() expects the axis to be a numeric variable",
          "but the axis is a character variable.  Coercing to numeric."
        )
      )
      axis_values <- as.numeric(axis_values)
    } else if (class(axis_values) != "numeric" &
               class(axis_values) != "integer") {
      stop(paste(
        "money_labels() expected a numeric axis, but got:",
        class(axis_values)
      ))
    }
    axis_range <-
      max(axis_values, na.rm = TRUE) - min(axis_values, na.rm = TRUE)
    sig_digits <-
      floor(log10(max(abs(axis_values), na.rm = TRUE))) -
      round(log10(axis_range))
    
    return(sapply(
      axis_values,
      label_one_value,
      max(abs(axis_values), na.rm = TRUE),
      sig_digits
    ))
  }
  
  # --------------------------------------------------------------------------------
  # subset data based on user input
  
  dataset <- reactive({
    # note: subset by year, based on year slider
    # ...input$Yr[1] is the user-selected minimum year
    # ...input$Yr[2] is the user-selected maximum year
    # ...as.numeric(levels(FY))[FY] is just FY, converted from a factor to
    # ...a numeric variable
    
    shown <- filter(FullData, FY >= input$Yr[1] & FY <= input$Yr[2])
    
    # note: subset data based on which categories the user selected ##
    # ...the selectInput widget holds the selected choices as a vector of
    # ...strings. This code checks whether the each observation is in the
    # ...selected categories, and discards it if isn't in all three.  The %in%
    # ...operator is a nice way to avoid typing lots of conditional tests all
    # ...strung together
    
    shown <- filter(shown,
                    Year %in% input$Checkbox)
    
    if (input$info_btn == 0) {
      shown <- filter(shown,
                      Organization %in% input$Organization &
                        Account %in% input$Account)
    } else
      shown <- shown
    
    if (input$info_btn == 1 & input$info_btn2 == 0) {
      shown <- filter(shown,
                      Category2 %in% input$Category2)
    } else
      shown <- shown
    
    if (input$info_btn == 1 &
        input$info_btn2 == 1 & input$info_btn3 == 0) {
      shown <- filter(shown,
                      BSA %in% input$BSA)
    } else
      shown <- shown
    
    
    if (input$info_btn == 1 &
        input$info_btn2 == 1 & input$info_btn3 == 1) {
      shown <- filter(shown,
                      PT %in% input$PT)
    } else
      shown <- shown
    
    
    # calculate percent of obligations for each VendorSize category
    shown <- shown %>%
      group_by(FY, Year) %>%
      summarise(Amount = sum(Amount, na.rm = TRUE))
    
    deflate <- c(
      "2010" = 0.8644,
      "2011" = 0.8820,
      "2012" = 0.8981,
      "2013" = 0.9133,
      "2014" = 0.9299,
      "2015" = 0.9411,
      "2016" = 0.9520,
      "2017" = 0.9684,
      "2018" = 0.9835,
      "2019" = 1.0000,
      "2020" = 1.0185,
      "2021" = 1.0386,
      "2022" = 1.0595,
      "2023" = 1.0808
    )
    
    if (input$Chart == "Constant") {
      shown$Amount <- (shown$Amount / deflate[as.character(shown$FY)])
    } else
      shown$Amount <- shown$Amount
    
    # return the subsetted dataframe to whatever called dataset()
    return(shown)
    
    # end of dataset() function
  })
  
  dataset2 <- reactive({
    # note: subset by year, based on year slider
    # ...input$Yr[1] is the user-selected minimum year
    # ...input$Yr[2] is the user-selected maximum year
    # ...as.numeric(levels(FY))[FY] is just FY, converted from a factor to
    # ...a numeric variable
    
    shown2 <-
      filter(FullData2, FY >= input$Yr[1] & FY <= input$Yr[2])
    
    # note: subset data based on which categories the user selected
    # ...the selectInput widget holds the selected choices as a vector of
    # ...strings. This code checks whether the each observation is in the
    # ...selected categories, and discards it if isn't in all three.  The %in%
    # ...operator is a nice way to avoid typing lots of conditional tests all
    # ...strung together
    
    shown2 <- filter(
      shown2,
      Organization %in% input$Organization &
        Account %in% input$Account &
        Year %in% input$Checkbox
    )
    
    
    shown2 <- shown2 %>%
      group_by(FY, Year) %>%
      summarise(
        Amount = sum(Amount, na.rm = TRUE),
        Amount2 = sum(Amount2, na.rm = TRUE)
      )
    
    deflate <- c(
      "2010" = 0.8644,
      "2011" = 0.8820,
      "2012" = 0.8981,
      "2013" = 0.9133,
      "2014" = 0.9299,
      "2015" = 0.9411,
      "2016" = 0.9520,
      "2017" = 0.9684,
      "2018" = 0.9835,
      "2019" = 1.0000,
      "2020" = 1.0185,
      "2021" = 1.0386,
      "2022" = 1.0595,
      "2023" = 1.0808
    )
    
    if (input$Chart == "Constant") {
      shown2$Amount <- (shown2$Amount / deflate[as.character(shown2$FY)])
      shown2$Amount2 <-
        (shown2$Amount2 / deflate[as.character(shown2$FY)])
    } else {
      shown2$Amount <- shown2$Amount
      shown2$Amount2 <- shown2$Amount2
    }
    return(shown2)
  })
  
  # --------------------------------------------------------------------------------
  # plot
  
  plotsettings2 <- reactive({
    p <- ggplot(data = dataset2(),
                aes(
                  x = FY,
                  y = Amount2,
                  color = Year,
                  group = Year,
                  linetype = Year
                )) +
      geom_line(size = 1.5) +
      ggtitle("Comparison of Future Years Defense Programs \n to actual funding") +
      
      scale_linetype_manual(
        values = c(
          "PB 2013 FYDP" = "longdash",
          "PB 2014 FYDP" =  "longdash",
          "PB 2015 FYDP" = "longdash",
          "PB 2016 FYDP" = "longdash",
          "PB 2017 FYDP" = "longdash",
          "PB 2018 FYDP" = "longdash",
          "PB 2019 FYDP" = "longdash",
          "Actual" = "solid"
        )
      ) +
      
      scale_color_manual(
        values = c(
          "PB 2013 FYDP" = "#C74F4F",
          "PB 2014 FYDP" =  "#C74F4F",
          "PB 2015 FYDP" = "#5F597C",
          "PB 2016 FYDP" = "#599a9e",
          "PB 2017 FYDP" = "#84B564",
          "PB 2018 FYDP" = "#CE884E",
          "PB 2019 FYDP" = "#E36D60",
          "Actual" = "#554449"
        )
      ) +
      
      theme(
        plot.title = element_text(
          family = "Arial",
          color = "#554449",
          size = 20,
          face = "bold",
          margin = margin(20, 0, 30, 0),
          hjust = 0.5
        )
      ) +
      
      theme(
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#FCFCFC"),
        plot.background = element_rect(fill = "#FCFCFC", color = "#FCFCFC"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "lightgray"),
        panel.grid.minor.y = element_line(size = .1, color = "lightgray")
      ) +
      
      scale_x_continuous(
        breaks = seq(input$Yr[1], input$Yr[2], by = 1),
        labels = function(x) {
          substring(as.character(x), 3, 4)
        }
      ) +
      
      theme(legend.position = "right") +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(size = 14, color = "#554449")) +
      theme(legend.text = element_text(size = 14, color = "#554449")) +
      theme(legend.key = element_rect(fill = "#fcfcfc", color = "#fcfcfc")) +
      theme(legend.background = element_rect(fill = "#fcfcfc")) +
      theme(legend.key.width = unit(3, "line")) +
      theme(axis.text.x = element_text(
        size = 12,
        color = "#554449",
        margin = margin(-5, 0, 0, 0)
      )) +
      theme(axis.ticks.length = unit(.00, "cm")) +
      theme(axis.text.y = element_text(
        size = 12,
        color = "#554449",
        margin = margin(0, 5, 0, 0)
      )) +
      theme(axis.title.x = element_text(
        size = 14,
        face = "bold",
        color = "#554449",
        margin = margin(15, 0, 0, 0)
      )) +
      theme(axis.title.y = element_text(
        size = 14,
        face = "bold",
        color = "#554449",
        margin = margin(0, 15, 0, 0)
      )) +
      
      
      xlab("Fiscal Year") +
      ylab((switch(
        input$Chart,
        Constant = "Constant FY19 Dollars",
        'Then-Year' = "Then-Year Dollars"
      ))) +
      
      theme(plot.caption = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Source Sans Pro"
      )) +
      
      labs(caption = "Source: Future Years Defense Program; CSIS analysis",
           size = 30,
           family = "Source Sans Pro") +
      
      if (input$Axis == "Zero") {
        scale_y_continuous(
          breaks = scales::pretty_breaks(n = 8),
          labels = money_labels,
          limits = c(0, NA)
        )
      } else if (input$Axis == "Auto") {
        scale_y_continuous(breaks = scales::pretty_breaks(n = 8),
                           labels = money_labels)
      } else
        scale_y_continuous(
          breaks = scales::pretty_breaks(n = 8),
          labels = money_labels,
          limits = c(0, NA)
        )
    
    p
  })
  
  plotsettings3 <-   reactive({
    df <- reactive({
      filter(dataset2(),
             Year %in% input$PT2)
    })
    
    # --------------------------------------------------------------------------------
    # plot
    
    p <-
      ggplot(data = df(), aes(
        x = FY,
        color = Year,
        group = Year
      )) +
      geom_line(data = df(),
                aes(
                  x = FY,
                  y = Amount,
                  linetype = "P-40 / R-2"
                ),
                size = 1.5) +
      geom_line(data = df(),
                aes(
                  x = FY,
                  y = Amount2,
                  linetype = "Topline"
                ),
                size = 1.5) +
      scale_linetype_manual(values = c("P-40 / R-2" = "dashed", "Topline" = "F1")) +
      scale_color_manual(
        values = c(
          "PB 2013 FYDP" = "#C74F4F",
          "PB 2014 FYDP" =  "#C74F4F",
          "PB 2015 FYDP" = "#5F597C",
          "PB 2016 FYDP" = "#599a9e",
          "PB 2017 FYDP" = "#84B564",
          "PB 2018 FYDP" = "#CE884E",
          "PB 2019 FYDP" = "#5A869C",
          "Actual" = "#554449"
        )
      ) +
      geom_ribbon(data = df(),
                  aes(ymin = Amount, ymax = Amount2),
                  alpha = 0.75) +
      ggtitle("Comparison of Future Years Defense Programs to actual funding") +
      
      
      theme(
        plot.title = element_text(
          family = "Arial",
          color = "#554449",
          size = 20,
          face = "bold",
          margin = margin(20, 0, 30, 0),
          hjust = 0.5
        )
      ) +
      
      theme(
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#FCFCFC"),
        plot.background = element_rect(fill = "#FCFCFC", color = "#FCFCFC"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "lightgray"),
        panel.grid.minor.y = element_line(size = .1, color = "lightgray")
      ) +
      
      scale_x_continuous(
        breaks = seq(input$Yr[1], input$Yr[2], by = 1),
        labels = function(x) {
          substring(as.character(x), 3, 4)
        }
      ) +
      
      theme(legend.position = "right") +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(size = 12, color = "#554449")) +
      theme(legend.text = element_text(size = 12, color = "#554449")) +
      theme(legend.key = element_rect(fill = "#fcfcfc", color = "#fcfcfc")) +
      theme(legend.background = element_rect(fill = "#fcfcfc")) +
      theme(legend.key.width = unit(3, "line")) +
      theme(axis.text.x = element_text(
        size = 12,
        color = "#554449",
        margin = margin(-5, 0, 0, 0)
      )) +
      theme(axis.ticks.length = unit(.00, "cm")) +
      theme(axis.text.y = element_text(
        size = 12,
        color = "#554449",
        margin = margin(0, 5, 0, 0)
      )) +
      theme(axis.title.x = element_text(
        size = 14,
        face = "bold",
        color = "#554449",
        margin = margin(15, 0, 0, 0)
      )) +
      theme(axis.title.y = element_text(
        size = 14,
        face = "bold",
        color = "#554449",
        margin = margin(0, 15, 0, 0)
      )) +
      
      xlab("Fiscal Year") +
      ylab((switch(
        input$Chart,
        Constant = "Constant Dollars",
        'Then-Year' = "Then-Year Dollars"
      ))) +
      
      theme(plot.caption = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Source Sans Pro"
      )) +
      labs(caption = "Source: Future Years Defense Program; CSIS analysis",
           size = 30,
           family = "Source Sans Pro") +
      if (input$Axis == "Zero") {
        scale_y_continuous(
          breaks = scales::pretty_breaks(n = 8),
          labels = money_labels,
          limits = c(0, NA)
        )
      } else if (input$Axis == "Auto") {
        scale_y_continuous(breaks = scales::pretty_breaks(n = 8),
                           labels = money_labels)
      } else
        scale_y_continuous(
          breaks = scales::pretty_breaks(n = 8),
          labels = money_labels,
          limits = c(0, NA)
        )
    
    p
    
  })
  
  # --------------------------------------------------------------------------------
  # plot
  
  plotsettings5 <- reactive({
    p <- ggplot(data = dataset(),
                aes(
                  x = FY,
                  y = Amount,
                  color = Year,
                  group = Year,
                  linetype = Year
                )) +
      geom_line(size = 1.5) +
      ggtitle("Comparison of Future Years Defense Programs to actual funding") +
      
      scale_linetype_manual(
        values = c(
          "PB 2013 FYDP" = "longdash",
          "PB 2014 FYDP" = "longdash",
          "PB 2015 FYDP" = "longdash",
          "PB 2016 FYDP" = "longdash",
          "PB 2017 FYDP" = "longdash",
          "PB 2018 FYDP" = "longdash",
          "PB 2019 FYDP" = "longdash",
          "Actual" = "solid"
        )
      ) +
      
      scale_color_manual(
        values = c(
          "PB 2013 FYDP" = "#C74F4F",
          "PB 2014 FYDP" =  "#C74F4F",
          "PB 2015 FYDP" = "#5F597C",
          "PB 2016 FYDP" = "#599a9e",
          "PB 2017 FYDP" = "#84B564",
          "PB 2018 FYDP" = "#CE884E",
          "PB 2019 FYDP" = "#5A869C",
          "Actual" = "#554449"
        )
      ) +
      
      theme(
        plot.title = element_text(
          family = "Arial",
          color = "#554449",
          size = 20,
          face = "bold",
          margin = margin(20, 0, 30, 0),
          hjust = 0.5
        )
      ) +
      
      theme(
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#FCFCFC"),
        plot.background = element_rect(fill = "#FCFCFC", color = "#FCFCFC"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "lightgray"),
        panel.grid.minor.y = element_line(size = .1, color = "lightgray")
      ) +
      
      scale_x_continuous(
        breaks = seq(input$Yr[1], input$Yr[2], by = 1),
        labels = function(x) {
          substring(as.character(x), 3, 4)
        }
      ) +
      
      theme(legend.position = "right") +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(size = 14, color = "#554449")) +
      theme(legend.text = element_text(size = 14, color = "#554449")) +
      theme(legend.key = element_rect(fill = "#fcfcfc", color = "#fcfcfc")) +
      theme(legend.background = element_rect(fill = "#fcfcfc")) +
      theme(legend.key.width = unit(3, "line")) +
      theme(axis.text.x = element_text(
        size = 12,
        color = "#554449",
        margin = margin(-5, 0, 0, 0)
      )) +
      theme(axis.ticks.length = unit(.00, "cm")) +
      theme(axis.text.y = element_text(
        size = 12,
        color = "#554449",
        margin = margin(0, 5, 0, 0)
      )) +
      theme(axis.title.x = element_text(
        size = 14,
        face = "bold",
        color = "#554449",
        margin = margin(15, 0, 0, 0)
      )) +
      theme(axis.title.y = element_text(
        size = 14,
        face = "bold",
        color = "#554449",
        margin = margin(0, 15, 0, 0)
      )) +
      
      xlab("Fiscal Year") +
      ylab((switch(
        input$Chart,
        Constant = "Constant FY19 Dollars",
        'Then-Year' = "Then-Year Dollars"
      ))) +
      
      theme(plot.caption = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Source Sans Pro"
      )) +
      labs(caption = "Source: Future Years Defense Program; CSIS analysis",
           size = 30,
           family = "Source Sans Pro") +
      if (input$Axis == "Zero") {
        scale_y_continuous(
          breaks = scales::pretty_breaks(n = 8),
          labels = money_labels,
          limits = c(0, NA)
        )
      } else if (input$Axis == "Auto") {
        scale_y_continuous(breaks = scales::pretty_breaks(n = 8),
                           labels = money_labels)
      } else
        scale_y_continuous(
          breaks = scales::pretty_breaks(n = 8),
          labels = money_labels,
          limits = c(0, NA)
        )
    p
  })
  
  
  # --------------------------------------------------------------------------------
  # output the built plot and start the app
  
  output$plot <- renderPlot({
    if (input$view != "Dark Matter" & input$info_btn == 0) {
      plotsettings2()
    }
    else if (input$info_btn == 1) {
      plotsettings5()
    }
    
    else{
      plotsettings3()
    }
  }, height = 500)
  
  output$CSVDownloadBtn <- downloadHandler(
    filename = paste('CSIS-FYDP-', Sys.Date(), '.csv', sep = ''),
    content = function(file) {
      if (input$view == "Standard View" & input$info_btn == 0) {
        writedata <- dataset2()
        colnames(writedata)[colnames(writedata) == "Year"] <-
          "Budget.Year"
        colnames(writedata)[colnames(writedata) == "Amount"] <-
          "P-40.R-2.Amount"
        colnames(writedata)[colnames(writedata) == "Amount2"] <-
          "Topline Amount"
      }
      else if (input$view == "Standard View") {
        writedata <- dataset()
        colnames(writedata)[colnames(writedata) == "Year"] <-
          "Budget.Year"
        colnames(writedata)[colnames(writedata) == "Amount"] <-
          "P-40.R-2.Amount"
      }
      write.csv(writedata, file, row.names = FALSE)
    }
  )
  
  output$description <- renderUI({
    HTML(
      "<div align = 'center'>",
      "<b> Chart the FYDP </b>",
      "<hr>",
      "<h5><b> Designed and built by Gabriel Coll, Loren Lipsey, Yuanjing Han, Shivani Pandya, and Kayla Keller </b></h4>",
      "<hr>",
      "</div>",
      "<div align = 'left'>",
      "<h5> The Future Years Defense Program (FYDP) is a five-year plan for the Department of Defense (DoD) released most years with the President's budget request. This valuable resource provides budget projections down to the project level, thereby making it possible to analyze DoD's plans for future investments and to assess how these investments support U.S. defense strategy. However, the FYDP data released publicly by DoD is difficult to analyze because it is buried in hundreds of separate PDF documents.</h5>",
      "<h5> CSIS has embarked on a project to create a centralized FYDP database and series of apps. We have designed these apps for outside analysts and congressional staffers, and we are making them available to anyone. This is part of a broader effort to create tools that make it easier to understand and plan for defense investments.</h5>",
      "<h5> These apps include historical FYDP data from the President's budget requests for FY 2014 through FY 2019, for Procurement and Research, Development, Test and Evaluation (RDT&E) accounts, and for Army, Navy, and Air Force accounts. Our sources include the P-40 (for procurement) and R-2 (for RDT&E) documents within the DoD justification books, as well as the DoD Greenbook and the OMB budget database.</h5>",
      "<h5> This is an ongoing project that we plan to build upon, so bookmark the site and check back in the coming weeks and months as more data and features are added. If you have any questions or comments, please contact Gabriel Coll (gcoll@csis.org). </h5>",
      "</div>"
      
    )
  })
  
  output$bottom_line <- renderUI({
    HTML(
      paste0(
        "<div align = 'left'>",
        "<h5> For more on our work, contact <a href='mailto:gcoll@csis.org?subject=Build%20your%20own%20Navy'>Gabriel Coll</a>. This is an ongoing project, so we welcome any recommendations. Learn more about our <a href=https://docs.google.com/spreadsheets/d/1C9AUqJwjD_1UbB9M9zHxmcbsQXWGtmc_CWBuglN4WJQ/edit?usp=sharing>numbers and sources</a> here. You can also visit the fydp <a href=https://analytics.csis.org/fydp/table>table app</a>, and find more apps on <a href=https://defense360.csis.org>Defense360</a>.</h4>"
      )
    )
  })
  
  # --------------------------------------------------------------------------------
  # give details when user hovers the plot
  
  # note: see https://gitlab.com/snippets/16220
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    
    if (is.null(hover))
      return(NULL)
    
    if (input$view == "Dark Matter") {
      if ("P-40/R-2 with classified" %in% input$Display) {
        data <- reactive({
          filter(dataset3(),
                 Year %in% input$PT2)
        })
      }
      else if ("P-40/R-2" %in% input$Display) {
        data <- reactive({
          filter(dataset2(),
                 Year %in% input$PT2)
        })
      }
    }
    else if (input$info_btn == 1) {
      data <- reactive({
        dataset()
      })
    }
    else{
      data <- reactive({
        dataset2()
      })
    }
    
    if (input$info_btn == 1) {
      switch(input$Chart,
             "Constant" = {
               point <- nearPoints(
                 data(),
                 hover,
                 xvar = "FY",
                 yvar = "Amount",
                 threshold = 200,
                 maxpoints = 1,
                 addDist = TRUE
               )
             },
             "Then-Year" = {
               point <- nearPoints(
                 data(),
                 hover,
                 xvar = "FY",
                 yvar = "Amount",
                 threshold = 200,
                 maxpoints = 1,
                 addDist = TRUE
               )
             })
      
    }
    else if (input$info_btn == FALSE) {
      switch(input$Chart,
             "Constant" = {
               point <- nearPoints(
                 data(),
                 hover,
                 xvar = "FY",
                 yvar = "Amount2",
                 threshold = 200,
                 maxpoints = 1,
                 addDist = TRUE
               )
             },
             "Then-Year" = {
               point <- nearPoints(
                 data(),
                 hover,
                 xvar = "FY",
                 yvar = "Amount2",
                 threshold = 200,
                 maxpoints = 1,
                 addDist = TRUE
               )
             })
    }
    
    if (nrow(point) == 0)
      return(NULL)
    year <- round(hover$x)
    if (year < input$Yr[1] | year > input$Yr[2])
      return(NULL)
    if (hover$y < 0)
      return(NULL)
    
    
    if (is.null(input$view == "Dark Matter")) {
      year <- point$FY
      hov_amount <- point$Amount
      
      if (hover$y < hov_amount)
        return(NULL)
    }
    
    else if (input$view == "Dark Matter") {
      FYDPYear <- input$PT2
      year <- point$FY
      hov_amount <- point$Amount
      hov_amount2 <- point$Amount2
    }
    
    
    # note: calculate point position INSIDE the image as percent of total dimensions
    # ...from left (horizontal) and from top (vertical)
    
    left_pct <- (hover$x - hover$domain$left) /
      (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) /
      (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct *
      (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct *
      (hover$range$bottom - hover$range$top)
    
    # Use HTML/CSS to change style of tooltip panel here
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:",
      left_px + 2,
      "px; top:",
      top_px + 2,
      "px;"
    )
    if (input$view == "Dark Matter") {
      wellPanel(style = style,
                p(HTML(
                  paste0(
                    "<b> Budget Year: </b>",
                    FYDPYear,
                    "<br/>",
                    "<b> Fiscal Year: </b>",
                    year,
                    "<br/>",
                    # "<b> Share: </b>", round(hov_percent*100,1), "%<br/>",
                    "<b> Topline Amount: </b> ",
                    label_one_value(hov_amount2, hov_amount2, 0),
                    br(),
                    "<b> P-40/R-2 Amount: </b>",
                    label_one_value(hov_amount, hov_amount, 0),
                    br(),
                    "<b> Difference: </b>",
                    label_one_value(
                      abs(hov_amount2 - hov_amount),
                      abs(hov_amount2 - hov_amount),
                      0
                    )
                  )
                )))
    }
    else if (input$view == "Standard View" & input$info_btn == 0) {
      wellPanel(style = style,
                p(HTML(
                  paste0(
                    "<b> Budget Year: </b>",
                    point$Year,
                    "<br/>",
                    "<b> Fiscal Year: </b>",
                    year,
                    "<br/>",
                    "<b> Topline Amount: </b> ",
                    label_one_value(point$Amount2, point$Amount2, 0)
                  )
                )))
    }
    else{
      wellPanel(style = style,
                p(HTML(
                  paste0(
                    "<b> Budget Year: </b>",
                    point$Year,
                    "<br/>",
                    "<b> Fiscal Year: </b>",
                    year,
                    "<br/>",
                    "<b> P-40/R-2 Amount: </b> ",
                    label_one_value(point$Amount, point$Amount, 0)
                  )
                )))
    }
  })
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  })
  
}

# --------------------------------------------------------------------------------
# start app

shinyApp(ui = ui, server = server)

# ================================================================================