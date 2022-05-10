# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(
    titleWidth = 250
  ),
  
  ########### sidebard ###################
  dashboardSidebar(
    width=250,
    tags$style(HTML(".sidebar-menu { margin: 28px; }")),
    tags$style(HTML(".sidebar-menu li { width: 220px; }")),
    tags$style(HTML(".sidebar-menu li a{ font-size: 20px; }")),
    tags$style(HTML(".sidebar-menu li>a>.fa-angle-left, .sidebar-menu li>a>.pull-right-container>.fa-angle-left{ margin-right: -30px }")),
    tags$style(HTML("section.sidebar .shiny-input-container {padding-left:1px}")),
    tags$head(tags$style(HTML('.modal-body {width: 800px;}'))),
    tags$style(HTML(".dropdown-menu {width: 198px;}")),
    tags$style(HTML(".selectize-input {outline:key-selectized;max-height: 100px; overflow-y:scroll; }")),
    tags$head(tags$style(HTML('div.box-header {text-align:center;}'))),
    tags$head(tags$style(HTML('div.dt-button-collection>ul.dropdown-menu {
      max-height: 200px;
      overflow-y: scroll;
    } '))),
    
     
    tags$head(tags$style(HTML('.Table th {
          position: sticky;
          top: 0;
          z-index: 999;
        }'))),
    useShinyjs(),
    tags$head(
      tags$style(HTML(".sidebar {
                      height: 90vh; overflow-y: auto;
                    }"
      ) # close HTML 
      
      )            # close tags$style
    ), 
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Select Extract",selected = T,
               tabName = "extract",
               icon=icon("check-square"),
               selectInput(inputId = "sel_extract", label="Select Extract",choices = NULL,selectize = FALSE)),
      menuItem("Upload Data",selected = TRUE,
               tabName = "upload",
               icon=icon("upload"),
               FileInputUI("files")        
      ),
      menuItem("Select Subject",
               tabName = "subj",
               # selectInput(
               #     inputId = "sel_subj",
               #     label="Select Subject",
               #     selectize = TRUE,
               #     multiple = TRUE,
               #     choices="")
               pickerInput("sel_subj","Select Subject",
                           choices="", options = list(`actions-box` = TRUE),multiple = T)
      ),
      
      menuItem("Combine",
               tabName = "comb",
               selectInput(
                 inputId = "comb1",
                 label="",
                 choices=""),
               selectInput(
                 inputId = "join_type",
                 label="",
                 choices=""
               ),
               selectInput(
                 inputId = "comb2",
                 label="",
                 choices=""),
               selectInput(
                 inputId = "key",
                 label="",
                 choices="",
                 multiple = TRUE,
                 selectize = TRUE
               ),
               actionButton("go","Combine")
               
      ),
      menuItem("Main",
               tabName = "main"
      ),
      menuItem("Pivot",
               tabName = "pivot"
      )#,
      # menuItem("AE",
      #          tabName = "AE")
    )),
  ########## Body ####################
  dashboardBody(
    useShinyjs(),
    shinyjs::hidden(
      div(id='main',
          conditionalPanel(condition = "output.setupComplete",
                           actionButton("remove", "Remove selected tab"),
                           actionButton("downBT", "Download"),
                           tabsetPanel(id = "tabs",
                                       tabPanel(
                                         title = "Main Dashboard",
                                         value = "page1",
                                         fluidRow(
                                           p("Click on a dataset in the plot to see the details"),
                                           highchartOutput("bars",height = 600)),
                                         fluidRow(
                                           column(width = 6,
                                                  DT::dataTableOutput("listing_variables")
                                           )
                                         )
                                       )
                                       
                           )
          ))),
    
    shinyjs::hidden(
      div(id='pivot',
          uiOutput("pivotts")
      )),

    shinyjs::hidden(
      div(id='AE',
          timevisOutput("timeline"))
    )
    
    
  )        
)