shinyServer(function(input, output, session){ 
  #variable used for showing loading box
  rv<- reactiveValues()
  rv$setupComplete<-FALSE
  
  #keep track of tabs
  tab_list<- NULL
  
  #keep track of combined tables
  comb_list<-reactiveValues()
  
  #modal for download
  myModal<- function(filenam){
    div(id = "test2",
        modalDialog(
          textInput(inputId = "save_name",label = "",value=filenam,placeholder = "Name of file"),
          downloadButton("save_state", "Save"),easyClose = TRUE, title="Dowload Combined Table",
          footer = tagList(actionButton("close", "OK"))
        )
    )
  }
  
  observeEvent(input$close,{
    removeModal()
  })
  
  #select extract
  observe({
    l1<-dir(paste0("Data/"),include.dirs = T)%>%str_match("^[0-9]+")%>%is.na()
    l2<-dir(paste0("Data/"),include.dirs = T)[!l1]
    updateSelectInput(session, "sel_extract",choices=c(" ",l2))
  })
  
  
  #upload file
  userFile<-callModule(FileInput,"files")
  
  observeEvent(userFile(),{
    ################browser()
    l1<-dir(paste0("Data/"),include.dirs = T)%>%str_match("^[0-9]+")%>%is.na()
    l2<-dir(paste0("Data/"),include.dirs = T)[!l1]
    updateSelectInput(session, "sel_extract",choices=c(" ",l2),selected = userFile()[[2]])
  })
  
  files<-reactive({
    ###########browser()
    #browser()
    if(!is.null(input$sel_extract) & hasValue(input$sel_extract)&length(dir(paste0("Data/",input$sel_extract),include.dirs = T))>0 )
    {
      rds <-try(readRDS(file=paste0("Data/",input$sel_extract,'/','ads.RDS')))
    }else{
      userFile()[[1]]
      return(userFile()[[1]])
    }
    
  })
  
  
  #### select date ####
  dd<-reactiveValues(l=NULL,chosen=NULL)
  observe({
    # #################browser()
    # input$sel_extract
    if(!is.null(input$sel_extract) & hasValue(input$sel_extract))
    {
      dd$l<-input$sel_extract
    }
    
  })
  
  observeEvent(dd$l,{
    #browser()
    if(is.data.frame(files()))
    {
      nam<-userFile()$name
    }else{
      nam<-names(files()) 
    }

  })
  #select subject
  observeEvent(list(files(),input$sel_extract),{
    #browser()
    if(length(files())>0 & nrow(files()$dm)>0){
      subj<-files()$dm[grep("^usubj",names(files()$dm),ignore.case = T,value = T)]%>%pull()%>%unique()
       # updateSelectInput(session, "sel_subj",choices=c(subj),selected = " ")
      updatePickerInput(session = session, inputId = "sel_subj",
                        choices = subj,selected = subj)
    }
  })
  
  
  observeEvent(input$sel_extract,{
    if(hasValue(input$sel_extract))
    {
      rv$setupComplete<-TRUE
      
      output$setupComplete<- reactive({
        return(rv$setupComplete)
      })
      
      outputOptions(output,'setupComplete',suspendWhenHidden=FALSE)
    }
    
  })
  
  js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")
  
  subj<-reactiveValues(subj=NULL)
  
  chart_data<-reactive({
    ####browser()
    if(length(input$sel_subj)>0 & all(!input$sel_subj %in% c(""," ",NULL))){subj$subj<-input$sel_subj}else{subj$subj<-files()$adsl.sas7bdat$USUBJID%>%unique()}
    if(length(subj$subj)>0)
    {
      #add column of dataset name
      aa<-mapply(cbind, files(), "datasets"=names(files()), SIMPLIFY=F)
      #### filtered data summary ####
      bb<-map(aa,filt1,subj$subj)
      
      filtered_data_summ=bb%>%bind_rows()
      
      #### filtered data ####
      bb2<-map(files(),filt2,subj$subj)
      filtered_data<-bb2
      
      return(list(filtered_data_summ,filtered_data))
    } 
  })
  
  #combine based on selected subjects
  observe({
    ##########browser()
    updateSelectInput(session, "comb1",label="dataset1",choices= chart_data()[[1]]$datasets,selected = " ")
    updateSelectInput(session, "comb2",label="dataset2",choices= chart_data()[[1]]$datasets,selected = " ")
    
    comb_type <- c(
      "inner_join",  "left_join", "right_join", "full_join","semi_join", "anti_join"
    )
    
    #clean_type<- gsub("_"," ",comb_type)
    updateSelectInput(session,"join_type",label="Combination Type",choices = comb_type)
  })
  
  #keyss<-reactiveValues(key=NULL)
  observe({
    #browser()
    one<-chart_data()[[2]][[input$comb1]]
    two<-chart_data()[[2]][[input$comb2]]
    
    #common keys
    k<-c(names(one),names(two)) 
    k_unique<-k%>%unique()
    keyss<-k[duplicated(k)]
    
    #check type compatibility
    one_class<-sapply(one[,keyss], class)%>%as.data.frame(stringsAsFactors = F)
    two_class<-sapply(two[,keyss], class)%>%as.data.frame(stringsAsFactors = F)
    bbb=as.data.frame(one_class==two_class)%>%slice(1)
    keysClean<-apply(bbb, 1, function(i) names(bbb)[i])%>%as.vector()

    keyss<-keysClean
    
    updateSelectInput(session,"key",label="Matching Keys",choices = k_unique,selected = keyss)
    
  })
  
  observeEvent(input$sidebar_tabs,{
    #########browser()
    toggle("main",condition=input$sidebar_tabs=="main")
    toggle("pivot",condition=input$sidebar_tabs=="pivot")
    toggle("AE", condition = input$sidebar_tabs=="AE")
    shinyjs::hide(selector="ul.menu-open")
  })
  
  observe({
    print(input$sidebar_tabs)
  })
  
  ##### combine #####
  observeEvent(input$go,{
    #browser()
    one<-chart_data()[[2]][[input$comb1]]
    two<-chart_data()[[2]][[input$comb2]]
    
    if(hasValue(input$comb1) & hasValue(input$comb2) & length(input$key)>0 & hasValue(input$join_type))
    {
      comb<-combine_data(one,two,type=input$join_type,by=input$key)
      tab_title <- paste0("combine","_",str_replace(input$comb1,".sas7bdat$",""),"_",str_replace(input$comb2,".sas7bdat$",""))
      
      #if new tab is already used, assign random number to it
      if(tab_title %in% input$tabs & !is.null(input$tabs))
      {
        tab_title<-paste0(tab_title,sample(1:10000,1,replace = F))
      }
      
      #finding column labels
      col_df<-column_listing()%>%as.data.frame()
      b_dat=col_df%>%filter(c %in% c(input$comb1,input$comb2)) #combined datasets
      
      col_names_ads<-b_dat%>%filter(a %in% names(comb))%>%distinct(a,.keep_all = T)%>%pull(b)
      headerCallback <- c(
        sprintf("  var tips = ['Row Names', %s];", toString(paste0("'", col_names_ads, "'"))),
        "header = table.columns().header();",
        "  for(var i = 1; i <= tips.length; i++){",
        "    $(header[i]).attr('title', tips[i]);",
        "  }"
      )
      
      #store combined data into reactive (comb_list)
      comb_list[[tab_title]]<-comb
      
      appendTab(inputId = "tabs",
                tabPanel(
                  tab_title,
                  # box(width = 12,
                  box(width = 12,background = "light-blue", "Keys Used:",
                      HTML(paste(input$key,sep = ","))),br(),br(),
                  
                  DT::renderDataTable({comb%>%mutate_if(is.character, as.factor)},filter='top',rownames=T,fillContainer=FALSE,escape=FALSE,style="bootstrap",extensions=list("Buttons"=NULL), options = list(
                    deferRender=TRUE,scrollX=TRUE,
                    autoWidth=TRUE,pageLength=5,lengthMenu=list(c(5,15,-1),c("5","15","All")),stateSave=FALSE, dom='Brlfrtip', 
                    buttons=I('colvis'),
                    columnDefs=list(
                      list(orderSequence = c("desc", "asc"), targets = "_all"),
                      list(className='dt-center',targets="_all")
                    ),
                    searchHighlight=TRUE,
                    search=list(regex=TRUE)
                    # style="bootstrap",columnDefs=list( list(orderSequence = c("desc", "asc"), targets = "_all"),list(className='dt-center',targets="_all"))#,
                  ),server = TRUE,
                  callback = htmlwidgets::JS(headerCallback))))
      # )))#)
      tab_list <<- c(tab_list, tab_title)
      updateTabsetPanel(session, "tabs", selected = tab_title)
      shinyjs::hide(selector = "ul.menu-open")
    }else{
      return(NULL)
    }
  })
  

  #### show modal when download button is clicked ####
  observeEvent(input$downBT,{
    if(input$tabs!="page1")
    {
      ignoreNULL = TRUE #show modal on startup
      showModal(myModal(input$tabs))
    }
    
  })
  
  output$bars<-renderHighchart({
    #browser()
    highchart() %>%
      hc_add_series(
        data = chart_data()[[1]]$n,
        type = "bar",
        name = paste("No. observations"),
        events = list(click = js_bar_clicked)) %>%
      hc_xAxis(
        categories = chart_data()[[1]]$datasets,
        tickmarkPlacement="on")
     
    
  })
  
  observeEvent(input$bar_clicked,
               {
                 ##########browser()
                 tab_title <- paste0(input$bar_clicked[1])
                 
                 #ccc=map(files(),filt2,input$sel_subj)
                 if(tab_title %in% tab_list == FALSE){
                   information<-files()[[input$bar_clicked[1]]]%>%filter(USUBJID %in% subj$subj)
                   
                   #finding column labels
                   row_ads<-which(column_listing()==input$bar_clicked[1],arr.ind = T)[,"row"]
                   col_names_ads<-column_listing()[row_ads,2]
                   headerCallback <- c(
                     sprintf("  var tips = ['Row Names', %s];", toString(paste0("'", col_names_ads, "'"))),
                     "header = table.columns().header();",
                     "  for(var i = 1; i <= tips.length; i++){",
                     "    $(header[i]).attr('title', tips[i]);",
                     "  }"
                   )
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               
                               tab_title,
                               DT::renderDataTable({information%>%mutate_if(is.character, as.factor)},rownames=T,fillContainer=FALSE,escape=FALSE,style="bootstrap", filter='top',extensions=list("Buttons"=NULL),options = list(
                                 deferRender=TRUE,scrollX=TRUE,
                                 autoWidth=TRUE,pageLength=5,lengthMenu=list(c(5,15,-1),c("5","15","All")),stateSave=FALSE, dom='Brlfrtip',
                                 
                                 buttons=I('colvis'),
                                 columnDefs=list(
                                   list(targets =c(seq_len(length(names(information)))[!seq_len(length(names(information))) %in% match(c("USUBJID", "STUDY"), names(information))]), visible = FALSE),
                                   list(className='dt-center',targets="_all")
                                 ),
                                 style="bootstrap"#,columnDefs=list( list(orderSequence = c("desc", "asc"), targets = "_all"),list(className='dt-center',targets="_all"))#,
                                 
                                 #searchHighlight=TRUE#,
                               ),server = TRUE,
                               callback = htmlwidgets::JS(headerCallback))))
                   
                   tab_list <<- c(tab_list, tab_title)
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
                 
               })
  
  
  
  #remove selected tabs
  observeEvent(input$remove, {
    # ##########browser()
    if(!input$tabs %in% "page1")
    {
      removeTab(inputId = "tabs", target = input$tabs)
      tab_list<<-tab_list[!tab_list %in% input$tabs]
    }
    
  })
  
  #remove all tabs if new subjects selected
  observeEvent(input$sel_subj,{
    tab_list %>%
      walk(~removeTab("tabs", .x))
    tab_list <<- NULL
  })
  
  ########## search dataset for strings ##################
  column_listing<-reactive({
    ###########browser()
    nam_files<-names(files())
    
    new<-list()
    for (i in 1:length(nam_files))
    {
      print(i)
      ncols<-files()[[nam_files[i]]]%>%names()%>%length()
      dc<-list()
      for (j in 1:ncols)
      {
        #print(j)
        
        temp<-files()[[nam_files[i]]]
        names_cols<-names(temp)
        bb=ifelse(hasValue(attr(temp[[names_cols[j]]],'label')),attr(temp[[names_cols[j]]],'label'),"NA")
        dc[[j]]<-c(a=names_cols[j],b=bb,c=nam_files[i])
        
      }
      new[[i]]<-do.call(rbind,dc)
      temp<-NULL
    }
    abc=do.call(rbind,new)
    abc
    
  })
  
  output$result<-renderText({
    # ##########browser()
    input$search_id
    rowss=which(column_listing()==toupper(input$search_id),arr.ind = T)[,"row"]
    
  })
  
  
  #Generate the table in tab1
  output$listing_variables<-DT::renderDataTable({
    # ##########browser()
    column_listing()%>%as.data.frame()%>%mutate_if(is.character, as.factor)},colnames=c("Variable","Label","Dataset"), rownames=FALSE,fillContainer=FALSE,escape=FALSE,style="bootstrap",options = list(
      deferRender=TRUE,
      pageLength = 5,
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      order=list(list(0,'asc')),
      autoWidth=TRUE,order=FALSE,stateSave=FALSE, dom='Bflritp',
      columnDefs=list( list(className='dt-center',targets="_all")),
      searchHighlight=TRUE,
      search=list(regex=TRUE)
      
    ),filter='top',server = TRUE
    
  )
  
  #####control download button#####
  output$save_state<- downloadHandler(
    filename=function(){
      # #########browser()
      if(!is.null(input$sel_extract) & hasValue(input$sel_extract))
      {
        d_date<-input$sel_extract
      }
      
      
      here::here("Data",d_date,paste0(input$save_name,Sys.Date(),".rds"))},
    content=function(file){
      ##########browser()
      data_combine<-comb_list[[input$tabs]]
      saveRDS(data_combine,file)
    }
    
  )
  
  
  #selected tab dataset
  tab_dataset<-reactive({
    ####browser()
    if(!is.null(input$tabs))
    {
      comb_list[[input$tabs]]
    }
    
  })
  
  
  
  #####   pivot #####
  output$pivotts<-renderUI({
    #browser()
    if(!is.null(tab_dataset()))
    {
      fluidPage(
        # fluidRow(
        #   column(width = 3,
        #   selectInput(inputId = "sel_combined",label = "Select combined datasets",choices = comb_list%>%names())
        # )),
        fluidRow(
          box(
            textOutput("pivot_tbl")
          )
        ),
        fluidRow(
          class = "panel panel-heading",
          div(
            class = "panel-heading",
            h3("Dragging variables to define a plot")
          ),
          fluidRow(
            class = "panel-body",
            column(
              width = 3,
              tags$div(
                class = "panel panel-default",
                tags$div(class = "panel-heading", "Variables"),
                tags$div(
                  class = "panel-body",
                  style = "overflow-y: scroll; height: 150px",
                  id = "sort1",
                  colnames_to_tags(tab_dataset()%>%select(sort(current_vars())))
                )
              )
            ),
            column(
              width = 3,
              # analyse as x
              tags$div(
                class = "panel panel-default",
                tags$div(
                  class = "panel-heading",
                  tags$span(class = "glyphicon glyphicon-stats"),
                  "Row (drag here)"
                ),
                tags$div(
                  class = "panel-body",
                  id = "sort2"
                )
              ),
              # analyse as y
              tags$div(
                class = "panel panel-default",
                tags$div(
                  class = "panel-heading",
                  tags$span(class = "glyphicon glyphicon-stats"),
                  "Column (drag here)"
                ),
                tags$div(
                  class = "panel-body",
                  id = "sort3"
                )
              )

            ),
            column(width = 2,
                   selectInput("x_filter", "Filter by:",choices = NULL),
                   selectInput("y_filter", "Filter by:",choices = NULL)

            ),
            column(width=2,
                   selectInput("x_val", "Filter by:",choices = NULL),
                   selectInput("y_val", "Filter by:",choices = NULL),
                   actionButton(inputId = "go_comb",label="go")
            ),
            column(width = 2,
                   shinyjs::hidden(
                     div(id='param_avalc',
                         selectInput("param_avalc1", "AVALC:",choices = NULL)
                     )
                   ))
            
          ),
          fluidRow(style="margin-left: 50px",
                   shinyjs::hidden(div(id="pivot_div",style = "overflow-y: scroll; height: 550px; margin-left:50px; width:1500px; overflow-x: scroll",
                                       pivottablerOutput('pvt')
                   )
                   ))
        ),
        sortable_js(
          "sort1",
          options = sortable_options(
            group = list(
              name = "sortGroup1",
              put = TRUE
            ),
            sort = FALSE,
            onSort = sortable_js_capture_input("sort_vars"),
            height="200px"
          )
        ),
        sortable_js(
          "sort2",
          options = sortable_options(
            group = list(
              group = "sortGroup1",
              put=TRUE
            ),
            onSort = sortable_js_capture_input("sort_x")
          )
        ),
        sortable_js(
          "sort3",
          options = sortable_options(
            group = list(
              group = "sortGroup1",
              put=TRUE
            ),
            sort = TRUE,
            onSort = sortable_js_capture_input("sort_y")
          )
        )
      )
    }
    
  })
  
  observeEvent(list(input$tabs,input$sort_x,input$sort_y),{
    #browser()
    if(length(input$sort_x)>0)
    {
      updateSelectInput(session,"x_filter",choices = c(" ",input$sort_x))
      
    }
    
    if(length(input$sort_y)>0)
    {
      updateSelectInput(session,"y_filter",choices = c(" ",input$sort_y))
    }
    
    if(length(input$sort_x)>0 & length(input$sort_y)>0)
    {
      shinyjs::show(id="pivot_div")
      n_table$val<-n_table$val+1
      if(any(tolower(input$sort_x) %in% "param") | tolower(input$sort_y) %in% "param")
      {
        shinyjs::show(id="param_avalc")
        avalc_val<-tab_dataset()[["AVALC"]]%>%unique()%>%sort()
        updateSelectInput(session,"param_avalc1",choices = c(" ",avalc_val),selected = max(avalc_val))
      }
    }
    
  })
  
  observeEvent(input$x_filter,{
    aval_x<-tab_dataset()[[input$x_filter]]%>%unique()
    updateSelectInput(session,"x_val",choices = c(" ",aval_x))
  })
  
  observeEvent(input$y_filter,{
    aval_y<-tab_dataset()[[input$y_filter]]%>%unique()
    updateSelectInput(session,"y_val",choices = c(" ",aval_y))
  })
  
  output$pivot_tbl<-renderText({
    ##browser()
    paste0("Current dataset:",input$tabs)
  })
  
  
  n_table<-reactiveValues(val=0)
  
  observeEvent(tab_dataset(),{
    # ######browser()
    pivot_vars$x<-NULL
    pivot_vars$y<-NULL
    n_table$val<-0
  })
  
 
  pivot_vars<-reactiveValues(x=NULL,y=NULL)
  
  test<-reactiveValues(val=NULL)
  observeEvent(input$go_comb,{
    #browser()
    if(n_table$val>0){
      pivot_vars$x<-input$sort_x
      pivot_vars$y<-input$sort_y
      
      #variables for rows and columns; remove duplicates
      pv_clean<-tab_dataset()%>%select(pivot_vars$x,pivot_vars$y)%>%distinct(.keep_all = T)
      
      #if avalc available
      if(length(input$param_avalc1)>0 & ((tolower(input$x_filter) %in% "param") | (tolower(input$y_filter) %in% "param")))
      {
        pv_clean<-tab_dataset()%>%select(pivot_vars$x,pivot_vars$y,AVALC)%>%distinct(.keep_all = T)%>%filter(AVALC %in% input$param_avalc1)
      }
      
      
      if(hasValue(input$x_filter) & hasValue(input$x_val))
      {
        pv_clean <-pv_clean%>%select(pivot_vars$x,pivot_vars$y)%>%distinct(.keep_all = T)%>%filter(!!rlang::sym(input$x_filter) %in% input$x_val)
      }
      
      if(hasValue(input$y_filter) & hasValue(input$y_val))
      {
        pv_clean <-pv_clean%>%select(pivot_vars$x,pivot_vars$y)%>%distinct(.keep_all = T)%>%filter(!!rlang::sym(input$y_filter) %in% input$y_val)
        
      }}
    test$val<-pv_clean%>%as.data.frame()
  })
  
  
  output$pvt<- renderPivottabler({
    if((!is.null(pivot_vars$x) & length(pivot_vars$x)>0) & (!is.null(pivot_vars$y) & length(pivot_vars$y)>0))
    {
      #browser()
      da<-test$val
      da_x<-pivot_vars$x
      da_y<-pivot_vars$y

        pt <- PivotTable$new()
      
        #check the number of rows before performing computation
        if(nrow(da[da_x])>200 | nrow(da[da_y])>200)
        {
          da<-da %>%head(n=100)
          showModal(modalDialog(
            title = "Table is too large. Only first 100 is shown.",
            "Use filter to narrow the search!",
            easyClose = TRUE,
            footer=NULL
          ))
        }

        withProgress(message = 'Creating plot',value = 0.1, {
        pt$addData(da)
   
        for(i in 1:length(da_x))
        {
          pt$addRowDataGroups(da_x[i])
        }
        
        for(i in 1:length(da_y))
        {
          pt$addColumnDataGroups(da_y[i])
          
        }
          incProgress(0.5)
      
        pt$defineCalculation(calculationName="ab", summariseExpression="n()")
        pt$evaluatePivot()
        incProgress(1)
        })
        pivottabler(pt)

    }else{
       pt <- PivotTable$new()
       pt$evaluatePivot()
       pivottabler(pt)
     }
    
  })
  
  #### Adverse Event ####
  output$timeline <- renderTimevis({
    ##browser()
    adae<-files()$adae.sas7bdat%>%filter(USUBJID %in% subj$subj)
    adae_clean<-adae%>%select(AEDECOD,AESTDTC,AEENDTC)%>%mutate(aestart=lubridate::ymd(AESTDTC),aeend=lubridate::ymd(AEENDTC))%>%
      filter(!AEDECOD %in% c(" ","",NA),!is.na(aestart),!is.na(aeend))%>%select(AEDECOD,aestart,aeend)%>%rename(content=AEDECOD,start=aestart,end=aeend)
    timevis(adae_clean)
  })
  
})