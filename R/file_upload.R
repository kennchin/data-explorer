#module for file upload
FileInputUI<- function(id)
{
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"),label="Upload File",accept = c(".sas7bdat",".rds"),multiple = TRUE)
  )
  
}

FileInput <- function(input,output,session)
{
  #if no file, stop;otherwise continue
  userFile<-reactive ({
    validate(need(input$file,message=FALSE))
    input$file
     #print(input$file)
  })
  
  observeEvent(userFile(),{
    showModal(modalDialog(
      textInput(inputId = session$ns("extract_date"), label="Enter Extract Date:",
                placeholder = 'Enter date here'),easyClose = TRUE, title="Extract Date",
      footer = tagList(actionButton(session$ns("close2"), "OK"))
    )
    )
  })

  rv_date<-reactiveValues(l=NULL,c=NULL)
  file_df <- eventReactive(input$close2,{
    #browser()
    
    if(!dir.exists(here::here("Data",paste0(input$extract_date))) | length(list.files(paste0(here::here("Data",input$extract_date))))<1)
    {
      dir.create(here::here("Data",paste0(input$extract_date)))
      #check that all files are sas7bdat
      if(all(str_extract(userFile()$name, "[^.]*$") %in% "sas7bdat"))
      {
        #single sas file
        if(length(userFile()$name)==1)
        {
          df<-single_file(userFile(),input$extract_date)
        }else{
          df<-multiple_files(userFile(),input$extract_date)
        }
        #df<-multiple_files(userFile())
        
      }else if(all(str_extract(userFile()$name, "[^.]*$") %in% "RDS"))
      {
        df<-readRDS(userFile()$datapath)
        saveRDS(df,paste0(here::here("Data",input$extract_date),"/",userFile()$name))
        df
      }
      
    }else{
      print("Folder with current date already exists!")
  
      rds_name<-list.files(paste0(here::here("Data",input$extract_date)),pattern = ".RDS$")
      df<-readRDS(paste0(here::here("Data",input$extract_date),"/",rds_name))
     
      showModal(modalDialog(size="s",footer = NULL,
                            "Extract already exists"
      ))  
      Sys.sleep(2)
      removeModal()
    }
    removeModal()
    
    rv_date$l<-input$extract_date
    rv_date$c<-input$close2
    list(df,rv_date$l,rv_date$c)
      })
  
  return(file_df)
}

