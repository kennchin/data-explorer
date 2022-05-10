
downUI<-function(id)
{
  ns<-NS(id)
  
  div(
    style = "position: relative; left: 15em; top: 2em;",
    actionBttn(
      inputId = ns("downBT"),
      icon = icon("search-plus", class = "opt"),
      style = "fill",
      color = "danger",
      size = "xs"
    )
  )
  
}

down <- function(input, output, session) 
{
  browser()
  #modal for download
  myModal<- function(ns){
    div(id = "test2",
        modalDialog(
          textInput(inputId = ns("save_name"),label = "",placeholder = "Name of file"),
          downloadButton(ns("save_state"), "Save"),easyClose = TRUE, title="Dowload Combined Table",
          footer = tagList(actionButton(ns("close"), "OK"))
        )
    )
  }
  
  
  
  observeEvent(input$downBT,{
    browser()
    ignoreNULL = TRUE #show modal on startup
    showModal(myModal(session$ns))
  })
  
  
}