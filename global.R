library(rlang)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
# library(easypackages)
library(DT)
library(data.table)
library(shinyjs)
library(stringi)
library(purrr)
library(highcharter)
library(pivottabler)
library(sortable)
library(timevis)
library(here)
library(stringr)
library(dplyr)



options(shiny.maxRequestSize=10000*1024^2) #10GB
# library(easypackages)
# easypackages::packages("here","DT","extrafont","haven","rlang","dplyr","tidyr","rpivotTable","lubridate","Hmisc","reshape2","readxl","stringr","grid","gridExtra","gtable","openxlsx")

source(here::here("R","single_file.R"))
source(here::here("R","multiple_files.R"))
source(here::here("R","file_upload.R"))
source(here::here("R","combine.R"))
source(here::here("R","down_module.R"))

#### Note: add download button in table to use in pivot
colnames_to_tags <- function(df){
  #browser()
  df<-df%>%select_if(function(col) is.character(col) | is.numeric(col))%>%as.data.frame()
  lapply(
    colnames(df),
    function(co) {
      tag(`_tag_name` = "p",
        list(
          #class = class(df[, co]),
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(co)
        )
      )
    }
  )
}

## define a helper function
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}


# packages("here","DT","extrafont","haven","rlang","dplyr","tidyr","rpivotTable","lubridate","Hmisc","reshape2","readxl","stringr","grid","gridExtra","gtable","openxlsx")


if(!file.exists(".here")){
  here::set_here()
  setwd(here::here())
}

print(here::here())
#create Data folder
if (!dir.exists(here::here("Data"))){
  dir.create(here::here("Data"))
} else {
  print("Data Dir already exists!")
}
#create folder with current date
# if(!dir.exists(here::here("Data",paste0(Sys.Date()))))
# {
#   #browser()
#   dir.create(here::here("Data",paste0(Sys.Date())))
# }else{
#   print("Folder with current date already exists!")
# }

hasValue<-function(x){
  if(is.null(x)){return(F)}
  if(x==""){return(F)}
  if(x==" "){return(F)}
  return(T)
}

filt1<-function(x,subj){
  # x%>%group_by(USUBJID)%>%summarise(n=n())%>%filter(USUBJID %in% subj)
  ###########browser()
  x%>%filter(USUBJID %in% subj)%>%group_by(datasets)%>%summarise(n=n())
}

filt2<-function(x,subj){
  x%>%filter(USUBJID %in% subj)
  # ##########browser()
  # x%>%filter(USUBJID %in% subj)%>%group_by(datasets)%>%summarise(n=n())
}

# path_old<-here::here("lib")
# library(remotes)
# # install_version("rlang", version = "0.4.9", lib = path_old,upgrade = "never",dependencies = T)
# install_version("shinyWidgets", version = "0.5.2", lib = path_old,upgrade = "never",dependencies = T)
# install_version("htmltools", version = "0.5.1", lib = path_old,upgrade = "never",dependencies = T)
# # install_version("vctrs", version = "0.3.5", lib = path_old,upgrade = "never",dependencies = T)
# install_version("highcharter", version = "0.7.0", lib = path_old,upgrade = "never",dependencies = T)
# install_version("glue", version = "1.3.2", lib = path_old,upgrade = "never",dependencies = T)
# install_version("lifecycle", version = "1.0.0", lib = path_old,upgrade = "never",dependencies = T)
# install_version("broom", version = "0.7.6", lib = path_old,upgrade = "never",dependencies = T)
# install_version("tidyverse", version = "1.3.1", lib = path_old,upgrade = "never",dependencies = T)

