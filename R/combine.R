get_data<-function(dataset,var="",envir=envir)
{
  if(!var %in% c(""))
  {
    filtered_data<-dataset%>%select(var)
  }else
  {
    filtered_data<-dataset
  }
  print(filtered_data)                      
}

combine_data <- function(x, y, by = "", add = "",type = "inner_join",other_cols="",
  envir = parent.frame(),
  ...
) {
  #browser()
  is_join <- grepl("_join", type)
  if (is_join && is_empty(by)) {
    return(cat("No variables selected to join datasets\n"))
  }
  
  x <- get_data(x, envir = envir)
  if (all(add == "")) {
    y <- get_data(y, envir = envir)
  } else {
    y <- get_data(y, unique(c(by, add)), envir = envir)
  }
  
  if (is_join) {
    x <- get(type, envir = as.environment("package:dplyr"))(x, y, by = by)
    madd <- paste0("<br>\nBy: ", paste0(by, collapse = ", "))
  } else {
    x <- get(type, envir = as.environment("package:dplyr"))(x, y)
    madd <- ""
  }
  
  return(x)
}
  