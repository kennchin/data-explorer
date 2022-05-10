extract<-"2022-05-09"
ads_folder<-paste0(here::here("Data"),"/",extract)

files=list.files(ads_folder,recursive = T,full.names = T)

#label for files
temp_label<-gsub(".RData","",basename(files))

b=lapply(lapply(files,load,.GlobalEnv),saveRDS,temp_label)

for(i in 1:length(b))
{
  load(files[i])
  saveRDS(x,file = paste0(here::here("Data/"),extract,"/",temp_label[i],".RDS"))
  
}

df <- list.files(ads_folder,pattern = ".RDS",full.names = T) %>%
  map(readRDS) %>% setNames(temp_label)

saveRDS(df,paste0(here::here("Data/"),extract,"/","ads.RDS"))

