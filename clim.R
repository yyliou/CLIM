
#------------------------------------#
# Historical Weather Data Downloader
#------------------------------------#

# Resources: https://mycolab.pp.nchu.edu.tw/historical_weather/
# Author: Oliver Liou
# Version: 1.0

# Notes: 
# (1) These function is used to collect data automatically.
# We do not ensure the accuracy of the data. For example, there
# exists some missing value which is coded as "-999" or other 
# unreasonable value. The function we provide do not deal with the 
# problem sine we want to accelerate the process.

# (2) These function are constructed specifically for academic purpose.
# We do not expect any utilization in commercial activities.
# If you want to used these function and obtain weather data,
# please acknowledge that these data comes from CWA, TW. 

#------------------------------------#
# Package required
#------------------------------------#

library("httr")
library("jsonlite")

#------------------------------------#
# Station information
#------------------------------------#

station <- function(type){
  url <- "https://e-service.cwa.gov.tw/wdps/obs/state.htm"
  webpage <- read_html(url)
  if(type == "existing") {n <- 1}
  else if(type == "stop") {n <- 2}
  else if(type == "temp_stop") {n <- 3}
  else if(type == "total"){
    n <- 0
    data <- data.frame()
    for( i in 1:2){
      temp <- webpage %>% html_table(fill = TRUE) %>% .[[i]]
      temp$exist <- 2 - i
      data <- rbind(data,temp)
    }
  } else {
    n <- 0
    cat(crayon::red("Type is forbidden.\n"))
  }
  if(type != "total" & n > 0) {
    data <- webpage %>% html_table(fill = T) %>% .[[n]]
  }
  else if(type != "total" & n == 0) {
    data <- data.frame()
  }
  if(length(data)>0){
    data$station_type <- ifelse(data$站種 == "署屬有人站","有人站","")
    data$station_type <- ifelse(grepl("雷達站",data$站名) == T,"雷達站",data$station_type)
    data$station_type <- ifelse(substr(data$站號,1,2) =="C1","雨量站",data$station_type)
    data$station_type <- ifelse(substr(data$站號,1,2) =="C0","氣象站",data$station_type)
    data$station_type <- ifelse(data$站種 =="農業站","農業站",data$station_type)
  }
  data
  
}

#------------------------------------#
# Data collector
#------------------------------------#

clim <- function(start,end,no,type = "daily",full_report = F,format = "list"){
  url <- "https://mycolab.pp.nchu.edu.tw/historical_weather/index.php?station_id="
  e <- 0
  contains <- function(x) {any(grepl(":", x))}
  if(format == "list") {
    file <- list()
    for(i in 1:length(no)) {
        temp <- read.csv(paste0(url,no[i],"&startdate=",
                                gsub("-","",start),"&enddate=",gsub("-","",end),"&type=",type))
      if(length(temp[,1])>0 & grepl("No such file or directory",temp[1,1]) == F) {
        temp$no <- no[i]
        file[[i]] <- temp
        if(full_report == T) {
          cat(paste0(no[i]," is loaded successfully.\n"))
          }
      } else {
        cat(crayon::red(paste0(no[i]," failed to load.\n")))
        e <- e + 1
      }
    }
  } else if(format == "data_frame") {
    file <- data.frame()
    for( i in 1:length(no)) {
      temp <- read.csv(paste0(url,no[i],"&startdate=",
                              gsub("-","",start),"&enddate=",gsub("-","",end),"&type=",type))
      if(length(temp[,1])>0 & grepl("No such file or directory",temp[1,1]) == F) {
        temp[] <- lapply(temp, as.character)
        temp$no <- no[i]
        file <- dplyr::bind_rows(file,temp)
        if(full_report == T) {
          cat(paste0(no[i]," is loaded successfully.\n"))
          }
      } else {
        cat(crayon::red(paste0(no[i]," failed to load.\n")))
        e <- e + 1
      }
    }
  } else { cat(crayon::red("Format is forbidden.\n"))
    }
  cat(crayon::red(paste0("Error proportion is ",round(e*100/length(no),2),"%.\n")))
  file
}

example3 <- clim(start = "2009-08-01",
                 end = "2009-08-31",
                 no = c("A2Q950","C0C480"),
                 type = "daily",
                 format = "data_frame")

#------------------------------------#
# Simple example
#------------------------------------#

st <- station("total")

# library(readxl)
# 測站 <- read_excel("~/Downloads/測站.xlsx", sheet = "工作表3")
# no <- as.vector(測站$站號)

example <- clim(start = "2009-08-01",
                end = "2009-08-31",
                no = no,
                type = "hourly",
                format = "data_frame",
                full_report = T)

write.csv(example,"main.csv")

example2 <- clim(start = "2009-08-01",
                 end = "2009-08-31",
                 no = no,
                 type = "daily",
                 format = "data_frame")

write.csv(example2,"daily.csv")

example3 <- clim(start = "2009-08-01",
                 end = "2009-08-31",
                 no = c("A2Q950","C0C480"),
                 type = "daily",
                 format = "data_frame")


