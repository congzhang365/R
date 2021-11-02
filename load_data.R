library(dplyr)
library(tidyr)
library(stringr)
library(imputeTS)


hz_to_erb <- function(x, na.rm = T){
  y = 16.7 * log10(x/165.4 + 1)
  return(y)
}



load_data <- function(path, interpolation) {
  # path = "C:/Users/sprin/Dropbox (Personal)/Journal_Article/TJ_YNQ/Study_1/data/refined_f0_f.txt" # For testing
  data <- read.table(path, header = T, stringsAsFactors = T)
  data <- data %>% 
    tidyr::separate(filename, c("speaker", "utterance", 'study_type', "tone", "version", "repetition", "tune"), sep = "_", remove = F) %>% 
    dplyr::mutate_if(is.character,as.factor)
  data$gender <- ifelse(str_starts(data$speaker,"M"), "M", "F")
  
  # undefined values to NA
  data$f0 <- as.character(data$f0)
  data$f0 <- ifelse(data$f0=="NaN", NA, data$f0)
  data$f0 <- ifelse(data$f0=="--undefined--", NA, data$f0)
  data$f0 <- ifelse(data$f0== -1, NA, data$f0)
  
  data$f0 <- as.numeric(as.character(data$f0))
  
  # interpolation
  if (interpolation == T){
    data <- data %>%
      group_by(filename) %>%
      dplyr::mutate(ip.f0=spline(time,f0 ,n=n())$y)
    data <- ungroup(data)
    # hz to erb
    data$f0.erb <- hz_to_erb(data$ip.f0)
  } else {
    # hz to erb
    data$f0.erb <- ifelse(is.na(data$f0), NA, hz_to_erb(data$f0))
    
  }
  
  return(data)
}



load_mean_data <- function(path) {
  # path = "C:/Users/sprin/Dropbox (Personal)/Journal_Article/TJ_YNQ/Study_2/data/mean_f0_f.txt" # For testing
  data <- read.table(path, header = T, stringsAsFactors = F)
  
  # # undefined values to NA

  data$max.f0 <- as.numeric(ifelse(as.character(data$max.f0)=="--undefined--", NA, data$max.f0))
  data$max.f0.time <- as.numeric(ifelse(as.character(data$max.f0.time)=="--undefined--", NA, data$max.f0.time))
  data$min.f0	<- as.numeric(ifelse(as.character(data$min.f0)=="--undefined--", NA, data$min.f0))
  data$min.f0.time <- as.numeric(ifelse(as.character(data$min.f0.time)=="--undefined--", NA, data$min.f0.time))
  data$mean.f0 <- as.numeric(ifelse(as.character(data$mean.f0)=="--undefined--", NA, data$mean.f0))
  data$mean.intensity <- as.numeric(ifelse(as.character(data$mean.intensity)=="--undefined--", NA, data$mean.intensity))
  
  data <- data %>% 
    tidyr::separate(filename, c("speaker", "utterance", 'study_type', "tone", "version", "repetition", "tune"), sep = "_", remove = F) %>% 
    dplyr::mutate_if(is.character,as.factor) 
  
  data$gender <- ifelse(str_starts(data$speaker,"M"), "M", "F")
  
  
  # hz to erb
  data$range.f0 = (data$max.f0 - data$min.f0)
  data$max.f0.erb = hz_to_erb(data$max.f0)
  data$min.f0.erb = hz_to_erb(data$min.f0)
  data$mean.f0.erb = hz_to_erb(data$mean.f0)
  data$range.f0.erb = hz_to_erb(data$range.f0)
  
  
  return(data)
}
