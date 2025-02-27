setwd("~/Open Modeling Group Dropbox/COACCH/climate/cmip5")

library(ncdf4)
library(abind)
library(raster)
library(dplyr)

#Open all historical files (fishery model)
filenames = list.files('.', pattern='.+hist.+aggregated.csv',full.names=TRUE, recursive = TRUE) 
files = grep("CNRM|MPI|CM5A-MR", filenames, value = TRUE)  #only for historical data

gcm = NULL
for (i in 1:length(files)) {
  df <-read.csv(files[i])
  df<-subset(df,time > "1850-01-01 12:00:00" & time < "1889-12-17 12:00:00")
  df$yyyymm <- sapply(df$time, function(time) substring(time, 1, 7)[[1]])
  names(df)[names(df) == 'tas'] = 'ts'
  df <- df[, c('yyyymm', 'NUTS_ID','ts')]
  if (is.null(gcm)) {
    gcm <-df
  }else{
    gcm <- gcm %>% left_join(df, by=c("yyyymm", "NUTS_ID"), suffix = c('', files[i]))
  }
}

gcm$tsavg <- rowMeans(gcm[, -1:-2])
write.csv(gcm, "Combine-GCM/energy-gcm-hist.csv", row.names = FALSE)

filenames = list.files('.', pattern='.+rcp85.+aggregated.csv',full.names=TRUE, recursive = TRUE) 
filesrcp = grep("CNRM|MPI|CM5A-MR", filenames, value = TRUE)  #only for historical data

gcm = NULL
for (i in 1:length(filesrcp)) {
  df <-read.csv(filesrcp[i])
  df$yyyymm <- sapply(df$time, function(time) substring(time, 1, 7)[[1]])
  names(df)[names(df) == 'tas'] = 'ts'
  df <- df[, c('yyyymm', 'NUTS_ID','ts')]
  if (is.null(gcm)) {
    gcm <-df
  }else{
    gcm <- gcm %>% left_join(df, by=c("yyyymm", "NUTS_ID"), suffix = c('', filesrcp[i]))
  }
}

gcm$tsavg <- rowMeans(gcm[, -1:-2])
write.csv(gcm, "Combine-GCM/energy-gcm-rcp.csv", row.names = FALSE)
