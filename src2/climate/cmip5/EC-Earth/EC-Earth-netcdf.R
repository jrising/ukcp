setwd("~/Open Modeling Group Dropbox/COACCH/climate/cmip5/EC-Earth/historical/")
datapath <- "~/COACCH/climate/cmip5/EC-Earth/historical/"

# setwd("~/Open Modeling Group Dropbox/COACCH/climate/cmip5/EC-Earth/rcp85/")
# datapath <- "~/COACCH/climate/cmip5/EC-Earth/rcp85/"

#install packages
library(ncdf4)
library(abind)
library(raster)

#Open all files
filenames= list.files('.',pattern='*.nc',full.names=TRUE)
files = grep("185001|186001|187001|188001|1890001", filenames, value = TRUE)  #only for historical data
files = sort(files)


time = c()
tas = NULL

# Loop over files
for(i in seq_along(files)) {
  nc = nc_open(files[i])
  date1 <- substring(ncatt_get(nc, "time", "units")$value, 12, 30)
  dates = as.Date(date1) + ncvar_get(nc, "time")
  time <- c(time,as.numeric(dates - as.Date("1850-01-01")))
  tas<-abind(tas, ncvar_get(nc, 'tas'), along=3)
  nc_close(nc)
}

#Concatenating 
nc1 = nc_open(files[1])
lon <- ncdim_def('lon', "degrees_east",ncvar_get(nc1, "lon"))
lat <- ncdim_def('lat', "degrees_north",ncvar_get(nc1, "lat"))
t <- ncdim_def('time', ncatt_get(nc, "time", "units")$value, time)

ts<-ncvar_def("tas", "K", list(lon, lat, t), 1.e20 )

ncnew <- nc_create( "../tas-EC-Earth-hist.nc", ts )   #for historical data
# ncnew <- nc_create( "../tas-EC-Earth-rcp85.nc", ts )   #for rcp85

ncatt_put(ncnew, 'time', 'calendar', ncatt_get(nc, "time", "calendar")$value)

ncvar_put(ncnew, "tas", tas)
nc_close(ncnew)
