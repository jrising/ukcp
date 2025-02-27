# setwd("~/Open Modeling Group Dropbox/COACCH/climate/cmip5/HadGEM2-ES/historical")
# datapath <- "/Users/ritikakhurana/Open Modeling Group Dropbox/COACCH/climate/cmip5/HadGEM2-ES/historical"

setwd("~/Open Modeling Group Dropbox/COACCH/climate/cmip5/HadGEM2-ES/rcp85")
datapath <- "/Users/ritikakhurana/Open Modeling Group Dropbox/COACCH/climate/cmip5/HadGEM2-ES/rcp85"


#install packages
library(ncdf4)
library(abind)
library(raster)


#Open all files
files= list.files('.',pattern='*.nc',full.names=TRUE)
files = sort(files)

time = c()
tas = NULL

# Loop over files
for(i in seq_along(files)) {
  nc = nc_open(files[i])
  time <- c(time,ncvar_get(nc, "time"))
  tas<-abind(tas, ncvar_get(nc, 'ts'), along=3)   #for rcp85
  
  nc_close(nc)
}

#Concatenating 
nc1 = nc_open(files[1])
lon <- ncdim_def('lon', "degrees_east",ncvar_get(nc1, "lon"))
lat <- ncdim_def('lat', "degrees_north",ncvar_get(nc1, "lat"))
t <- ncdim_def('time', ncatt_get(nc, "time", "units")$value, time)

ts<-ncvar_def("tas", "K", list(lon, lat, t), 1.e20 )

ncnew <- nc_create( "/Users/ritikakhurana/Open Modeling Group Dropbox/COACCH/climate/cmip5/HadGEM2-ES/ts-HadGEM2-rcp.nc", ts )

ncatt_put(ncnew, 'time', 'calendar', ncatt_get(nc, "time", "calendar")$value)

ncvar_put(ncnew, "tas", tas)
nc_close(ncnew)
