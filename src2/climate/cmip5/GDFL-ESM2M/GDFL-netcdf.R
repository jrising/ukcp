setwd("~/Open Modeling Group Dropbox/COACCH/climate/cmip5/GDFL-ESM2M/")

#install packages
library(ncdf4)
library(abind)
library(raster)

#Concatenating GDFL model historical files
filenames= list.files('historical/',pattern='*.nc',full.names=TRUE) 
files = grep("186601|187101|187601|188101|188601", filenames, value = TRUE)  #only for historical data
files = sort(files)

#Concatenating GDFL model rcp85 files
filesrcp= list.files('rcp85/',pattern='*.nc',full.names=TRUE) 
filesrcp = sort(filesrcp)

time = c()
tas = NULL

labels <- c()

# Loop over files
# for(i in seq_along(files)) {   # for historical files
#   nc = nc_open(files[i])
for(i in seq_along(filesrcp)) {   # for rcp85 files
  nc = nc_open(filesrcp[i])
  time <- c(time,ncvar_get(nc, "time"))
  tas<-abind(tas, ncvar_get(nc, 'ts'), along=3)  
  nc_close(nc)
}

#Concatenating 
# nc1 = nc_open(files[1])  #for historical data
nc1 = nc_open(filesrcp[1])
lon <- ncdim_def('lon', "degrees_east",ncvar_get(nc1, "lon"))
lat <- ncdim_def('lat', "degrees_north",ncvar_get(nc1, "lat"))
t <- ncdim_def('time', ncatt_get(nc, "time", "units")$value, time)

ts<-ncvar_def("tas", "K", list(lon, lat, t), 1.e20 )

# ncnew <- nc_create( "tasGDFL-hist.nc", ts )   #for historical data

ncnew <- nc_create( "tasGDFL-rcp.nc", ts )   #for rcp85 data

ncatt_put(ncnew, 'time', 'calendar', ncatt_get(nc, "time", "calendar")$value)

ncvar_put(ncnew, 'tas', tas)
nc_close(ncnew)
