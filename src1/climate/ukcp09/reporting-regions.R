setwd("~/Open Modeling Group Dropbox/UK Economic Risks/climate/ukcp09")

library(ncdf4)

## Figure out temperatures of scenariso

nc <- nc_open("A1B-03236a-global-ann-2070-2099.nc")
temp <- ncvar_get(nc, "temp_dmean_tmean_abs")
c(mean(temp), sd(temp)) # 3.3956514 0.6404901

nc <- nc_open("A1FI-03236a-global-ann-2070-2099.nc")
temp <- ncvar_get(nc, "temp_dmean_tmean_abs")
c(mean(temp), sd(temp)) # 4.2983703 0.7331057 <-- 4C?? [supported by appendix C]

nc <- nc_open("B1-03236a-global-ann-2070-2099.nc")
temp <- ncvar_get(nc, "temp_dmean_tmean_abs")
c(mean(temp), sd(temp)) # 2.6464651 0.5487779 <-- 2C??

## Aggregate up the data

library(PBSmapping)

shp <- importShapefile("../../regions/gadm36_GBR_shp/gadm36_GBR_2.shp")
polydata <- attr(shp, 'PolyData')

regions <- read.csv("../../channels/flooding/table6-9-tabula.csv")
regions <- unique(regions$X.1[-1])[-1]

polydata$region <- NA
for (name in polydata$NAME_2)
    print(c(name, grep(name, regions)))
