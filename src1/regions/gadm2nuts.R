setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(PBSmapping)

shp.gadm <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
polydata.gadm <- attr(shp.gadm, 'PolyData')
shp.nuts <- importShapefile("regions/ref-nuts-2016-10m.shp/NUTS_RG_10M_2016_4326_LEVL_3.shp/NUTS_RG_10M_2016_4326_LEVL_3.shp")
polydata.nuts <- attr(shp.nuts, 'PolyData')

centroids.gadm <- calcCentroid(shp.gadm, rollup=1)
names(centroids.gadm)[1] <- "EID"
events <- as.EventData(centroids.gadm, projection=1)

found <- findPolys(events, shp.nuts)

all(polydata.nuts$NUTS_ID == polydata.nuts$FID)

xwalk <- cbind(polydata.gadm[found$EID,], polydata.nuts[found$PID, c('NUTS_ID', 'NUTS_NAME')])

write.csv(xwalk, "regions/gadm2nuts.csv", row.names=F)
