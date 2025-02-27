disaggregate <- function(regionpath, regattr, subset.shp=function(shp, polydata) shp) {
    library(PBSmapping)
    library(dplyr)
    shp.withattr <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.shp")
    polydata.withattr <- attr(shp.withattr, 'PolyData')
    polydata.withattr <- polydata.withattr[, -(which(names(polydata.withattr)[-1] == 'PID') + 1)]

    ## Need to decide on 1 region PID per adm3 PID

    centroids <- calcCentroid(shp.withattr, rollup=3)
    centroids$EID <- 1:nrow(centroids)

    shp.regions <- importShapefile(regionpath)
    polydata.regions <- attr(shp.regions, 'PolyData')

    shp.regions <- subset.shp(shp.regions, polydata.regions)

    found <- findPolys(as.EventData(centroids, projection=1), shp.regions)

    centroids$PID.region <- NA
    centroids$PID.region[found$EID] <- found$PID
    matching <- centroids %>% group_by(PID) %>% summarize(PID.region=ifelse(all(PID.region == PID.region[1]), PID.region[1], NA))
    polydata.withattr$PID.region <- matching$PID.region

    centroids <- calcCentroid(shp.withattr, rollup=1)
    centroids$EID <- 1:nrow(centroids)

    found <- findPolys(as.EventData(centroids, projection=1), shp.regions)
    new.PID.region <- rep(NA, nrow(centroids))
    new.PID.region[found$EID] <- found$PID

    polydata.withattr$PID.region[is.na(polydata.withattr$PID.region)] <- new.PID.region[is.na(polydata.withattr$PID.region)]

    ## Now assign names that match regions in algalblooms

    polydata.withattr$region <- polydata.regions[polydata.withattr$PID.region, regattr]

    ## Construct area shares by region

    polydata.withattr %>% left_join(polydata.withattr %>% group_by(region) %>% summarize(PID=PID, area.share=area / sum(area),
                                                                                         pop.share=popsum / sum(popsum)),
                                    by='PID', suffix=c('', '.x'))
}
