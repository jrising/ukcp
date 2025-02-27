setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

algalblooms <- read.csv("channels/fisheries/algalbloom-projections.csv")

source("lib/disaggregate.R")
polydata.withattr2 <- disaggregate("regions/ukcp-spatial-files-master/spatial-files/ukcp18-uk-land-region-hires/ukcp18-uk-land-region-hires-latlon.shp",
             'geo_region')

## Make new damages by ADM3

algalblooms.adm3 <- algalblooms %>% left_join(polydata.withattr2[, c('NAME_3', 'region', 'area.share')], by='region')
algalblooms.adm3$damage.adm3 <- algalblooms.adm3$damage * algalblooms.adm3$area.share

save(algalblooms.adm3, file="channels/fisheries/algalbloom-projections-adm3.RData")
