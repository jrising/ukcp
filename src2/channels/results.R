#Results code

install.packages('maptools')
library(PBSmapping)
library(maptools)

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

channel = "SLR-AdA"
baseyear0 = 1860 #change these according to the channel
baseyear1 = 1880

mapping <- list("Energy-Supply"="energy-supply-la.RData", "Riverine"="riverine-la.RData",
                "Agriculture" = "agriculture-la.RData", "SLR" = "slr-la.RData", "SLR-AdA" = "slr-ada-la.RData",
                "Fishery" = "fishery-la.RData", "Energy-Demand" = "energy-demand-la.RData" )

load(file = file.path("~/Open Modeling Group Dropbox/COACCH/channels",
                      channel, mapping[[channel]]))

source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/project.R")
source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/disaggregate.R")

## 1. Determine the NUTS region for each grid cell
shp_nuts <- importShapefile("~/Open Modeling Group Dropbox/UK Economic Risks/regions/nuts2-2013/nuts2-2013-wgs84.shp")
polydata <- attr(shp_nuts, 'PolyData')

grid2region <- get.grid2region.shape(shp_nuts, polydata$NUTS_ID)

## 2. Project at a grid cell level
results <- grid.project(allla, baseyear0, baseyear1, grid2region=grid2region)

save(results, file= file.path("~/Open Modeling Group Dropbox/COACCH/channels", channel, "gridnuts-project.RData"))

## 3. Project at a NUTS2 level
aggregate.cdiff <- function(cdiffs) {
    cbind(cdiffs[!is.na(cdiffs$variable),], region=grid2region) %>%
        group_by(region) %>% summarize(x=mean(x), y=mean(y),
                                       variable=mean(variable))
}

# Determine x, y for each region
regionxy <- aggregate.cdiff(grid.pattern.cdiff('MIROC6', 'ssp126', 2020))
regionxy$x02 <- round(regionxy$x, 2)
regionxy$y02 <- round(regionxy$y, 2)
regionxy <- regionxy[, c('region', 'x02', 'y02')]

#Add this in each of the channel
uki4 <- regionxy$region == "UKI4"
regionxy <- rbind(regionxy, regionxy[uki4,])
regionxy$region[nrow(regionxy)] = "UKI3"

results.region <- grid.project(allla, baseyear0, baseyear1, grid2region=sort(unique(grid2region)),
                               aggregate.cdiff=aggregate.cdiff)

#drop last column of results.region
#results.region <-results.region[,1:8]


results.region <- results.region %>% mutate(x02=round(x, 2), y02=round(y, 2)) %>% left_join(regionxy) %>% dplyr::select(-c(x02, y02))


save(results, results.region, file= file.path("~/Open Modeling Group Dropbox/COACCH/channels", channel, "nuts-project.RData"))

## 4. Aggregate gridded to ADM3

source("lib/aggregate.R")
byadm3 <- aggregate.gridded.county(results, 'sum')

save(results, results.region, byadm3, file=file.path("~/Open Modeling Group Dropbox/COACCH/channels", channel, "results-project.RData"))

## Force-match ADM3 to regional

gdps.adm3 <- read.csv("socioeconomics/adm3inc.csv")
gdps.adm3$mgbp <- as.numeric(gdps.adm3$GDPpc) * gdps.adm3$pop / 1e6

polydata.adm3 <- reg2adm3(shp_nuts, 'NUTS_ID') # adds geo_region

gdps <- gdps.adm3 %>% left_join(polydata.adm3[, c('NAME_3', 'geo_region')], by=c('county'='NAME_3')) %>% group_by(geo_region) %>% summarize(mgbp=sum(mgbp))

results.region2 <- results.region %>% left_join(gdps, by=c('region'='geo_region'))
results.region2$damage <- results.region2$damage * results.region2$mgbp / 100
rows2keep <-grep("^UK..$",results.region$region)

byadm3$run_id <- as.numeric(byadm3$run_id)
byadm32 <- byadm3 %>% left_join(gdps.adm3 %>% 
                                  dplyr::select(`mgbp`, `county`))
byadm32$damage <- as.numeric(byadm32$tot) * byadm32$mgbp

byadm3.match <- full.force.match(byadm32, results.region2[rows2keep,], polydata.adm3)

save(byadm3.match, file=file.path("~/Open Modeling Group Dropbox/COACCH/channels/", channel, "results-final.RData"))
