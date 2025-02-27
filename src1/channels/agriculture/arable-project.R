setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

do.scenario <- 1
## 1: w/o TP, no irrig
## 3: w/ TP, no irrig

df.grid <- read.csv("channels/agriculture/Smith/data/Ag_model_grid_2km.v2.csv")

library(raster)
rr <- raster("climate/worldclim/uk-present/uk_bio.nc", band=1)

coords <- coordinates(rr)
coords.lon <- unique(coords[, 1])
coords.lat <- unique(coords[, 2])

df.grid$lonii <- NA
df.grid$latii <- NA
for (ii in 1:nrow(df.grid)) {
    if (ii %% 100 == 0)
        print(ii / nrow(df.grid))
    df.grid$lonii[ii] <- which.min(abs(df.grid$lon[ii] - coords.lon))
    df.grid$latii[ii] <- which.min(abs(df.grid$lat[ii] - coords.lat))
}

library(dplyr)
df.grid2 <- df.grid %>% group_by(lonii, latii) %>% summarize(Area_ha=sum(Area_ha), lon=mean(lon), lat=mean(lat))

if (do.scenario != 3) {
    load(paste0("channels/agriculture/arable-coeffs-", do.scenario, ".RData")) # results
} else {
    load("channels/agriculture/arable-coeffs.RData") # results
}

## Project each grid cell
source("lib/aggregate.R")

df.grid2$havecoeff <- F
for (ii in 1:nrow(df.grid2)) {
    if (ii > length(results) || is.null(results[[ii]]))
        next
    df.grid2$havecoeff[ii] <- T
}

validrrdf <- rrdf[!is.na(rrdf$variable),]

grid2region <- rep(NA, nrow(validrrdf))
for (ii in 1:nrow(validrrdf)) {
    dists <- (df.grid2$lon - validrrdf$x[ii])^2 + (df.grid2$lat - validrrdf$y[ii])^2
    dists[!df.grid2$havecoeff] <- Inf
    grid2region[ii] <- which.min(dists)
}

## Use strings for region names
results2 <- list()
for (ii in 1:length(results)) {
    if (!is.null(results[[ii]]))
        results2[[as.character(ii)]] <- list(coeff=results[[ii]])
}
grid2region <- as.character(grid2region)

source("lib/project.R")

library(ncdf4)
nc <- nc_open("channels/agriculture/Boulton/Temp_35K_A1B_CO2.nc") # Standard HadGEM3/HadRM3-PPE-UK ECS
year <- nc$dim$year$vals

projs <- grid.project(results2, zero.y0=year[1], zero.y1=year[12], grid2region=grid2region)

projs %>% group_by(scenario, period) %>% summarize(cdiff=mean(cdiff), damage=mean(damage))

save(projs, file=paste0("channels/agriculture/arable-proj-", do.scenario, ".RData"))
## load("channels/agriculture/arable-proj.RData")

## Aggregate to ADM3 level

source("lib/aggregate.R")

byadm3 <- aggregate.gridded.county(projs, 'mean')

library(readxl)
agprod <- read_excel("channels/agriculture/regional/agr_r_accts__custom_2148028_spreadsheet.xlsx", sheet=13, skip=9)

weights.adm2 <- data.frame(nuts=agprod$TIME...1[nchar(agprod$TIME...1) == 4], region=agprod$TIME...2[nchar(agprod$TIME...1) == 4], agprod=apply(agprod[nchar(agprod$TIME...1) == 4, as.character(2016:2020)], 1, function(xx) mean(as.numeric(xx), na.rm=T)))

## Construct weights for ADM3
xwalk <- read.csv("regions/gadm2nuts.csv")
xwalk$NUTS2_ID <- sapply(xwalk$NUTS_ID, function(str) substring(str, 1, 4))

library(PBSmapping)
shp.withattr <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.shp")
polydata.withattr <- attr(shp.withattr, 'PolyData')

library(dplyr)
weights.adm3 <- weights.adm2 %>% left_join(xwalk, by=c('nuts'='NUTS2_ID')) %>% left_join(polydata.withattr[, c('PID', 'area')], by='PID')
## Distribute agricultural production by area
weights.adm3.est <- weights.adm3 %>% group_by(nuts) %>% summarize(county=NAME_3, est.agprod=max(agprod[1]) * area / sum(area))

byadm3.wlev <- byadm3 %>% left_join(weights.adm3.est)
byadm3.wlev$avg <- as.numeric(byadm3.wlev$avg)
byadm3.wlev$run_id <- as.numeric(byadm3.wlev$run_id)

byadm3.wlev$loss.mgbp <- (1 - exp(byadm3.wlev$avg)) * byadm3.wlev$est.agprod

save(byadm3.wlev, file=paste0("channels/agriculture/arable-proj-", do.scenario, "-adm3.RData"))
# load("channels/agriculture/arable-proj-adm3.RData")

source("lib/report.R")

library(dplyr)
agprod <- byadm3.wlev %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(loss.mgbp, na.rm=T) / 1000) # bgbp

library(xtable) # Not used: agcombine table so consistent
print(xtable(make.table(agprod, 'damage'), digits=1), include.rownames=F)

shp.adm3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

## Get baseline arable
df.grid <- read.csv("channels/agriculture/Smith/data/Ag_model_grid_2km.v2.csv")
df.base <- read.csv("channels/agriculture/Ritchie et al ERL/ECO_AG_baseline.csv")

events <- data.frame(EID=1:nrow(df.grid), X=df.grid$lon, Y=df.grid$lat)
found <- findPolys(as.EventData(events, projection=1), shp.adm3)
found$arable <- df.base$arable_ha_6190[found$EID]

bypid <- found %>% group_by(PID) %>% summarize(arable=sum(arable, na.rm=T))
bypid$county <- shp.adm3.polydata$NAME_3[bypid$PID]

byadm3.wlev2 <- byadm3.wlev %>% left_join(bypid)
byadm3.wlev2$loss <- 1 - exp(byadm3.wlev2$avg)

byadm32 <- make.maps.loop("channels/agriculture/maps", paste0("arable", do.scenario), byadm3.wlev2, shp.adm3, function(subres) {
    subres %>% group_by(PID) %>% summarize(mu=mean(loss, na.rm=T), ci5=quantile(loss, .05, na.rm=T), ci95=quantile(loss, .95, na.rm=T))
}, "Expected\narable\nloss (%)", "95% CI\narable\nloss (%)", -1, 1, labels=percent)
