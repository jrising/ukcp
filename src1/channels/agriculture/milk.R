setwd("C:/Users/ritik/Open Modeling Group Dropbox/UK Economic Risks")
## setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/damagefunc.R")
source("lib/project.R")

## Use data from Jones et al. (2020), Tbl. 20.

source("climate/ukcp18/load.R")

df <- read.csv("channels/agriculture/milk.csv")

ddf <- data.frame() # damage functions
pdf <- data.frame() # points
allla <- list() # region -> la
for (rr in 1:length(clim.regions)) {
    print(clim.regions[rr])
    dfrr <- df$Region == clim.regions[rr]
    if (sum(dfrr) == 0 || is.na(df$baseloss[dfrr]))
        next

    cdiff <- c(yr2001.2010[rr], sc2C[rr], sc4C[rr]) - preind[rr]
    damages <- c(0, c(df$X2Dloss[dfrr], df$X4DLoss[dfrr]) - df$baseloss[dfrr])
    damages.ses <- damages * 12.2 / 31.4 #André et al. (2011)

    la <- fit.damages(cdiff, damages, damages.ses)

    ## Record for plotting
    pdf <- rbind(pdf, data.frame(region=clim.regions[rr], T=cdiff, mu=damages - mean(la$coeff[, 1]), damages.ses))

    rrddf <- data.frame(region=clim.regions[rr], T=seq(0, 6, length.out=100))
    rrddf$mu <- sapply(rrddf$T, function(TT) mean(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2))
    rrddf$ci25 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .25))
    rrddf$ci75 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .75))
    ddf <- rbind(ddf, rrddf)

    allla[[clim.regions[rr]]] <- la
}

library(ggplot2)

ggplot(ddf, aes(T, mu)) +
    facet_wrap(~ region, scales="free_y") +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_point(data=pdf) +
    scale_x_continuous("Difference in temperature from 1900-1929 (C)", expand=c(0, 0)) +
    theme_bw() + ylab("Change in costs due to milk (£ million)")

save(allla, file="channels/agriculture/milk-las.RData")

results.byregion <- ukcp18.project(allla)

save(results.byregion, file="channels/agriculture/milk-projs.RData")

source("lib/report.R")
results.uk <- results.byregion %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))

tbl <- make.table(results.uk, 'damage')
print(xtable(tbl, digits=2), include.rownames=F)

grid2region <- get.grid2region("regions/ukcp-spatial-files-master/spatial-files/ukcp18-uk-land-region-hires/ukcp18-uk-land-region-hires-latlon.shp")
results <- grid.project(allla, 1900, 1930, grid2region=grid2region)

save(results, results.byregion, file="channels/agriculture/milk-proj.RData")
## load("channels/agriculture/milk-proj.RData")

## Estimate cows

library(readxl)

cows <- read_excel("channels/agriculture/Milk Production Data.xlsx", sheet=3, skip=8) # Actually 1000t raw cow milk
weights.adm1 <- data.frame(nuts=cows$TIME...1[nchar(cows$TIME...1) == 3], region=cows$TIME...2[nchar(cows$TIME...1) == 3], cows=apply(cows[nchar(cows$TIME...1) == 3, as.character(2016:2020)], 1, function(xx) mean(as.numeric(xx), na.rm=T)))
weights.adm2 <- data.frame(nuts=cows$TIME...1[nchar(cows$TIME...1) == 4], region=cows$TIME...2[nchar(cows$TIME...1) == 4], cows=apply(cows[nchar(cows$TIME...1) == 4, as.character(2016:2020)], 1, function(xx) mean(as.numeric(xx), na.rm=T)))

## Construct weights for ADM3
xwalk <- read.csv("regions/gadm2nuts.csv")
xwalk$NUTS2_ID <- sapply(xwalk$NUTS_ID, function(str) substring(str, 1, 4))

library(PBSmapping)
shp.withattr <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.shp")
polydata.withattr <- attr(shp.withattr, 'PolyData')

library(dplyr)
weights.adm3 <- weights.adm2 %>% left_join(xwalk, by=c('nuts'='NUTS2_ID')) %>% left_join(polydata.withattr[, c('PID', 'area')], by='PID')
weights.adm3$est.cows <- weights.adm3$cows * weights.adm3$area

## Bring gridded to ADM3

source("lib/aggregate.R")
byadm3 <- aggregate.gridded.county(results, 'sum')

save(byadm3, file="channels/agriculture/milk-final.RData")

## load("channels/agriculture/milk-proj.RData")
## load("channels/agriculture/milk-final.RData")

pop <- get.gridded.pop()
totpop <- sum(pop$w001001[pop$is.uk], na.rm=T)
byadm3$damage <- as.numeric(byadm3$tot) / totpop

## force.match each ADM3 to region

polydata.adm3 <- reg2adm3("regions/ukcp-spatial-files-master/spatial-files/ukcp18-uk-land-region-hires/ukcp18-uk-land-region-hires-latlon.shp") # adds geo_region

byadm3.match <- full.force.match(byadm3, results.byregion, polydata.adm3)

save(byadm3, byadm3.match, file="channels/agriculture/milk-final.RData")

source("lib/report.R")

subset(results.byregion, scenario == 'ssp370' & period == '2081-2100') %>% group_by(region) %>% summarize(damage=mean(damage))
subset(byadm3.match, scenario == 'ssp370' & period == '2081-2100') %>% group_by(run_id, geo_region) %>% summarize(damage=sum(damage)) %>% group_by(geo_region) %>% summarize(damage=mean(damage))

bypid <- make.maps.loop("channels/agriculture/maps", "milk", byadm3.match, shp.adm3, function(subres) {
    subres %>% group_by(PID) %>% summarize(mu=mean(damage, na.rm=T), ci05=quantile(damage, .05, na.rm=T), ci95=quantile(damage, .95, na.rm=T))
}, 'Expected\nproduction\nloss (£m)', '95% CI\nproduction\nloss (£m)', 0, .5)
