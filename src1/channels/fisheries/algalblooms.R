setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/damagefunc.R")
source("lib/project.R")

## Use data from Jones et al. (2020), Tbl. 7.

source("climate/ukcp18/load.R")

df <- read.csv("channels/fisheries/algalblooms2.csv")

ddf <- data.frame() # damage functions
pdf <- data.frame() # points
allla <- list() # region -> la
for (rr in 1:length(clim.region)) {
    print(clim.region[rr])
    dfrr <- df$Region == clim.region[rr]
    if (sum(dfrr) == 0)
        next

    cdiff <- c(yr2001.2010[rr], sc2C[rr], sc4C[rr]) - preind[rr]
    damages <- c(0, c(df$X2C[dfrr], df$X4C[dfrr]) - df$X2001.2010[dfrr])
    range <- c(105, 160) # From Pretty et al. range of cost estimates
    damages.ses <- damages * diff(range) / mean(range)

    la <- fit.damages(cdiff, damages, damages.ses)

    ## Record for plotting
    pdf <- rbind(pdf, data.frame(region=clim.region[rr], T=cdiff, mu=damages - mean(la$coeff[, 1]), damages.ses))

    rrddf <- data.frame(region=clim.region[rr], T=seq(0, 6, length.out=100))
    rrddf$mu <- sapply(rrddf$T, function(TT) mean(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2))
    rrddf$ci25 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .25))
    rrddf$ci75 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .75))
    ddf <- rbind(ddf, rrddf)

    allla[[clim.region[rr]]] <- la
}

library(ggplot2)

ggplot(ddf, aes(T, mu)) +
    facet_wrap(~ region, scales="free_y") +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_point(data=pdf) +
    scale_x_continuous("Difference in temperature from 1900-1929 (C)", expand=c(0, 0)) +
    theme_bw() + ylab("Change in costs due to algal blooms (£ million)")

## Load historical data, make 1900-1929 average
## I DON'T USE THIS-- JUST REMOVE BASELINE PERIOD WARMING FROM GLOBAL
bestnc <- nc_open(file.path(datapath, "climate/best/Europe_TAVG_LatLong0.25.nc"))
bestyear <- ncvar_get(bestnc, "time")
baseline.inds <- bestyear >= 1900 & bestyear < 1930

historical <- read.csv(file.path(datapath, "climate/ukcp18/best/Europe_TAVG_LatLong0.25.csv"))
baseline.cols <- paste0("tas", which(baseline.inds)-1)
baseline.tas <- rowMeans(historical[, baseline.cols])
names(baseline.tas) <- historical$geo_region

## ## Need region temps corresponding to 1995 - 2014 for each GCM-scenario
## temps.1995.2014 <- data.frame()
## for (scenario in unique(gmst2$scenario)) {
##     for (model in unique(gmst2$model)) {
##         values <- data.frame()
##         for (year in 1995:2014) {
##             cdiffs <- region.pattern.cdiff(model, scenario, year)
##             valrow <- data.frame(year)
##             for (region in names(cdiffs))
##                 valrow[, region] <- cdiffs[[region]]
##             values <- rbind(values, valrow)
##         }
##         tempsrows <- as.data.frame(colMeans(values))
##         names(tempsrows) <- 'cdiff'
##         tempsrows$region <- rownames(tempsrows)

##         temps.1995.2014 <- rbind(temps.1995.2014, cbind(scenario, model, tempsrows))
##     }
## }

## Project for periods and regions
results <- ukcp18.project(allla)

write.csv(results, "algalbloom-projections.csv", row.names=F)

algalblooms <- read.csv("channels/fisheries/algalbloom-projections.csv")
library(dplyr)
algalblooms2 <- algalblooms %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))

source("lib/report.R")

tbl <- make.table(algalblooms2, 'damage')
print(xtable(tbl, digits=2), include.rownames=F)

## Make maps at region level
polydata.adm3 <- reg2adm3("regions/ukcp-spatial-files-master/spatial-files/ukcp18-uk-land-region-hires/ukcp18-uk-land-region-hires-latlon.shp") # adds geo_region

algalblooms2 <- algalblooms %>% left_join(polydata.reg[, c('PID', 'geo_region')], by=c('region'='geo_region'))
all(!is.na(algalblooms2$PID))

source("lib/report.R")

bypid <- make.maps.loop("channels/fisheries/maps", "region", algalblooms2, shp.reg, function(subres) {
    subres %>% group_by(PID) %>% summarize(mu=mean(damage, na.rm=T), ci05=quantile(damage, .05, na.rm=T), ci95=quantile(damage, .95, na.rm=T))
}, 'Expected\nalgal bloom\ndamage (£m)', '95% CI\nalgal bloom\ndamage (£m)', 0, 50)

## ## Match up with ADM3 regions, just by area

## algalblooms2 <- algalblooms %>% left_join(polydata.adm3[, c('PID', 'NAME_3', 'geo_region')], by=c('region'='geo_region'))
## all(!is.na(algalblooms2$NAME_3))

## algalblooms2 %>% left_join(polydata.adm3[, c('PID', 'area')])
