setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/damagefunc.R")
source("lib/project.R")

## Use data from Jones et al. (2020), Tbl. 17

source("climate/ukcp18/load.R")

## Determine uncertainty
#Data source: Hsu and Levine 1976, Rose 1963
dat = data.frame(D50 = c(5.75, 3.1, 3, 2.7, 4, 10, 14,22.5), temp = c(20,25,30,35,25.5,20.5,15.5,10.5))

#Calculate the instantaneous rate
instantaneous.dev = -log(0.5)/dat$D50

#Fit a linear model
mod = lm(instantaneous.dev~dat$temp)
summary(mod)

#Find minimum temperature threshold for development
library(MASS)
draws <- mvrnorm(1e6, coef(mod), vcov(mod))
dev.thresholds = -draws[, 1]/draws[, 2]
unc.ratio <- sd(dev.thresholds) / mean(dev.thresholds)

df <- read.csv("channels/agriculture/lambs-data.csv")

inputs <- data.frame()
for (ii in 1:nrow(df)) {
    if (df$region[ii] == 'Scotland (total)') {
        rrs <- grep("Scotland", clim.regions)
        cdiffs <- c(mean(yr2001.2010[rrs]), mean(sc2C[rrs]), mean(sc4C[rrs])) - mean(preind[rrs])
    } else {
        rr <- which(df$region[ii] == clim.regions)
        cdiffs <- c(yr2001.2010[rr], sc2C[rr], sc4C[rr]) - preind[rr]
    }

    damages <- c(0, c(df$X2C[ii], df$X4C[ii]) - df$baseline[ii])
    damages.ses <- damages * unc.ratio
    inputs <- rbind(inputs, data.frame(region=df$region[ii], cdiffs, damages, damages.ses))
}

fits <- fit.damages.byregion(inputs$region, inputs$cdiffs, inputs$damages, inputs$damages.ses)

save(fits, file="channels/agriculture/lambs-fits.RData")
## load("channels/agriculture/lambs-fits.RData")

library(ggplot2)

ggplot(fits$ddf, aes(T, mu)) +
    facet_wrap(~ region, scales="free_y") +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_point(data=fits$pdf) +
    scale_x_continuous("Difference in temperature from 1900-1929 (C)", expand=c(0, 0)) +
    theme_bw() + ylab("Annual lamb production losses (£ million)")

grid2region <- get.grid2region("regions/ukcp-spatial-files-master/spatial-files/ukcp18-uk-land-region-hires/ukcp18-uk-land-region-hires-latlon.shp")

grid2region[grep("Scotland", grid2region)] <- "Scotland (total)"
grid2region[grid2region == "London"] <- "South East England"

results <- grid.project(fits$allla, 1900, 1930, grid2region=grid2region)

save(results, file="channels/agriculture/lambs-proj.RData")

source("climate/ukcp18/load.R")

results.byregion <- ukcp18.project(fits$allla, transform.cdiff=function(cdiffs) {
    cdiffs$`Scotland (total)` <- (cdiffs$`East Scotland` + cdiffs$`North Scotland` + cdiffs$`West Scotland`) / 3
    cdiffs
})

save(results, results.byregion, file="channels/agriculture/lambs-proj.RData")
## load("channels/agriculture/lambs-proj.RData")

library(dplyr)
results.uk <- results.byregion %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))

source("lib/report.R")
tbl <- make.table(results.uk, 'damage')
print(xtable(tbl, digits=2), include.rownames=F)

library(readxl)

lambs <- read_excel("channels/agriculture/lambs/agr_r_accts__custom_2217079_spreadsheet.xlsx", sheet=3, skip=9)
weights.adm1 <- data.frame(nuts=lambs$TIME...1[nchar(lambs$TIME...1) == 3], region=lambs$TIME...2[nchar(lambs$TIME...1) == 3], lambs=apply(lambs[nchar(lambs$TIME...1) == 3, as.character(2016:2020)], 1, function(xx) mean(as.numeric(xx), na.rm=T)))
weights.adm2 <- data.frame(nuts=lambs$TIME...1[nchar(lambs$TIME...1) == 4], region=lambs$TIME...2[nchar(lambs$TIME...1) == 4], lambs=apply(lambs[nchar(lambs$TIME...1) == 4, as.character(2016:2020)], 1, function(xx) mean(as.numeric(xx), na.rm=T)))

## Construct weights for ADM3
xwalk <- read.csv("regions/gadm2nuts.csv")
xwalk$NUTS2_ID <- sapply(xwalk$NUTS_ID, function(str) substring(str, 1, 4))

library(PBSmapping)
shp.withattr <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.shp")
polydata.withattr <- attr(shp.withattr, 'PolyData')

library(dplyr)
weights.adm3 <- weights.adm2 %>% left_join(xwalk, by=c('nuts'='NUTS2_ID')) %>% left_join(polydata.withattr[, c('PID', 'area')], by='PID')
weights.adm3$est.lambs <- weights.adm3$lambs * weights.adm3$area

## Bring gridded to ADM3

source("lib/aggregate.R")

byadm3 <- aggregate.gridded.county(results, 'sum')

save(byadm3, file="channels/agriculture/lambs-final.RData")

## load("channels/agriculture/lambs-proj.RData")
## load("channels/agriculture/lambs-final.RData")

pop <- get.gridded.pop()
totpop <- sum(pop$w001001[pop$is.uk], na.rm=T)

byadm3$damage <- as.numeric(byadm3$tot) / totpop

## force.match each ADM3 to region

polydata.adm3 <- reg2adm3("regions/ukcp-spatial-files-master/spatial-files/ukcp18-uk-land-region-hires/ukcp18-uk-land-region-hires-latlon.shp") # adds geo_region
polydata.adm3$geo_region[polydata.adm3$geo_region %in% c("East Scotland", "West Scotland", "North Scotland")] <- "Scotland (total)"

byadm3.match <- full.force.match(byadm3, results.byregion, polydata.adm3)

save(byadm3, byadm3.match, file="channels/agriculture/lambs-final.RData")

## testit <- subset(byadm3.match, scenario == 'ssp370' & run_id == 3 & period == '2081-2100' & geo_region == 'East Midlands')
## force.match.vector(testit$damage.adm3.scaled, testit$damage.reg[1])
## testit %>% group_by(scenario, run_id, period, geo_region) %>% summarize(PID=PID, county=county, damage=force.match.vector(damage.adm3.scaled, damage.reg[1])) %>% group_by(run_id, geo_region) %>% summarize(damage=sum(damage)) %>% group_by(geo_region) %>% summarize(damage=mean(damage))

source("lib/report.R")

subset(results.byregion, scenario == 'ssp370' & period == '2081-2100') %>% group_by(region) %>% summarize(damage=mean(damage))
subset(byadm3.match, scenario == 'ssp370' & period == '2081-2100') %>% group_by(run_id, geo_region) %>% summarize(damage=sum(damage)) %>% group_by(geo_region) %>% summarize(damage=mean(damage))

bypid <- make.maps.loop("channels/agriculture/maps", "lambs", byadm3.match, shp.adm3, function(subres) {
    subres %>% group_by(PID) %>% summarize(mu=mean(damage, na.rm=T), ci05=quantile(damage, .05, na.rm=T), ci95=quantile(damage, .95, na.rm=T))
}, 'Expected\nproduction\nloss (£m)', '95% CI\nproduction\nloss (£m)', 0, 1)

## for (reg in unique(results.byregion$region)) {
##     inreg <- byadm3$county %in% polydata.adm3$NAME_3[polydata.adm3$geo_region == reg]
##     for (scn in unique(regional$scenario)) {
##         for (rid in unique(regional$run_id[regional$scenario == scn])) {
##             for (per in unique(regional$period)) {
##                 print(c(reg, scn, rid, per))

##                 rows <- which(inreg & byadm3$scenario == scn & byadm3$run_id == rid & byadm3$period == per)

## }


##             list("North East (UK)"="North East England", "North West (UK)"="North West England",
##      "Yorkshire and The Humber"="Yorkshire and Humber", "East Midlands (UK)"="East Midlands",
##      "West Midlands (UK)"="West Midlands", "East of England"="East of England")

