setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

load("channels/flooding/combo-adm3.RData")

library(dplyr)

combo.uk <- combo %>% group_by(scenario, period, run_id) %>% summarize(damage.mgbp=sum(damage.mgbp))

source("lib/constants.R")

combo.uk$percent <- 100 * (combo.uk$damage.mgbp * 1e6) / gdp.2015.gbp

source("lib/report.R")

tbl <- make.table(combo.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

library(PBSmapping)

shp.adm3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
polydata.adm3 <- attr(shp.adm3, 'PolyData')

combo2 <- combo %>% left_join(polydata.adm3[, c('PID', 'NAME_3')], by=c('county'='NAME_3'))

make.maps.loop("channels/flooding/maps", "combo", combo2, shp.adm3, function(subres) {
        subres %>% group_by(PID) %>% summarize(mu=mean(damage.mgbp, na.rm=T), ci5=quantile(damage.mgbp, .05, na.rm=T), ci95=quantile(damage.mgbp, .95, na.rm=T))
    }, "Expected\nflooding\ndamage (£m)", "95% CI\nflooding\ndamage (£m)", -5, 50)
