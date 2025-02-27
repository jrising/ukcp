#Mapping for agriculture

load("~/Dropbox/COACCH/channels/Agriculture/results-final.RData")

source("~/Dropbox/UK Economic Risks/lib/report.R")
source("~/Dropbox/UK Economic Risks/lib/constants.R")

library(dplyr)
byadm3.match.uk <- byadm3.match %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage))

byadm3.match.uk$percent <- byadm3.match.uk$damage.mgbp / gdp.2015.gbp * 100 * 1e6
tbl <- make.table(byadm3.match.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

setwd("~/Dropbox/COACCH/channels/Agriculture")

shp.adm3 <- importShapefile("~/Dropbox/UK Economic Risks/regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

quantile(byadm3.match$damage)

make.maps.loop("graphs", "for", byadm3.match, shp.adm3, function(subres) {
  subres %>% group_by(PID) %>% summarize(mu=mean(damage, na.rm=T), ci5=quantile(damage, .05, na.rm=T),
                                         ci95=quantile(damage, .95, na.rm=T))
}, "Agriculture\ndamages\n(% GDP)", "95% CI\nagriculture\ndamage\n(% GDP)", -10, 10)

load("~/Dropbox/COACCH/channels/Agriculture/agcombo-adm3.RData")

source("~/Dropbox/UK Economic Risks/lib/report.R")
source("~/Dropbox/UK Economic Risks/lib/constants.R")

library(dplyr)
combo.uk <- combo %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage.mgbp, na.rm=T))

combo.uk$percent <- combo.uk$damage.mgbp * 1e6 / gdp.2015.gbp * 100
tbl <- make.table(combo.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)
