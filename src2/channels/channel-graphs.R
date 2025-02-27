#Plotting

channel = "Transport"


load(file = file.path("~/Dropbox/COACCH/channels",
                      channel, "results-final.RData"))
byadm3.match$damage <- -byadm3.match$damage

source("~/Dropbox/UK Economic Risks/lib/report.R", chdir = T)
source("~/Dropbox/UK Economic Risks/lib/constants.R")

library(dplyr)
byadm3.match.uk <- byadm3.match %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage))

byadm3.match.uk$percent <- byadm3.match.uk$damage.mgbp / gdp.2015.gbp * 100 * 1e6
tbl <- make.table(byadm3.match.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

setwd(file.path("~/Dropbox/COACCH/channels", channel))

shp.adm3 <- importShapefile("~/Dropbox/UK Economic Risks/regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

quantile(byadm3.match$damage, c(.05, .95))

limits <- list("Energy-Demand"=c(0, 250), "Energy-Supply"=c(0, 5), "Fishery"=c(-20, 20),
               "Forestry"=c(-25, 5), "Labor"=c(0, 350), "Agriculture" = c(-5, 10 ),
               "Riverine"=c(0,120), "SLR"=c(-15, 60), "SLR-Ada" = c(-12, 10), "Transport"=c(-3, 5))

make.maps.loop("graphs", "for", byadm3.match, shp.adm3, function(subres) {
  subres %>% group_by(PID) %>% summarize(mu=mean(damage, na.rm=T), ci5=quantile(damage, .05, na.rm=T),
                                         ci95=quantile(damage, .95, na.rm=T))
}, "Damages\n(million £)", "95% CI\nDamages\n(million £)", limits[[channel]][1], limits[[channel]][2])

