setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)

gdp.2015.gbp <- 2089276e6 # GBP, from https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2

## Algal blooms
algalblooms <- read.csv("channels/fisheries/algalbloom-projections.csv")
algalblooms2 <- algalblooms %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))
algalblooms2$fracgdp <- algalblooms2$damage * 1e6 / gdp.2015.gbp
algalblooms2.merge <- algalblooms2[, c('period', 'scenario', 'run_id', 'fracgdp')]

algalblooms2$percent <- algalblooms2$fracgdp * 100
tbl <- make.table(algalblooms2, 'percent')
print(xtable(tbl, digits=4), include.rownames=F)

## Milk production
load("channels/agriculture/milk-projs.RData") # results.byregion
milkprod2 <- results.byregion %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))
milkprod2$fracgdp <- milkprod2$damage * 1e6 / gdp.2015.gbp
milkprod2.merge <- milkprod2[, c('period', 'scenario', 'run_id', 'fracgdp')]

milkprod2$percent <- milkprod2$fracgdp * 100
tbl <- make.table(milkprod2, 'percent')
print(xtable(tbl, digits=4), include.rownames=F)

## Lambs
load("channels/agriculture/lambs-proj.RData")
results.lambs <- results.byregion %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))
results.lambs$fracgdp <- results.lambs$damage * 1e6 / gdp.2015.gbp
results.lambs.merge <- results.lambs[, c('period', 'scenario', 'run_id', 'fracgdp')]

results.lambs$percent <- results.lambs$fracgdp * 100
tbl <- make.table(results.lambs, 'percent')
print(xtable(tbl, digits=4), include.rownames=F)

alldamage <- algalblooms2.merge %>%
    left_join(milkprod2, by=c('scenario', 'period', 'run_id'), suffix=c('', '.milk')) %>%
    left_join(results.lambs.merge, by=c('scenario', 'period', 'run_id'), suffix=c('.alga', '.lamb'))

source("lib/report.R")

alldamage$percent <- (alldamage$fracgdp.alga + alldamage$fracgdp.milk + alldamage$fracgdp.lamb) * 100
tbl <- make.table(alldamage, 'percent')
print(xtable(tbl, digits=4), include.rownames=F)

## Combined maps

load("channels/fisheries/algalbloom-projections-adm3.RData") # algalblooms.adm3
load("channels/agriculture/milk-final.RData") # byadm3.match
milk.adm3 <- byadm3.match
load("channels/agriculture/lambs-final.RData") # byadm3.match
lamb.adm3 <- byadm3.match

## Load PESETA for full county list
load("synthesis/pesetaiv-final-Droughts.RData") # byadm3.matched
allcounty <- byadm3.matched[, c('scenario', 'period', 'run_id', 'county')]

alldamage <- allcounty %>%
    left_join(milk.adm3, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.milk')) %>%
    left_join(lamb.adm3, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.lamb')) %>%
    left_join(algalblooms.adm3, by=c('scenario', 'period', 'run_id', 'county'='NAME_3'), suffix=c('', '.fish'))

alldamage$total <- alldamage$damage + alldamage$damage.lamb + alldamage$damage.adm3 # milk, lamb, fish

shp.adm3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

alldamage2 <- alldamage %>% left_join(shp.adm3.polydata[, c('PID', 'NAME_3')], by=c('county'='NAME_3'), suffix=c('.geo', ''))

source("lib/report.R")
make.maps.loop("channels/agriculture/maps", "livestock", alldamage2, shp.adm3, function(subres) {
    subres %>% group_by(PID) %>% summarize(mu=mean(total, na.rm=T), ci5=quantile(total, .05, na.rm=T), ci95=quantile(total, .95, na.rm=T))
}, "Expected\nlivestock\nloss (£m)", "95% CI\nlivestock\nloss (£m)", 0, 10)
