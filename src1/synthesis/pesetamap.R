setwd("~/Open Modeling Group Dropbox/UK Economic Risks")
library(dplyr)
library(PBSmapping)

source("lib/aggregate.R")

for (chan in unique(uk.results$channel)) {
    print(chan)
    load(paste0("synthesis/pesetaiv-project-", chan, ".RData"))

    byadm3 <- aggregate.gridded.county(results, 'sum')
    save(results, byadm3, file=paste0("synthesis/pesetaiv-project-", chan, ".RData"))
}

source("lib/report.R")

## PESETA IV
load("synthesis/pesetaiv-project-new.RData")
## Drop PESETA IV coastal floods: get implausible negative values, believe CIAM more
uk.results <- subset(uk.results, channel != "Coastal floods")

for (chan in unique(uk.results$channel)) {
    uk.results.sub <- subset(uk.results, channel == chan)

    load(paste0("synthesis/pesetaiv-project-", chan, ".RData"))
    byadm3$tot <- as.numeric(byadm3$tot)
    byadm3$run_id <- as.numeric(byadm3$run_id)

    byadm3.touk <- byadm3 %>% group_by(scenario, run_id, period) %>% summarize(tot=sum(tot)) %>% left_join(uk.results.sub, by=c('scenario', 'run_id', 'period'), suffix=c('', '.uk'))
    mod <- lm(damage ~ 0 + tot, data=byadm3.touk)
    byadm3$damage.scaled <- byadm3$tot * mod$coeff[1]

    byadm32 <- byadm3 %>% left_join(uk.results.sub, by=c('scenario', 'run_id', 'period'), suffix=c('', '.uk'))
    byadm3.matched <- byadm32 %>% group_by(scenario, run_id, period) %>% summarize(county=county, damage=force.match.vector(damage.scaled, damage[1]))

    ## Checks
    uk.results.sub %>% group_by(scenario, period) %>% summarize(damage=mean(damage))
    byadm3.matched %>% group_by(scenario, period, run_id) %>% summarize(damage=sum(damage)) %>% group_by(scenario, period) %>% summarize(damage=mean(damage))

    save(byadm3.matched, file=paste0("synthesis/pesetaiv-final-", chan, ".RData"))
}

lobounds <- list('River floods'=0, 'Crop Productivity'=0, 'Droughts'=0, 'Electricity Production'=0, 'Heat and Cold Mortality'=0)
hibounds <- list('River floods'=0.005, 'Crop Productivity'=0.001, 'Droughts'=0.003, 'Electricity Production'=0.0001, 'Heat and Cold Mortality'=.005)

for (chan in unique(uk.results$channel)) {
    load(paste0("synthesis/pesetaiv-final-", chan, ".RData"))

    shp.adm3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
    shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

    byadm3.map <- byadm3.matched %>% left_join(shp.adm3.polydata[, c('PID', 'NAME_3')], by=c('county'='NAME_3'))

    make.maps.loop("synthesis/maps", chan, byadm3.map, shp.adm3, function(subres) {
        subres %>% group_by(PID) %>% summarize(mu=mean(damage, na.rm=T), ci5=quantile(damage, .05, na.rm=T), ci95=quantile(damage, .95, na.rm=T))
    }, "Expected\nGDP\ndamage (%)", "1-in-20\nGDP\ndamage (%)", lobounds[[chan]], hibounds[[chan]], labels=percent)
}
