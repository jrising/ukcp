setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)

source("lib/constants.R")
source("lib/report.R")

library(PBSmapping)
shp <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
polydata <- attr(shp, 'PolyData')
in.nireland <- polydata$NAME_3[polydata$NAME_1 == "Northern Ireland"]

for (do.scenario in c(1, 3, 'mix')) {
    load(paste0("channels/agriculture/arable-proj-", do.scenario, "-adm3.RData"))
    ## Replace N. Ireland with 0 damages
    byadm3.wlev$loss.mgbp[byadm3.wlev$county %in% in.nireland] <- 0

    agprod <- byadm3.wlev %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(loss.mgbp, na.rm=T))
    agprod$fracgdp <- agprod$damage * 1e6 / gdp.2015.gbp
    agprod$fracgdp.wCGE <- agprod$fracgdp * 2.13668464 # x PESETA III CGE effect

    agprod$percent <- agprod$fracgdp.wCGE * 100
    print(xtable(make.table(agprod, 'percent'), digits=2), include.rownames=F)
}

## Make other reports

load("channels/agriculture/agcombo-adm3.RData")

combo.uk <- combo %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage.mgbp, na.rm=T))
combo.uk$percent <- (combo.uk$damage.mgbp * 1e6 / gdp.2015.gbp) * 100
print(xtable(make.table(combo.uk, 'percent'), digits=2), include.rownames=F)

combo2 <- combo %>% left_join(polydata[, c('PID', 'NAME_3')], by=c('county'='NAME_3'))

make.maps.loop("channels/agriculture/maps", "agcombo", combo2, shp, function(subres) {
    subres %>% group_by(PID) %>% summarize(mu=mean(damage.mgbp, na.rm=T), ci5=quantile(damage.mgbp, .05, na.rm=T), ci95=quantile(damage.mgbp, .95, na.rm=T))
}, "Expected\nproduction\nloss (£m)", "95% CI\nproduction\nloss (£m)", -250, 250)

## Not sure if there's any use in what's below, now that we have agcombine-adm3.R

load("synthesis/pesetaiv-project-new.RData")
agprod2 <- subset(uk.results, channel == "Crop Productivity")

all(paste(agprod$scenario, agprod$run_id, agprod$period) ==
    paste(agprod2$scenario, agprod2$run_id, agprod2$period))

source("lib/combine.R")

maxrid <- max(agprod$run_id)
combo <- data.frame()
for (scn in unique(agprod$scenario)) {
    for (per in unique(agprod$period)) {
        print(c(scn, per))
        mcs <- cbind(subset(agprod, scenario == scn & period == per)$fracgdp.wCGE,
                     subset(agprod2, scenario == scn & period == per)$damage / 100)

        byrid <- hier.combine(mcs)
        combo <- rbind(combo, data.frame(scenario=scn, period=per, run_id=0:maxrid, fracgdp=byrid))
    }
}

save(combo, file="channels/agriculture/agcombo.RData")

combo$percent <- combo$fracgdp * 100
tbl <- make.table(combo, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

mean(combo$fracgdp[combo$scenario == 'ssp370' & combo$period == '2081-2100']) * gdp.2015.gbp / 1e9

