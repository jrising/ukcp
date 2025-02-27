### Combine by adm3

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)

source("lib/constants.R")

load("channels/agriculture/arable-proj-mix-adm3.RData")
byadm3.wlev$loss.mgbp.wCGE <- byadm3.wlev$loss.mgbp * 2.13668464 # x PESETA III CGE effect

load("synthesis/pesetaiv-final-Crop Productivity.RData")
byadm3.matched$damage.mgbp <- (byadm3.matched$damage / 100) * gdp.2015.gbp / 1e6

all(paste(byadm3.wlev$scenario, byadm3.wlev$run_id, byadm3.wlev$period, byadm3.wlev$county) ==
    paste(byadm3.matched$scenario, byadm3.matched$run_id, byadm3.matched$period, byadm3.matched$county))

source("lib/combine.R")

maxrid <- max(byadm3.wlev$run_id)

## combo <- data.frame()
load("channels/agriculture/agcombo-adm3.RData")
known <- unique(paste(combo$scenario, combo$period, combo$county))

for (scn in unique(byadm3.wlev$scenario)) {
    for (per in unique(byadm3.wlev$period)) {
        for (cty in unique(byadm3.wlev$county)) {
            if (paste(scn, per, cty) %in% known)
                next

            print(c(scn, per, cty))
            mcs <- cbind(subset(byadm3.wlev, scenario == scn & period == per & county == cty)$loss.mgbp.wCGE,
                         subset(byadm3.matched, scenario == scn & period == per & county == cty)$damage.mgbp)
            valid <- rowSums(is.na(mcs)) == 0
            if (sum(valid) < 100) {
                combo <- rbind(combo, data.frame(scenario=scn, period=per, county=cty, run_id=0:maxrid, damage.mgbp=NA))
                next
            } else if (sum(!valid) == 0) {
                byrid <- hier.combine(mcs)
                combo <- rbind(combo, data.frame(scenario=scn, period=per, county=cty, run_id=0:maxrid, damage.mgbp=byrid))
            } else {
                byrid <- hier.combine(mcs[valid,])
                allbyrid <- rep(NA, nrow(mcs))
                allbyrid[valid] <- byrid
                combo <- rbind(combo, data.frame(scenario=scn, period=per, county=cty, run_id=0:maxrid, damage.mgbp=allbyrid))
            }

            if (cty == unique(byadm3.wlev$county)[200])
                save(combo, file="channels/agriculture/agcombo-adm3.RData")
        }

        save(combo, file="channels/agriculture/agcombo-adm3.RData")
    }
}

## Replace N. Ireland with only PESETA damages
load("channels/agriculture/agcombo-adm3.RData")

load("synthesis/pesetaiv-final-Crop Productivity.RData")
byadm3.matched$damage.mgbp <- (byadm3.matched$damage / 100) * gdp.2015.gbp / 1e6

library(PBSmapping)
shp <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
polydata <- attr(shp, 'PolyData')

in.nireland <- polydata$NAME_3[polydata$NAME_1 == "Northern Ireland"]

for (county in in.nireland) {
    for (period in unique(combo$period)) {
        print(county)
        rows.combo <- which(combo$county == county & combo$period == period)
        rows.piv <- which(byadm3.matched$county == county & byadm3.matched$period == period)

        stopifnot(all(paste(combo$scenario[rows.combo], combo$period[rows.combo], combo$run_id[rows.combo]) ==
                      paste(byadm3.matched$scenario[rows.piv], byadm3.matched$period[rows.piv], byadm3.matched$run_id[rows.piv])))
        combo$damage.mgbp[rows.combo] <- byadm3.matched$damage.mgbp[rows.piv]
    }
}

save(combo, file="channels/agriculture/agcombo-adm3.RData")
