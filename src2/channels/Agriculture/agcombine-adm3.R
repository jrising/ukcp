### Combine by adm3

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)

source("lib/constants.R")

load("channels/agriculture/arable-proj-mix-adm3.RData")
byadm3.wlev$loss.mgbp.wCGE <- byadm3.wlev$loss.mgbp * 2.13668464 # x PESETA III CGE effect
byadm3.wlev <- byadm3.wlev %>% arrange(scenario, run_id, period, county)

load("synthesis/pesetaiv-final-Crop Productivity.RData")
byadm3.peseta <- byadm3.matched
byadm3.peseta$damage.mgbp <- (byadm3.peseta$damage / 100) * gdp.2015.gbp / 1e6
byadm3.peseta <- byadm3.peseta %>% arrange(scenario, run_id, period, county)

load("~/Open Modeling Group Dropbox/COACCH/channels/Agriculture/results-final.RData")
byadm3.coacch <- byadm3.match
byadm3.coacch$damage.mgbp <- byadm3.coacch$damage*(-1)
byadm3.coacch <- byadm3.coacch %>% arrange(scenario, run_id, period, county)

## Check that scenario, run_id, ... order is the same
all(paste(byadm3.wlev$scenario, byadm3.wlev$run_id, byadm3.wlev$period, byadm3.wlev$county) ==
    paste(byadm3.peseta$scenario, byadm3.peseta$run_id, byadm3.peseta$period, byadm3.peseta$county))

all(paste(byadm3.wlev$scenario, byadm3.wlev$run_id, byadm3.wlev$period, byadm3.wlev$county) ==
    paste(byadm3.coacch$scenario, byadm3.coacch$run_id, byadm3.coacch$period, byadm3.coacch$county))

library(PBSmapping)
adm3info <- attr(importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.shp"), 'PolyData')
northirelands <- adm3info$NAME_3[adm3info$NAME_1 == "Northern Ireland"]

source("lib/combine.R")

## First time: set combo to blank df
 combo <- data.frame()
## All future times: Load it

if (file.exists("../COACCH/channels/Agriculture/agcombo-adm3.RData"))
    load("../COACCH/channels/Agriculture/agcombo-adm3.RData")

known <- unique(paste(combo$scenario, combo$period, combo$county))
maxrid = max(byadm3.wlev$run_id)

for (scn in unique(byadm3.wlev$scenario)) {
    for (per in unique(byadm3.wlev$period)) {
        for (cty in unique(byadm3.wlev$county)) {
            if (paste(scn, per, cty) %in% known)
                next

            print(c(scn, per, cty))
            if (cty %in% northirelands || cty %in% c("North Ayrshire", "Orkney Islands", "Portsmouth")) {
                mcs <- cbind(subset(byadm3.peseta, scenario == scn & period == per & county == cty)$damage.mgbp,
                             subset(byadm3.coacch, scenario == scn & period == per & county == cty)$damage.mgbp)
            } else {
                mcs <- cbind(subset(byadm3.wlev, scenario == scn & period == per & county == cty)$loss.mgbp.wCGE,
                             subset(byadm3.peseta, scenario == scn & period == per & county == cty)$damage.mgbp,
                             subset(byadm3.coacch, scenario == scn & period == per & county == cty)$damage.mgbp)
            }
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
                save(combo, file="../COACCH/channels/Agriculture/agcombo-adm3.RData")
        }

        save(combo, file="../COACCH/channels/Agriculture/agcombo-adm3.RData")
    }
}

load("~/Open Modeling Group Dropbox/COACCH/channels/Agriculture/agcombo-adm3.RData")
source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/report.R")
source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/constants.R")

library(dplyr)
combo.uk <- combo %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage.mgbp, na.rm=T))

combo.uk$percent <- combo.uk$damage.mgbp * 1e6 / gdp.2015.gbp * 100
tbl <- make.table(combo.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)
