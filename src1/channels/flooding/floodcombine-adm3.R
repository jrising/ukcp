### Combine by adm3

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)

source("lib/constants.R")

load("channels/flooding/flooding-final.RData")
byadm3.match$loss.mgbp.wCGE <- byadm3.match$damage * 1.24570131 / 1e6 # x PESETA III CGE effect

load("synthesis/pesetaiv-final-River Floods.RData")
byadm3.matched$damage.mgbp <- (byadm3.matched$damage / 100) * gdp.2015.gbp / 1e6

## Need to re-order byadm3.match
byadm3.sayers <- byadm3.matched %>% left_join(byadm3.match, by=c('scenario', 'run_id', 'period', 'county'), suffix=c('.piv', ''))

all(paste(byadm3.sayers$scenario, byadm3.sayers$run_id, byadm3.sayers$period, byadm3.sayers$county) ==
    paste(byadm3.matched$scenario, byadm3.matched$run_id, byadm3.matched$period, byadm3.matched$county))

source("lib/combine.R")

maxrid <- max(byadm3.sayers$run_id)

## combo <- data.frame()
load("channels/flooding/combo-adm3.RData")
known <- unique(paste(combo$scenario, combo$period, combo$county))

for (scn in unique(byadm3.sayers$scenario)) {
    for (per in unique(byadm3.sayers$period)) {
        for (cty in unique(byadm3.sayers$county)) {
            if (paste(scn, per, cty) %in% known)
                next

            print(c(scn, per, cty))
            mcs <- cbind(subset(byadm3.sayers, scenario == scn & period == per & county == cty)$loss.mgbp.wCGE,
                         subset(byadm3.matched, scenario == scn & period == per & county == cty)$damage.mgbp)

            byrid <- hier.combine(mcs)
            combo <- rbind(combo, data.frame(scenario=scn, period=per, county=cty, run_id=0:maxrid, damage.mgbp=byrid))

            if (cty == unique(byadm3.sayers$county)[200])
                save(combo, file="channels/flooding/combo-adm3.RData")
        }

        save(combo, file="channels/flooding/combo-adm3.RData")
    }
}
