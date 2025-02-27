setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)
source("lib/constants.R")

adm3info <- read.csv("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.csv")

load("channels/health/Bressleretal-adm3-final.RData") # byadm3.matched
byadm3.bea <- byadm3.matched
byadm3.bea$deathrate <- 100000 * uk.mortrate * byadm3.bea$damage / 100 # deaths / 100k

load("synthesis/pesetaiv-final-Heat and Cold Mortality.RData") # byadm3.matched
byadm3.piv <- byadm3.matched # VSL: 1.3 million euro
byadm3.piv$deaths <- (byadm3.piv$damage / 100) * gdp.2015.gbp / (1.3e6 * 0.7263) # https://www.exchangerates.org.uk/EUR-GBP-spot-exchange-rates-history-2015.html
byadm3.piv2 <- byadm3.piv %>% left_join(adm3info, by=c('county'='NAME_3'))
byadm3.piv$deathrate <- 100000 * byadm3.piv2$deaths / byadm3.piv2$popsum

all(paste(byadm3.bea$scenario, byadm3.bea$run_id, byadm3.bea$period, byadm3.bea$county) ==
    paste(byadm3.piv$scenario, byadm3.piv$run_id, byadm3.piv$period, byadm3.piv$county))

source("lib/combine.R")

maxrid <- max(byadm3.bea$run_id)

## combo <- data.frame()
load("channels/health/combo-adm3.RData")
known <- unique(paste(combo$scenario, combo$period, combo$county))

for (scn in unique(byadm3.bea$scenario)) {
    for (per in unique(byadm3.bea$period)) {
        for (cty in unique(byadm3.bea$county)) {
            if (paste(scn, per, cty) %in% known)
                next

            print(c(scn, per, cty))
            mcs <- cbind(subset(byadm3.bea, scenario == scn & period == per & county == cty)$deathrate,
                         subset(byadm3.piv, scenario == scn & period == per & county == cty)$deathrate)
            byrid <- hier.combine(mcs)
            combo <- rbind(combo, data.frame(scenario=scn, period=per, county=cty, run_id=0:maxrid, deathrate=byrid))

            if (cty == unique(byadm3.bea$county)[200])
                save(combo, file="channels/health/combo-adm3.RData")
        }

        save(combo, file="channels/health/combo-adm3.RData")
    }
}

if (F) {
    library(dplyr)
    source("lib/report.R")

    adm3info <- read.csv("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.csv")

    ## Produce deathrate tables
    byadm3.bea.uk <- byadm3.bea %>% left_join(adm3info, by=c('county'='NAME_3')) %>%
        group_by(scenario, period, run_id) %>% summarize(damage=sum(damage * popsum) / sum(popsum),
                                                         deaths=sum(deathrate * popsum) / 1e5,
                                                         deathrate=sum(deathrate * popsum) / sum(popsum))
    print(xtable(make.table(byadm3.bea.uk, 'deathrate'), digits=2), include.rownames=F)
    byadm3.bea.uk$percent <- 100 * byadm3.bea.uk$deaths * vpf.2016 / gdp.2015.gbp
    print(xtable(make.table(byadm3.bea.uk, 'percent'), digits=2), include.rownames=F)

    byadm3.piv.uk <- byadm3.piv %>% left_join(adm3info, by=c('county'='NAME_3')) %>%
        group_by(scenario, period, run_id) %>% summarize(deaths=sum(deathrate * popsum) / 1e5,
                                                         deathrate=sum(deathrate * popsum) / sum(popsum))

    print(xtable(make.table(byadm3.piv.uk, 'deathrate'), digits=2), include.rownames=F)
    byadm3.piv.uk$percent <- 100 * byadm3.piv.uk$deaths * vpf.2016 / gdp.2015.gbp
    print(xtable(make.table(byadm3.piv.uk, 'percent'), digits=2), include.rownames=F)

    load("channels/health/combo-adm3.RData")
    combo2 <- combo %>% left_join(adm3info, by=c('county'='NAME_3'))

    combo.uk <- combo2 %>% group_by(scenario, period, run_id) %>% summarize(deaths=sum(deathrate * popsum) / 1e5,
                                                                            deathrate=sum(deathrate * popsum) / sum(popsum))
    print(xtable(make.table(combo.uk, 'deathrate'), digits=2), include.rownames=F)
    combo.uk$percent <- 100 * combo.uk$deaths * vpf.2016 / gdp.2015.gbp
    print(xtable(make.table(combo.uk, 'percent'), digits=2), include.rownames=F)
}
