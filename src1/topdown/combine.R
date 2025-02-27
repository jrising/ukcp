setwd("~/Dropbox/UK Economic Risks")

source("topdown/driver.R")

load("topdown/burkeetal-project.RData")
results2.bea <- byscen(results, function(results) {
    results2 <- results %>% group_by(run_id, ADM0) %>% summarize(year=year, impact=cumsum(dimpact))
    results2$period <- NA
    for (per in c(2020, 2050, 2090))
        results2$period[results2$year > per - 10 & results2$year <= per + 10] <- paste0(per - 9, "-", per + 10)
    results2 %>% group_by(period, run_id, ADM0) %>% summarize(impact=mean(impact, na.rm=T))
})

load("topdown/djo-project.RData")
results2.djo <- byscen(results, function(results) {
    results$impact <- results$dimpact
    results$period <- NA
    for (per in c(2020, 2050, 2090))
        results$period[results$year > per - 10 & results$year <= per + 10] <- paste0(per - 9, "-", per + 10)
    results %>% group_by(period, run_id, ADM0) %>% summarize(impact=mean(impact, na.rm=T))
})

load("topdown/kahnetal-project.RData")
results2.kea <- byscen(results, function(results) {
    results$impact <- results$dimpact
    results$period <- NA
    for (per in c(2020, 2050, 2090))
        results$period[results$year > per - 10 & results$year <= per + 10] <- paste0(per - 9, "-", per + 10)
    results %>% group_by(period, run_id, ADM0) %>% summarize(impact=mean(impact, na.rm=T))
})

source("lib/combine.R")

## Combine at the ADM level, so don't need to exponentiate yet
maxrid <- max(results2.bea[['ssp370']]$run_id)
## combo <- data.frame()
load("topdown/combo.RData")
for (scn in c('ssp126', 'ssp370')) {
    for (per in unique(results2.bea[[scn]]$period)) {
        if (is.na(per))
            next
        if (any(combo$scenario == scn & combo$period == per))
            next
        print(c(scn, per))
        for (adm in unique(results2.bea[[scn]]$ADM0)) {
            mcs <- cbind(subset(results2.bea[[scn]], period == per & ADM0 == adm)$impact,
                         subset(results2.djo[[scn]], period == per & ADM0 == adm)$impact,
                         subset(results2.kea[[scn]], period == per & ADM0 == adm)$impact)
            ## Drop if NAs
            mcs <- mcs[, apply(mcs, 2, function(col) all(!is.na(col)))]
            if (ncol(mcs) == 0)
                next

            byrid <- hier.combine(mcs)
            combo <- rbind(combo, data.frame(scenario=scn, period=per, ADM0=adm, run_id=0:maxrid, impact=byrid))
        }

        save(combo, file="topdown/combo.RData")
    }
}

save(combo, file="topdown/combo.RData")
load("topdown/combo.RData")

source("topdown/driver.R")
make.map.post("topdown/maps", "combo", combo, loval=-.5, hival=.5)

## neshp <- importShapefile("regions/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
## neattr <- attr(neshp, 'PolyData')
## highinc <- neattr$ADM0_A3[neattr$ECONOMY %in% c('1. Developed region: G7', '2. Developed region: nonG7')]
## combo$impact[combo$ADM0 %in% highinc] <- NA
make.map.post("topdown/maps", "combo-now", combo)

## As fractional losses
combo2 <- combo %>% group_by(scenario, period, ADM0) %>% summarize(mu=1 - exp(mean(impact, na.rm=T)),
                                                                   ci05=1 - exp(quantile(impact, .95, na.rm=T)),
                                                                   ci95=1 - exp(quantile(impact, .05, na.rm=T)))

source("topdown/togdp.R")

finals <- data.frame()
for (scn in unique(combo2$scenario)) {
    for (per in unique(combo2$period)) {
        for (col in c('mu', 'ci05', 'ci95')) {
            subco <- subset(combo2, scenario == scn & period == per)

            subco$fracloss <- subco[, col, drop=T]

            gdploss <- calc.tradeloss(subco, -subco$fracloss[subco$ADM0 == 'GBR'])

            finals <- rbind(finals, data.frame(scenario=scn, period=per, measure=col, alone=100 * subco$fracloss[subco$ADM0 == 'GBR'], globe=gdploss))
        }
    }
}

subset(finals, measure == 'mu')

## Try to compute statistics at end
byrid <- data.frame()
for (scn in unique(combo$scenario)) {
    for (per in unique(combo$period)) {
        for (rid in unique(combo$run_id)) {
            print(c(scn, per, rid))
            subco <- subset(combo, scenario == scn & period == per & run_id == rid)
            subco$fracloss <- 1 - exp(subco$impact)
            gdploss <- calc.tradeloss(subco, -subco$fracloss[subco$ADM0 == 'GBR'])

            byrid <- rbind(byrid, data.frame(scenario=scn, period=per, run_id=rid, alone=100 * subco$fracloss[subco$ADM0 == 'GBR'], globe=gdploss))
        }
    }
}

source("lib/report.R")
print(xtable(make.table(byrid, 'globe'), digits=2), include.rownames=F)

save(byrid, file="topdown/combo-trade.RData")
## load("topdown/combo-trade.RData")

make.map("topdown/maps", "combo", results2, loval=-1, hival=1)
