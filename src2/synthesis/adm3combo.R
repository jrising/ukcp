setwd("~/Dropbox/UK Economic Risks")

library(dplyr)
source("lib/constants.R")

do.doublecount <- T
do.bigouts <- F

incs <- read.csv("socioeconomics/adm3inc.csv")
incs$GDP <- as.numeric(incs$GDPpc) * incs$pop

## SLR damages
load("../COACCH/channels/SLR/adm3combo.RData")
slr <- combo %>% left_join(incs, by='county')
slr$fracgdp <- slr$damage.mgbp * 1e6 / slr$GDP
slr.merge <- slr[, c('period', 'scenario', 'county', 'run_id', 'fracgdp')]

## Agriculture
load("../COACCH/channels/Agriculture/agcombo-adm3.RData")
agprod <- combo %>% left_join(incs, by='county')
agprod$fracgdp <- agprod$damage.mgbp * 1e6 / agprod$GDP
agprod.merge <- agprod[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## Algal blooms
load("channels/fisheries/algalbloom-projections-adm3.RData")
algalblooms.adm32 <- algalblooms.adm3 %>% left_join(incs, by=c('NAME_3'='county'))
algalblooms.adm32$fracgdp <- algalblooms.adm32$damage.adm * 1e6 / algalblooms.adm32$GDP
algalblooms.merge <- algalblooms.adm32[, c('period', 'scenario', 'run_id', 'NAME_3', 'fracgdp')]
names(algalblooms.merge)[4] <- 'county'

## Marine fisheries
load("../COACCH/channels/Fishery/results-final.RData")
fishprod <- byadm3.match %>% left_join(incs, by='county')
fishprod$fracgdp <- fishprod$damage * 1e6 / fishprod$GDP
fishprod.merge <- fishprod[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## Milk production
load("channels/agriculture/milk-final.RData") # byadm3.match
milkprod <- byadm3.match %>% left_join(incs, by='county')
milkprod$fracgdp <- milkprod$damage * 1e6 / milkprod$GDP
milkprod.merge <- milkprod[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## Lambs
load("channels/agriculture/lambs-final.RData")
lambs <- byadm3.match %>% left_join(incs, by='county')
lambs$fracgdp <- lambs$damage * 1e6 / milkprod$GDP
lambs.merge <- lambs[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## Biosphere
load("channels/biosphere/forest-projs-adm3.RData")
forests <- byadm3 %>% left_join(incs, by='county')
forests$fracgdp <- -forests$damage * 1000 * 0.7798 / forests$GDP
forests.merge <- forests[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]
forests.merge$run_id <- as.numeric(forests.merge$run_id)

load("channels/biosphere/bioloss.RData")
results$fracgdp <- results$percent / 100
bioloss.merge <- results[, c('period', 'scenario', 'run_id', 'fracgdp')]

## Forestry
load("../COACCH/channels/Forestry/results-final.RData")
forprod <- byadm3.match %>% left_join(incs, by='county')
forprod$fracgdp <- forprod$damage * 1e6 / forprod$GDP
forprod.merge <- forprod[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## Energy demand
load("../COACCH/channels/Energy-Demand/adm3combo.RData")
energy <- combo %>% left_join(incs, by='county')
energy$fracgdp <- energy$damage.mgbp * 1e6 / energy$GDP
energydemand.merge <- energy[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## Energy supply
load("../COACCH/channels/Energy-Supply/adm3combo.RData")
results.energy <- combo %>% left_join(incs, by='county')
results.energy$fracgdp <- (results.energy$damage.mgbp * 1e6 / gdp.2015.gbp)
energysupply.merge <- results.energy[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## Mortality
load("channels/health/combo-adm3.RData")
mortality <- combo %>% left_join(incs, by='county')
mortality$deaths <- mortality$pop * mortality$deathrate / 1e5
mortality$fracgdp <- mortality$deaths * vpf.2016 / mortality$GDP
mortality.merge <- mortality[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]
rm('mortality', 'combo')

## Labor productivity
load("../COACCH/channels/Labor/adm3combo.RData")
labor <- combo %>% left_join(incs, by='county')
labor$fracgdp <- labor$damage.mgbp * 1e6 / labor$GDP
labor.merge <- labor[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## Trade effects
load("topdown/combo-trade.RData")
trade <- byrid
trade$fracgdp <- trade$globe / 100

## River flooding
load("../COACCH/channels/Riverine/rivercombo-adm3.RData")
flooding <- combo %>% left_join(incs, by='county')
flooding$fracgdp <- (flooding$damage.mgbp * 1e6) / flooding$GDP
flooding.merge <- flooding[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## CCRA3 missing
missing.ccra <- read.csv("../COACCH/CCRA3/missrisk-news.csv")
missing.ccra.merge <- data.frame(scenario=rep(c('ssp126', 'ssp370'), each=4000*3),
                                 period=rep(rep(c('2011-2030', '2041-2060', '2081-2100'), each=4000), 2),
                                 run_id=rep(1:4000, 6),
                                 fracgdp=c(subset(missing.ccra, temp == '1 C')$affected,
                                           subset(missing.ccra, temp == '1.5 C')$affected,
                                           subset(missing.ccra, temp == '2 C')$affected,
                                           subset(missing.ccra, temp == '1 C')$affected,
                                           subset(missing.ccra, temp == '1.5 C')$affected,
                                           subset(missing.ccra, temp == '4 C')$affected))

## PESETA IV
load("synthesis/pesetaiv-final-Droughts.RData")
drought <- byadm3.matched %>% left_join(incs, by='county') # percent of total GDP
drought$fracgdp <- (drought$damage / 100) * gdp.2015.gbp / drought$GDP
if (do.doublecount) {
    ## Double-count in droughts is 47% in 1981-2010 (1995) and 38% in 2100
    for (year in c(2020, 2050, 2090)) {
        doublecount <- 47 + (38 - 47) * (year - 1995) / (2100 - 1995)
        period <- paste0(year - 9, '-', year + 10)
        drought$fracgdp[drought$period == period] <- (1 - doublecount / 100) * drought$fracgdp[drought$period == period]
    }
}
drought.merge <- drought[, c('period', 'scenario', 'run_id', 'county', 'fracgdp')]

## Combine all

alldamage <- drought.merge %>%
    left_join(fishprod.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.marine')) %>%
    left_join(slr.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.slr')) %>%
    left_join(mortality.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.mort')) %>%
    left_join(labor.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.labor')) %>%
    left_join(milkprod.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.milk')) %>%
    left_join(lambs.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.lamb')) %>%
    left_join(forests.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.bios')) %>%
    left_join(bioloss.merge, by=c('scenario', 'period', 'run_id'), suffix=c('', '.bios2')) %>%
    left_join(forprod.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.forest')) %>%
    left_join(energydemand.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.ener')) %>%
    left_join(energysupply.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.esup')) %>%
    left_join(agprod.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.crop')) %>%
    left_join(trade, by=c('scenario', 'period', 'run_id'), suffix=c('', '.trad')) %>%
    left_join(algalblooms.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.algal')) %>%
    left_join(flooding.merge, by=c('scenario', 'period', 'run_id', 'county'), suffix=c('', '.rivr')) %>%
    left_join(missing.ccra.merge, by=c('scenario', 'period', 'run_id'), suffix=c('.dght', '.ccra'))
alldamage$fracgdp.slr[is.na(alldamage$fracgdp.slr)] <- 0
alldamage$fracgdp.milk[is.na(alldamage$fracgdp.milk)] <- 0
alldamage$fracgdp.lamb[is.na(alldamage$fracgdp.lamb)] <- 0
alldamage$fracgdp.algal[is.na(alldamage$fracgdp.algal)] <- 0

## ## Apply CGE effects (NB: Already added in combos)
## cge.effects <- data.frame(col=c('fracgdp.slr', 'fracgdp.ener', 'fracgdp.crop', 'fracgdp.labor'),
##                           ratio=c(1.22205362, 0.95089301, 2.13668464, 2.05989019))
## for (ii in 1:nrow(cge.effects)) {
##     col <- cge.effects$col[ii]
##     alldamage[, paste0(col, '.cge')] <- alldamage[, col] * cge.effects$ratio[ii]
## }

## Add missing non-catastrophic damages
alldamage$total.known <- alldamage$fracgdp.slr + alldamage$fracgdp.algal +
    alldamage$fracgdp.marine + alldamage$fracgdp.mort +
    alldamage$fracgdp.labor + alldamage$fracgdp.crop + alldamage$fracgdp.dght +
    alldamage$fracgdp.ener + alldamage$fracgdp.esup +
    alldamage$fracgdp.rivr + alldamage$fracgdp.milk + alldamage$fracgdp.lamb + alldamage$fracgdp.ener +
    alldamage$fracgdp.trad + alldamage$fracgdp.bios + alldamage$fracgdp.bios2 + alldamage$fracgdp.forest

## Fit distribution to the Nordhaus approach
library(sn)
nordhaus <- read.csv("../COACCH/compare/missing-nordhaus.csv")
nordhaus.merge <- data.frame()
for (ii in 1:nrow(nordhaus)) {
    for (scenario in c('ssp126', 'ssp370')) {
        q05 <- nordhaus[ii, paste0(toupper(scenario), ".q05")]
        mu <- nordhaus[ii, paste0(toupper(scenario), ".mean")]
        q95 <- nordhaus[ii, paste0(toupper(scenario), ".q95")]
        result <- optim(c(mu, q95 - mu, 0), function(params) {
            delta <- params[3] / sqrt(1 + params[3]^2)
            mu.pred <- params[1] + abs(params[2]) * delta * sqrt(2 / pi)
            as.pred <- tryCatch({
                qsn(c(.05, .95), params[1], abs(params[2]), params[3])
            }, error=function(e) {
                rep(NA, 2)
            })
            sqrt(sum((c(mu.pred, as.pred) - c(mu, q05, q95))^2))
        })
        values <- rsn(2237, result$par[1], abs(result$par[2]), result$par[3])
        nordhaus.merge <- rbind(nordhaus.merge, data.frame(period=nordhaus$Years[ii], scenario, run_id=0:2236, fracgdp.nordhaus=values / 100))
    }
}

bymc <- alldamage %>% group_by(period, scenario, run_id) %>% summarize(avg=mean(total.known) * .25)
bymc2 <- bymc %>% left_join(nordhaus.merge)
bymc2$nordhaus.ratio <- ifelse(sign(bymc2$fracgdp.nordhaus) == sign(bymc2$avg) & abs(bymc2$avg) > .1 * abs(bymc2$fracgdp.nordhaus), bymc2$fracgdp.nordhaus / bymc2$avg, 1)

load("topdown/catastrophic.RData")
cata.merge <- results
names(cata.merge)[4] <- 'fracgdp.cata'

alldamage2 <- alldamage %>% left_join(cata.merge, by=c('scenario', 'period', 'run_id'), suffix=c('', '.cata')) %>%
    left_join(bymc2[, c('period', 'scenario', 'run_id', 'nordhaus.ratio')], by=c('scenario', 'period', 'run_id'), suffix=c('', '.nordhaus'))

alldamage2$missing.nordhaus <- 0.25 * alldamage2$total.known * alldamage2$nordhaus.ratio
alldamage2$missing <- ifelse(sample(c(T, F), nrow(alldamage2), T) & !is.na(alldamage2$fracgdp.ccra), alldamage2$fracgdp.ccra, alldamage2$missing.nordhaus)

alldamage2$total <- alldamage2$total.known + alldamage2$missing + alldamage2$fracgdp.cata

alldamage2$fracgdp.live <- alldamage2$fracgdp.algal + alldamage2$fracgdp.marine +
    alldamage2$fracgdp.milk + alldamage2$fracgdp.lamb
alldamage2$fracgdp.esad <- alldamage2$fracgdp.esup + alldamage2$fracgdp.ener
alldamage2$fracgdp.hydr <- alldamage2$fracgdp.dght + alldamage2$fracgdp.rivr
alldamage2$fracgdp.ecos <- alldamage2$fracgdp.bios + alldamage2$fracgdp.bios2 + alldamage2$fracgdp.forest

if (!do.doublecount) {
    alldamage.adm3 <- alldamage2
    save(alldamage.adm3, file="../COACCH/synthesis/alldamage-adm3.RData")
} else {
    alldamage.adm3.dblcnt <- alldamage2
    save(alldamage.adm3.dblcnt, file="../COACCH/synthesis/alldamage-adm3-dblcnt.RData")
}

source("lib/report.R")

shp.adm3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

alldamage3 <- alldamage2 %>% left_join(shp.adm3.polydata[, c('PID', 'NAME_3')], by=c('county'='NAME_3'))

if (do.bigouts) {
    quantile(alldamage3$total, c(.01, .05, .25, .75, .95, .99))

    make.maps.loop("../COACCH/synthesis/maps", "total", alldamage3, shp.adm3, function(subres) {
        subres %>% group_by(PID) %>% summarize(mu=mean(total, na.rm=T), ci5=quantile(total, .05, na.rm=T), ci95=quantile(total, .95, na.rm=T))
    }, "Expected\nGDP\ndamage (%)", "1-in-20\nGDP\ndamage (%)", -.01, 0.15, labels=percent)

    if (!do.doublecount) {
        collabs <- list('fracgdp.slr'='Coastal impacts', 'fracgdp.mort'='Health', 'fracgdp.labor'='Labour productivity',
                        'fracgdp.crop'='Agriculture', 'fracgdp.hydr'='Droughts and flooding', 'fracgdp.esad'='Energy supply & demand',
                        'fracgdp.live'='Livestock & fisheries', 'fracgdp.ecos'='Ecosystems', 'fracgdp.trad'='Trade effects',
                        'missing'='Missing non-catastrophic', 'fracgdp.cata'='Catastrophic risk')
    } else {
        collabs <- list('total'='Total costs')
    }

    for (col in names(collabs)) {
        alldamage3$mapval <- alldamage3[, col, drop=T]
        tosave <- alldamage3 %>% group_by(PID, county) %>%
            summarize(mu_ssp126_2020=mean(mapval[scenario == 'ssp126' & period == '2011-2030'], na.rm=T),
                      ci5_ssp126_2020=quantile(mapval[scenario == 'ssp126' & period == '2011-2030'], .05, na.rm=T),
                      ci95_ssp126_2020=quantile(mapval[scenario == 'ssp126' & period == '2011-2030'], .95, na.rm=T),
                      mu_ssp126_2050=mean(mapval[scenario == 'ssp126' & period == '2041-2060'], na.rm=T),
                      ci5_ssp126_2050=quantile(mapval[scenario == 'ssp126' & period == '2041-2060'], .05, na.rm=T),
                      ci95_ssp126_2050=quantile(mapval[scenario == 'ssp126' & period == '2041-2060'], .95, na.rm=T),
                      mu_ssp126_2090=mean(mapval[scenario == 'ssp126' & period == '2081-2100'], na.rm=T),
                      ci5_ssp126_2090=quantile(mapval[scenario == 'ssp126' & period == '2081-2100'], .05, na.rm=T),
                      ci95_ssp126_2090=quantile(mapval[scenario == 'ssp126' & period == '2081-2100'], .95, na.rm=T),
                      mu_ssp370_2020=mean(mapval[scenario == 'ssp370' & period == '2011-2030'], na.rm=T),
                      ci5_ssp370_2020=quantile(mapval[scenario == 'ssp370' & period == '2011-2030'], .05, na.rm=T),
                      ci95_ssp370_2020=quantile(mapval[scenario == 'ssp370' & period == '2011-2030'], .95, na.rm=T),
                      mu_ssp370_2050=mean(mapval[scenario == 'ssp370' & period == '2041-2060'], na.rm=T),
                      ci5_ssp370_2050=quantile(mapval[scenario == 'ssp370' & period == '2041-2060'], .05, na.rm=T),
                      ci95_ssp370_2050=quantile(mapval[scenario == 'ssp370' & period == '2041-2060'], .95, na.rm=T),
                      mu_ssp370_2090=mean(mapval[scenario == 'ssp370' & period == '2081-2100'], na.rm=T),
                      ci5_ssp370_2090=quantile(mapval[scenario == 'ssp370' & period == '2081-2100'], .05, na.rm=T),
                      ci95_ssp370_2090=quantile(mapval[scenario == 'ssp370' & period == '2081-2100'], .95, na.rm=T))
        write.csv(tosave, paste0("../COACCH/online/output-v2/", collabs[[col]], "-v2.csv"), row.names=F)

        ## for (per in unique(alldamage3$period)) {
        ##     for (scn in unique(alldamage3$scenario)) {
        ##         tosave <- subset(alldamage3, period == per & scenario == scn) %>% group_by(PID, county) %>%
        ##             summarize(mu=mean(mapval, na.rm=T), ci5=quantile(mapval, .05, na.rm=T), ci95=quantile(mapval, .95, na.rm=T))
        ##         write.csv(tosave, paste0("synthesis/output/", collabs[[col]], "-", scn, "-", per, ".csv"), row.names=F)
        ##     }
        ## }
    }

    ## Make maps for all sectors
    lobounds <- list('fracgdp.slr'=0, 'fracgdp.algal'=0, 'fracgdp.mort'=0,
                     'fracgdp.labor'=0, 'fracgdp.crop'=-.025, 'fracgdp.dght'=0, 'fracgdp.esup'=-.0000005,
                     'fracgdp.rivr'=0, 'fracgdp.milk'=-.0002, 'fracgdp.lamb'=0, 'fracgdp.ener'=-.0005,
                     'fracgdp.bios'=-.005, 'fracgdp.ecos'=-.004, 'fracgdp.live'=0,
                     'fracgdp.esad'=-.0002, 'fracgdp.hydr'=0)
    #'fracgdp.esad'=-.0005, 'fracgdp.hydr'=0)
    hibounds <- list('fracgdp.slr'=.1, 'fracgdp.algal'=.001, 'fracgdp.mort'=.01,
                     'fracgdp.labor'=.001, 'fracgdp.crop'=.05, 'fracgdp.dght'=.00004, 'fracgdp.esup'=.000001,
                     'fracgdp.rivr'=.00001, 'fracgdp.milk'=.0004, 'fracgdp.lamb'=.0005, 'fracgdp.ener'=0,
                     'fracgdp.bios'=0, 'fracgdp.ecos'=.002, 'fracgdp.live'=0.002,
                     'fracgdp.esad'=.0002, 'fracgdp.hydr'=.01)
    #'fracgdp.esad'=.000001, 'fracgdp.hydr'=.00005)

    for (col in c('fracgdp.esad')) {#, 'fracgdp.hydr')) {#c('fracgdp.live', 'fracgdp.ecos')) { #c('fracgdp.slr', 'fracgdp.algal', 'fracgdp.mort',
                                        #  'fracgdp.labor', 'fracgdp.crop', 'fracgdp.dght', 'fracgdp.esup',
                                        #  'fracgdp.rivr', 'fracgdp.milk', 'fracgdp.lamb', 'fracgdp.ener',
                                        #  'fracgdp.trad', 'fracgdp.bios', 'fracgdp.bios2')) {
        alldamage3$mapval <- alldamage3[, col, drop=T]
        make.maps.loop("synthesis/channel-maps", col, alldamage3, shp.adm3, function(subres) {
            subres %>% group_by(PID) %>% summarize(mu=mean(mapval, na.rm=T), ci5=quantile(mapval, .05, na.rm=T), ci95=quantile(mapval, .95, na.rm=T))
        }, "Expected\nGDP\ndamage (%)", "1-in-20\nGDP\ndamage (%)", lobounds[[col]], hibounds[[col]], labels=percent)
    }
}

## Combine into larger regions

shp.nuts1 <- importShapefile("regions/ref-nuts-2016-10m.shp/NUTS_RG_10M_2016_4326_LEVL_1.shp/NUTS_RG_10M_2016_4326_LEVL_1.shp")
shp.nuts1.polydata <- attr(shp.nuts1, 'PolyData')

centroids <- calcCentroid(shp.adm3, rollup=1)
centroids$EID <- 1:nrow(centroids)

found <- findPolys(as.EventData(centroids), shp.nuts1)
alldamage3$NUTS_NAME <- NA
for (ii in 1:nrow(found))
    alldamage3$NUTS_NAME[found$EID[ii] == alldamage3$PID] <- as.character(shp.nuts1.polydata$NUTS_NAME[found$PID[ii]])

alldamage4 <- alldamage3 %>% left_join(shp.adm3.polydata, by=c('county'='NAME_3'))
## alldamage4$bigreg <- as.character(alldamage4$NAME_1)
## alldamage4$bigreg[alldamage4$bigreg == 'England'] <- alldamage4$NUTS_NAME[alldamage4$bigreg == 'England']
## alldamage4$bigreg[alldamage4$bigreg %in% c("LONDON", "EAST OF ENGLAND", "SOUTH EAST (ENGLAND)", "SOUTH WEST (ENGLAND)")] <- "Southern England"
## alldamage4$bigreg[alldamage4$bigreg %in% c("EAST MIDLANDS (ENGLAND)", "WEST MIDLANDS (ENGLAND)")] <- "Midlands England"
## alldamage4$bigreg[alldamage4$bigreg %in% c("YORKSHIRE AND THE HUMBER", "NORTH WEST (ENGLAND)", "NORTH EAST (ENGLAND)")] <- "Northern England"

library(stringr)
alldamage4$nutsreg <- as.character(alldamage4$NAME_1)
alldamage4$nutsreg[alldamage4$nutsreg == 'England'] <- str_to_title(alldamage4$NUTS_NAME[alldamage4$nutsreg == 'England'])
alldamage4$nutsreg[alldamage4$county == "Portsmouth"] <- "South East (England)"

unique(alldamage4$nutsreg)

alldamage5 <- alldamage4 %>% left_join(incs, by='county') %>% group_by(scenario, run_id, period, nutsreg) %>%
    summarize(fracgdp.dght=sum(fracgdp.dght * GDP) / sum(GDP), fracgdp.slr=sum(fracgdp.slr * GDP) / sum(GDP),
              fracgdp.mort=sum(fracgdp.mort * GDP) / sum(GDP), fracgdp.labor=sum(fracgdp.labor * GDP) / sum(GDP),
              fracgdp.milk=sum(fracgdp.milk * GDP) / sum(GDP), fracgdp.lamb=sum(fracgdp.lamb * GDP) / sum(GDP),
              fracgdp.bios=sum(fracgdp.bios * GDP) / sum(GDP), fracgdp.ener=sum(fracgdp.ener * GDP) / sum(GDP),
              fracgdp.crop=sum(fracgdp.crop * GDP) / sum(GDP), fracgdp.trad=sum(fracgdp.trad * GDP) / sum(GDP),
              fracgdp.algal=sum(fracgdp.algal * GDP) / sum(GDP), fracgdp.rivr=sum(fracgdp.rivr * GDP) / sum(GDP),
              fracgdp.esup=sum(fracgdp.esup * GDP) / sum(GDP), missing=sum(missing * GDP) / sum(GDP),
              fracgdp.cata=sum(fracgdp.cata * GDP) / sum(GDP), fracgdp.bios2=sum(fracgdp.bios2 * GDP) / sum(GDP),
              fracgdp.marine=sum(fracgdp.algal * GDP) / sum(GDP), fracgdp.forest=sum(fracgdp.forest * GDP) / sum(GDP))

alldamage5$fracgdp.live <- alldamage5$fracgdp.algal + alldamage5$fracgdp.marine +
    alldamage5$fracgdp.milk + alldamage5$fracgdp.lamb
alldamage5$fracgdp.esad <- alldamage5$fracgdp.esup + alldamage5$fracgdp.ener
alldamage5$fracgdp.hydr <- alldamage5$fracgdp.dght + alldamage5$fracgdp.rivr
alldamage5$fracgdp.ecos <- alldamage5$fracgdp.bios + alldamage5$fracgdp.bios2 + alldamage5$fracgdp.forest
alldamage5$total <- alldamage5$fracgdp.dght + alldamage5$fracgdp.slr + alldamage5$fracgdp.mort + alldamage5$fracgdp.labor +
    alldamage5$fracgdp.milk + alldamage5$fracgdp.lamb + alldamage5$fracgdp.bios + alldamage5$fracgdp.ener +
    alldamage5$fracgdp.crop + alldamage5$fracgdp.trad + alldamage5$fracgdp.algal + alldamage5$fracgdp.marine + alldamage5$fracgdp.rivr + alldamage5$fracgdp.esup +
    alldamage5$missing + alldamage5$fracgdp.cata + alldamage5$fracgdp.bios2 + alldamage5$fracgdp.forest

if (!do.doublecount) {
    alldamage.reg <- alldamage5
    save(alldamage.reg, file="../COACCH/synthesis/alldamage-reg.RData")
} else {
    alldamage.reg.dblcnt <- alldamage5
    save(alldamage.reg.dblcnt, file="../COACCH/synthesis/alldamage-reg-dblcnt.RData")
}

alldamage5$scenario.label <- "High mitigation (SSP1-2.6)"
alldamage5$scenario.label[alldamage5$scenario == 'ssp370'] <- "Current policies (SSP3-7.0)"

alldamage5$nutsreg <- factor(alldamage5$nutsreg, levels=rev(c("Northern Ireland", "Scotland", "Wales", "North West (England)",
                                                              "North East (England)", "Yorkshire And The Humber",
                                                              "West Midlands (England)", "East Midlands (England)",
                                                              "South West (England)", "South East (England)",
                                                              "East Of England", "London")))

pdf1 <- subset(alldamage5, period == '2081-2100') %>% group_by(nutsreg, scenario.label, period) %>%
    summarize(channel=c('Coastal impacts', 'Health', 'Labour productivity', 'Agriculture',
                        'Droughts', 'Energy supply & demand', 'River floods', 'Livestock & fisheries',
                        'Ecosystems', 'Trade effects', 'Missing non-catastrophic', 'Catastrophic risk'),
              mu=c(mean(fracgdp.slr), mean(fracgdp.mort, na.rm=T),
                   mean(fracgdp.labor, na.rm=T), mean(fracgdp.crop, na.rm=T),
                   mean(fracgdp.dght, na.rm=T), mean(fracgdp.esup, na.rm=T) + mean(fracgdp.ener, na.rm=T),
                   mean(fracgdp.rivr, na.rm=T),
                   mean(fracgdp.milk, na.rm=T) + mean(fracgdp.lamb, na.rm=T) + mean(fracgdp.algal) + mean(fracgdp.marine), mean(fracgdp.bios + fracgdp.bios2 + fracgdp.forest),
                   mean(fracgdp.trad, na.rm=T), mean(missing, na.rm=T), mean(fracgdp.cata, na.rm=T)))
pdf2 <- subset(alldamage5, period == '2081-2100') %>% group_by(nutsreg, scenario.label, period) %>%
    summarize(ci5=quantile(fracgdp.slr + fracgdp.algal + fracgdp.marine + fracgdp.mort +
                           fracgdp.labor + fracgdp.crop + fracgdp.dght + fracgdp.esup + fracgdp.ener + fracgdp.rivr +
                           fracgdp.milk + fracgdp.lamb + fracgdp.bios + fracgdp.bios2 + fracgdp.forest + fracgdp.trad + missing + fracgdp.cata, .05, na.rm=T),
              ci95=quantile(fracgdp.slr + fracgdp.algal + fracgdp.marine + fracgdp.mort +
                            fracgdp.labor + fracgdp.crop + fracgdp.dght + fracgdp.esup + fracgdp.ener + fracgdp.rivr +
                            fracgdp.milk + fracgdp.lamb + fracgdp.bios + fracgdp.bios2 + fracgdp.forest + fracgdp.trad + missing + fracgdp.cata, .95, na.rm=T))

channel.order <- c('Droughts', 'River floods', 'Agriculture', 'Livestock & fisheries',
                   'Ecosystems', 'Energy supply & demand', 'Labour productivity', 'Health',
                   'Coastal impacts', 'Trade effects', 'Missing non-catastrophic', 'Catastrophic risk')
pdf1$channel <- factor(pdf1$channel, levels=rev(channel.order))

ggplot(pdf1, aes(x=nutsreg)) +
    coord_flip() +
    facet_wrap(~ scenario.label) +
    geom_hline(yintercept=0) +
    geom_col(aes(y=mu, fill=channel), alpha=.8, position='stack') +
    geom_errorbar(data=pdf2, aes(ymin=ci5, ymax=ci95), width=.25) +
    scale_y_continuous("Annual welfare-equivalent damages, 2081-2100 (% GDP)", labels=scales::percent) + xlab(NULL) +
    theme_bw() + scale_fill_manual("Channel:", breaks=rev(channel.order),
                                   values=rev(c('#b15928', '#a6cee3', '#33a02c', '#ffff99',
                                                '#b2df8a', '#e31a1c', '#fdbf6f', '#fb9a99',
                                                '#1f78b4', '#ff7f00', '#808080', '#cab2d6')))
ggsave("../COACCH/synthesis/barbyregion.pdf", width=8, height=3.5)

## Map with color by biggest sector

alldamage3$sectormax <- pmax(alldamage3$fracgdp.slr, alldamage3$fracgdp.mort,
                             alldamage3$fracgdp.labor, alldamage3$fracgdp.crop, alldamage3$fracgdp.dght,
                             alldamage3$fracgdp.esup + alldamage3$fracgdp.ener, alldamage3$fracgdp.rivr,
                             alldamage3$fracgdp.milk + alldamage3$fracgdp.lamb + alldamage3$fracgdp.algal + alldamage3$fracgdp.marine,
                             alldamage3$fracgdp.bios + alldamage3$fracgdp.forest)
alldamage3$bigsector <- ifelse(alldamage3$fracgdp.slr == alldamage3$sectormax, 'Coastal impacts',
                        ifelse(alldamage3$fracgdp.milk + alldamage3$fracgdp.lamb + alldamage3$fracgdp.algal + alldamage3$fracgdp.marine == alldamage3$sectormax, 'Livestock & fisheries',
                        ifelse(alldamage3$fracgdp.mort == alldamage3$sectormax, 'Health',
                        ifelse(alldamage3$fracgdp.labor == alldamage3$sectormax, 'Labour productivity',
                        ifelse(alldamage3$fracgdp.crop == alldamage3$sectormax, 'Agriculture',
                        ifelse(alldamage3$fracgdp.dght == alldamage3$sectormax, 'Droughts',
                        ifelse(alldamage3$fracgdp.esup + alldamage3$fracgdp.ener == alldamage3$sectormax, 'Energy supply & demand',
                        ifelse(alldamage3$fracgdp.rivr == alldamage3$sectormax, 'River floods',
                        ifelse(alldamage3$fracgdp.bios + alldamage3$fracgdp.forest == alldamage3$sectormax, 'Ecosystems', 'unknown')))))))))

library(pracma)
alldamage3$bigsector <- factor(alldamage3$bigsector, channel.order)
alldamage3$secnum <- as.numeric(alldamage3$bigsector)

pdf <- alldamage3 %>% group_by(scenario, period, PID) %>% summarize(secnum=Mode(secnum))
pdf$bigsector <- channel.order[pdf$secnum]

allshp <- data.frame()
for (period in c('2011-2030', '2041-2060', '2081-2100')) {
    shp.adm32 <- shp.adm3 %>% left_join(pdf[pdf$scenario == 'ssp370' & pdf$period == period, c('PID', 'bigsector')])
    allshp <- rbind(allshp, cbind(period=period, shp.adm32))

    gp <- ggplot(shp.adm32, aes(X, Y, group=paste(PID, SID))) +
        coord_map(xlim=c(-8, 2), ylim=c(50, 60.5)) + geom_polygon(aes(fill=bigsector)) +
        geom_polygon(data=shp.national.level, size=.1, fill="#00000000", colour="#808080") +
        theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                           axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
        xlab(NULL) + ylab(NULL) +
        scale_fill_manual("High risk channel:", breaks=rev(channel.order),
                          values=rev(c('#b15928', '#a6cee3', '#33a02c', '#ffff99',
                                       '#b2df8a', '#e31a1c', '#fdbf6f', '#fb9a99',
                                       '#1f78b4', '#ff7f00', '#808080', '#cab2d6')))
    ggsave(paste0("../COACCH/synthesis/maps/worst-", period, ".pdf"), gp, width=4.5, height=7)
}

gp <- ggplot(allshp, aes(X, Y, group=paste(PID, SID))) +
    facet_wrap(~ period, ncol=3) +
    coord_map(xlim=c(-8, 2), ylim=c(50, 60.5)) + geom_polygon(aes(fill=bigsector)) +
    geom_polygon(data=shp.national.level, size=.1, fill="#00000000", colour="#808080") +
    theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                       axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    xlab(NULL) + ylab(NULL) +
    scale_fill_manual("High risk channel:", breaks=rev(channel.order),
                      values=rev(c('#b15928', '#a6cee3', '#33a02c', '#ffff99',
                                   '#b2df8a', '#e31a1c', '#fdbf6f', '#fb9a99',
                                   '#1f78b4', '#ff7f00', '#808080', '#cab2d6')))
ggsave(paste0("../COACCH/synthesis/maps/worst-all.pdf"), gp, width=.8*(4*3 + 1), height=.8*7)

## Portion of positive and negative
load("../COACCH/synthesis/alldamage-adm3-dblcnt.RData")

library(dplyr)

alldamage.adm3.mean <- alldamage.adm3.dblcnt %>% group_by(period, scenario, county) %>% summarize(fracgdp.crop=mean(fracgdp.crop), total=mean(total))
alldamage.adm3.mean %>% group_by(period, scenario) %>% summarize(pos=mean(total > 0.05))
alldamage.adm3.mean %>% group_by(period, scenario) %>% summarize(pos=mean(total > 0.1))
alldamage.adm3.mean %>% group_by(period, scenario) %>% summarize(fracgdp.crop=max(fracgdp.crop))
quantile(subset(alldamage.adm3.dblcnt, period == '2081-2100' & scenario == 'ssp370')$fracgdp.crop)

load("../COACCH/synthesis/alldamage-reg-dblcnt.RData")
alldamage.reg.dblcnt %>% filter(nutsreg == "London") %>% group_by(scenario, period) %>%
    summarize(fracgdp.labor=mean(fracgdp.labor), fracgdp.esad=mean(fracgdp.esad))

library(reshape2)
alldamage.reg.dblcnt2 <- melt(alldamage.reg.dblcnt, names(alldamage.reg.dblcnt)[1:4])
alldamage.reg.dblcnt3 <- alldamage.reg.dblcnt2 %>% group_by(scenario, period, nutsreg, variable) %>%
    summarize(mu=mean(value), ci5=quantile(value, .05), ci95=quantile(value, .95))

disp.range <- function(match, mus, ci5s, ci95s) {
    paste0(round(mus[match] * 100, 2), "% [", floor(ci5s[match] * 100 * 10) / 10, " - ", ceiling(ci95s[match] * 100 * 10) / 10, "]")
}

library(xtable)
for (scen in unique(alldamage.reg.dblcnt3$scenario)) {
    for (per in unique(alldamage.reg.dblcnt3$period)) {
        tbl <- subset(alldamage.reg.dblcnt3, scenario == scen & period == per) %>%
            group_by(nutsreg) %>%
            summarize(Hydrological=disp.range(variable == 'fracgdp.hydr', mu, ci5, ci95),
                      Agriculture=disp.range(variable == 'fracgdp.crop', mu, ci5, ci95),
                      Animals=disp.range(variable == 'fracgdp.live', mu, ci5, ci95),
                      Ecosystems=disp.range(variable == 'fracgdp.ecos', mu, ci5, ci95),
                      Energy=disp.range(variable == 'fracgdp.esad', mu, ci5, ci95),
                      Productivity=disp.range(variable == 'fracgdp.labor', mu, ci5, ci95),
                      Mortality=disp.range(variable == 'fracgdp.mort', mu, ci5, ci95),
                      Coastal=disp.range(variable == 'fracgdp.slr', mu, ci5, ci95))
                      ## Missing=disp.range(variable == 'missing', mu, ci5, ci95),
                      ## Total=disp.range(variable == 'total', mu, ci5, ci95))
        names(tbl)[names(tbl) == 'nutsreg'] <- "Region"
        xtbl <- xtable(tbl)
        scenario.name <- ifelse(scen == 'ssp126', "SSP1-2.6", "SSP3-7.0")
        caption(xtbl) <- paste0("Channel-specific damages, as percent of local GDP, by region, under ", scenario.name, ", ", per, ".")
        print(xtbl, include.rownames=F, file=paste0("../COACCH/synthesis/tables/regtbl-", scen, "-", per, ".tex"))
    }
}

## What region is most impacted by each type of impact?
library(reshape2)
alldamage.reg.dblcnt2 <- melt(alldamage.reg.dblcnt, names(alldamage.reg.dblcnt)[1:4])
as.data.frame(alldamage.reg.dblcnt2 %>% filter(scenario == 'ssp370' & period == '2081-2100') %>%
              group_by(variable, nutsreg) %>% summarize(mu=mean(value)) %>%
              group_by(variable) %>% summarize(maxmu=max(mu), maxreg=nutsreg[which.max(mu)]))


## Calculate damages with risk and inequality aversion
## u = (c^(1 - eta) - 1) / (1 - eta)
## c = (1 + u (1 - eta))^(1 / (1 - eta))
eta <- 1.35
incs$pop <- as.numeric(incs$pop)
incs$GDPpc <- as.numeric(incs$GDPpc)
withwelfare <- alldamage.adm3.dblcnt %>% left_join(incs, by='county') %>%
    mutate(total=pmin(total, 0.99), # Limit the losses
           GDPpc.post=GDPpc * (1 - total),
           welfare.post=(GDPpc.post^(1 - eta) - 1) / (1 - eta))
withwelfare.xhet <- withwelfare %>% group_by(scenario, run_id, period) %>%
    summarize(welfare.post=sum(pop * welfare.post) / sum(pop),
              GDPpc.base=sum(pop * GDPpc) / sum(pop),
              GDPpc.welfareequiv.post=(1 + welfare.post * (1 - eta))^(1 / (1 - eta)),
              GDPpc.post=sum(pop * GDPpc.post) / sum(pop),
              welfare.mean.post=(GDPpc.post^(1 - eta) - 1) / (1 - eta)) %>%
    group_by(scenario, period) %>% summarize(GDPpc.welfareequiv.post=mean(GDPpc.welfareequiv.post, na.rm=T),
                                             GDPpc.post=mean(GDPpc.post, na.rm=T),
                                             GDPpc.base=mean(GDPpc.base, na.rm=T),
                                             frac=(GDPpc.base - GDPpc.welfareequiv.post) / GDPpc.base,
                                             frac.mean.post=(GDPpc.base - GDPpc.post) / GDPpc.base,
                                             GDPpc.welfare.mean.post=(1 + mean(welfare.mean.post) * (1 - eta))^(1 / (1 - eta)),
                                             frac.welfare.mean.post=(GDPpc.base - GDPpc.welfare.mean.post) / GDPpc.base)
withwelfare.xunc <- withwelfare %>% group_by(scenario, county, period) %>%
    summarize(welfare.post=mean(welfare.post),
              GDPpc.base=mean(GDPpc),
              GDPpc.welfare.post=(1 + welfare.post * (1 - eta))^(1 / (1 - eta)),
              GDPpc.post=mean(GDPpc.post), pop=mean(pop),
              welfare.GDPpc.post=(GDPpc.post^(1 - eta) - 1) / (1 - eta)) %>%
    group_by(scenario, period) %>% summarize(GDPpc.welfare.post=sum(pop * GDPpc.welfare.post, na.rm=T) / sum(pop * !is.na(GDPpc.welfare.post)),
                                             GDPpc.post=sum(pop * GDPpc.post) / sum(pop),
                                             GDPpc.base=sum(pop * GDPpc.base) / sum(pop),
                                             frac=(GDPpc.base - GDPpc.welfare.post) / GDPpc.base,
                                             frac.mean=(GDPpc.base - GDPpc.post) / GDPpc.base,
                                             welfare.GDPpc.post=sum(pop * welfare.GDPpc.post, na.rm=T) / sum(pop * !is.na(welfare.GDPpc.post)),
                                             GDPpc.welfare.GDPpc.post=(1 + welfare.GDPpc.post * (1 - eta))^(1 / (1 - eta)),
                                             frac.ukriskeq=(GDPpc.base - GDPpc.welfare.GDPpc.post) / GDPpc.base)
withwelfare.xbot <- withwelfare %>% group_by(scenario, period) %>%
    summarize(welfare.post=sum(pop * welfare.post, na.rm=T) / sum(pop * !is.na(welfare.post)),
              GDPpc.base=sum(pop * GDPpc) / sum(pop),
              GDPpc=(1 + welfare.post * (1 - eta))^(1 / (1 - eta)),
              GDPpc.mean=sum(pop * GDPpc.post) / sum(pop),
              frac=(GDPpc.base - GDPpc) / GDPpc.base,
              frac.mean=(GDPpc.base - GDPpc.mean) / GDPpc.base)

finaltable <- withwelfare.xhet %>% left_join(withwelfare.xunc, by=c('scenario', 'period'), suffix=c('', '.xunc')) %>%
    left_join(withwelfare.xbot, by=c('scenario', 'period'), suffix=c('.xhet', '.xbot'))
finaltable2 <- finaltable[, c('scenario', 'period', 'frac.mean.post', 'frac.welfare.mean.post', 'frac.xunc',
                              'frac.ukriskeq', 'frac.xhet', 'frac.xbot')]
finaltable2[, 3:ncol(finaltable2)] <- finaltable2[, 3:ncol(finaltable2)] * 100
names(finaltable2) <- c('Scenario', 'Period', 'No Welfare', 'UK-Total Risk', 'County Risk',
                        'Equity of Avg.', 'Avg. of Equity', 'Risk & Equity')

library(xtable)
print(xtable(finaltable2), include.rownames=F)

alldamage.adm3.dblcnt %>% group_by(scenario, period) %>% summarize(frac=mean(total > 1), ncnty=length(unique(county[total > 1])), fraccnty=ncnty / length(unique(county)))
