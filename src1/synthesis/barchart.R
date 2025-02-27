setwd("~/Open Modeling Group Dropbox/UK Economic Risks/synthesis")

library(dplyr)
source("../lib/constants.R")

do.doublecount <- T

## SLR damages
slr <- read.csv("../channels/slr/slr-projections.csv")
slr$period <- as.character(slr$period)
slr$period[slr$period == "2020"] <- "2011-2030"
slr$period[slr$period == "2050"] <- "2041-2060"
slr$period[slr$period == "2090"] <- "2081-2100"

for (scenario in unique(slr$scenario))
    for (period in unique(slr$period))
        slr$run_id[slr$scenario == scenario & slr$period == period] <- sample(1:sum(slr$scenario == scenario & slr$period == period)) - 1

## Growth rate
growrate <- mean(diff(log(c(1314.245, 1329.186, 1350.140, 1382.128, 1418.235, 1455.828, 1495.583))))
gdp.proj <- gdp.2015 * exp(growrate * (c(2020, 2050, 2090) - 2015))
slr2 <- slr %>% left_join(data.frame(gdp.proj, period=c("2011-2030", "2041-2060", "2081-2100")))
slr2$fracgdp <- slr2$value * 1e9 / slr2$gdp.proj
slr2.merge <- slr2[, c('period', 'scenario', 'run_id', 'fracgdp')]

## Agriculture
load("../channels/agriculture/agcombo-adm3.RData")
agprod <- combo %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage.mgbp, na.rm=T))
agprod$fracgdp <- (agprod$damage.mgbp * 1e6 / gdp.2015.gbp)
agprod.merge <- agprod[, c('period', 'scenario', 'run_id', 'fracgdp')]

## Algal blooms
algalblooms <- read.csv("../channels/fisheries/algalbloom-projections.csv")
algalblooms2 <- algalblooms %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))
algalblooms2$fracgdp <- algalblooms2$damage * 1e6 / gdp.2015.gbp
algalblooms2.merge <- algalblooms2[, c('period', 'scenario', 'run_id', 'fracgdp')]

## Milk production
load("../channels/agriculture/milk-projs.RData") # results.byregion
milkprod2 <- results.byregion %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))
milkprod2$fracgdp <- milkprod2$damage * 1e6 / gdp.2015.gbp
milkprod2.merge <- milkprod2[, c('period', 'scenario', 'run_id', 'fracgdp')]

## Lambs
load("../channels/agriculture/lambs-proj.RData")
results.lambs <- results.byregion %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))
results.lambs$fracgdp <- results.lambs$damage * 1e6 / gdp.2015.gbp
results.lambs.merge <- results.lambs[, c('period', 'scenario', 'run_id', 'fracgdp')]

## Biosphere
load("../channels/biosphere/forest-projs.RData")
projs.bio <- projs %>% group_by(scenario, run_id, period) %>% summarize(fracgdp=-sum(damage * 1000 * 0.7798 / gdp.2020.gbp, na.rm=T))

load("../channels/biosphere/bioloss.RData")
results$fracgdp <- results$percent / 100
projs.bio2 <- results[, c('period', 'scenario', 'run_id', 'fracgdp')]

## Energy demand
load("../channels/energy/rodeetal2-project.RData")
results.energy <- results2 %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage, na.rm=T))
results.energy$fracgdp <- 1e9 * (1 + 1.401 / 100) * 0.7798 * results.energy$damage / gdp.2020.gbp

## Mortality
load("../channels/health/combo-adm3.RData")
adm3info <- read.csv("../regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.csv")
combo2 <- combo %>% left_join(adm3info, by=c('county'='NAME_3'))
combo2$deaths <- combo2$popsum * combo2$deathrate / 1e5
health.uk <- combo2 %>% group_by(scenario, run_id, period) %>% summarize(deaths=sum(deaths, na.rm=T))
vpf.2016 <- 1.83e6 # https://www.bristol.ac.uk/media-library/sites/policybristol/PolicyBristol-Report-April-2018-value-human-life.pdf
health.uk$fracgdp <- health.uk$deaths * vpf.2016 / gdp.2015.gbp

## Labor productivity
load("../channels/labor/projects.RData") # uk.results, results.new
labor2 <- uk.results
labor2$fracgdp <- labor2$damage / 100

## Trade effects
load("../topdown/combo-trade.RData")
trade <- byrid
trade$fracgdp <- trade$globe / 100

## River flooding
load("../channels/flooding/combo-adm3.RData")
flooding <- combo %>% group_by(scenario, period, run_id) %>% summarize(damage.mgbp=sum(damage.mgbp))
flooding$fracgdp <- (flooding$damage.mgbp * 1e6) / gdp.2015.gbp
flooding2 <- flooding[, c('period', 'scenario', 'run_id', 'fracgdp')]

## PESETA IV
load("pesetaiv-project-new.RData")
pesetaiv <- uk.results
## Drop PESETA IV coastal floods: get implausible negative values, believe CIAM more
pesetaiv <- subset(pesetaiv, channel != "Coastal floods")

pesetaiv2 <- pesetaiv %>% group_by(scenario, run_id, period) %>% summarize(crops=damage[channel == "Crop Productivity"] / 100, # Already in agprod
                                                                           droughts=damage[channel == "Droughts"] / 100, # reduce by double-count for ag
                                                                           elecprod=damage[channel == "Electricity Production"] / 100,
                                                                           rivers=damage[channel == "River floods"] / 100)
if (do.doublecount) {
    ## Double-count in droughts is 47% in 1981-2010 (1995) and 38% in 2100
    for (year in c(2020, 2050, 2090)) {
        doublecount <- 47 + (38 - 47) * (year - 1995) / (2100 - 1995)
        period <- paste0(year - 9, '-', year + 10)
        pesetaiv2$droughts[pesetaiv2$period == period] <- (1 - doublecount / 100) * pesetaiv2$droughts[pesetaiv2$period == period]
    }
}

alldamage <- algalblooms2.merge %>% left_join(slr2.merge, by=c('scenario', 'period', 'run_id'), suffix=c('', '.slr')) %>%
    left_join(health.uk, by=c('scenario', 'period', 'run_id'), suffix=c('', '.mort')) %>%
    left_join(labor2, by=c('scenario', 'period', 'run_id'), suffix=c('', '.labor')) %>%
    left_join(milkprod2, by=c('scenario', 'period', 'run_id'), suffix=c('', '.milk')) %>%
    left_join(results.lambs.merge, by=c('scenario', 'period', 'run_id'), suffix=c('', '.lamb')) %>%
    left_join(projs.bio, by=c('scenario', 'period', 'run_id'), suffix=c('', '.bios')) %>%
    left_join(projs.bio2, by=c('scenario', 'period', 'run_id'), suffix=c('', '.bios2')) %>%
    left_join(results.energy, by=c('scenario', 'period', 'run_id'), suffix=c('', '.ener')) %>%
    left_join(agprod.merge, by=c('scenario', 'period', 'run_id'), suffix=c('', '.crop')) %>%
    left_join(trade, by=c('scenario', 'period', 'run_id'), suffix=c('', '.trad')) %>%
    left_join(flooding, by=c('scenario', 'period', 'run_id'), suffix=c('.algal', '.flood')) %>%
    left_join(pesetaiv2)

## Apply CGE effects
cge.effects <- data.frame(col=c('fracgdp.slr', 'fracgdp.ener'), # 'fracgdp.crop', 'fracgdp.labor',
                          ratio=c(1.22205362, 0.95089301)) # 2.13668464, 2.05989019,

for (ii in 1:nrow(cge.effects)) {
    col <- cge.effects$col[ii]
    alldamage[, paste0(col, '.cge')] <- alldamage[, col] * cge.effects$ratio[ii]
}

## Add missing non-catastrophic damages
alldamage$total.known <- alldamage$fracgdp.slr.cge + alldamage$fracgdp.algal + alldamage$fracgdp.mort +
    alldamage$fracgdp.labor + alldamage$fracgdp.crop + alldamage$droughts + alldamage$elecprod +
    alldamage$fracgdp.flood + alldamage$fracgdp.milk +
    alldamage$fracgdp.lamb + alldamage$fracgdp.ener.cge +
    alldamage$fracgdp.trad + alldamage$fracgdp.bios + alldamage$fracgdp.bios2

alldamage$missing <- alldamage$total.known * .25

load("../topdown/catastrophic.RData")
alldamage2 <- alldamage %>% left_join(results, by=c('scenario', 'period', 'run_id'), suffix=c('', '.cata'))

alldamage2$total <- alldamage2$fracgdp.slr.cge + alldamage2$fracgdp.algal + alldamage2$fracgdp.mort +
    alldamage2$fracgdp.labor + alldamage2$fracgdp.crop + alldamage2$droughts + alldamage2$elecprod +
    alldamage2$fracgdp.flood + alldamage2$fracgdp.milk +
    alldamage2$fracgdp.lamb + alldamage2$fracgdp.bios +
    alldamage2$fracgdp.bios2 + alldamage2$fracgdp.trad + alldamage2$missing + alldamage2$damage.cata +
    alldamage2$fracgdp.ener.cge

alldamage2$fracgdp.live <- alldamage2$fracgdp.algal + alldamage2$fracgdp.milk + alldamage2$fracgdp.lamb
alldamage2$fracgdp.esad <- alldamage2$elecprod + alldamage2$fracgdp.ener.cge
alldamage2$fracgdp.hydr <- alldamage2$droughts + alldamage2$fracgdp.flood
alldamage2$fracgdp.ecos <- alldamage2$fracgdp.bios + alldamage2$fracgdp.bios2

if (!do.doublecount) {
    alldamage.uk <- alldamage2
    save(alldamage.uk, file="alldamage-uk.RData")
} else {
    alldamage.uk.dblcnt <- alldamage2
    save(alldamage.uk.dblcnt, file="alldamage-uk-dblcnt.RData")
}

alldamage2$scenario.label <- "High mitigation (SSP1-2.6)"
alldamage2$scenario.label[alldamage2$scenario == 'ssp370'] <- "Current policies (SSP3-7.0)"

pdf1 <- alldamage2 %>% group_by(scenario.label, period) %>%
    summarize(channel=c('Coastal impacts', 'Health', 'Labour productivity', 'Agriculture',
                        'Droughts', 'Energy supply & demand', 'River floods', 'Livestock & fisheries',
                        'Ecosystems', 'Trade effects', 'Missing non-catastrophic', 'Catastrophic risk'),
              mu=c(mean(fracgdp.slr.cge), mean(fracgdp.mort, na.rm=T),
                   mean(fracgdp.labor, na.rm=T), mean(fracgdp.crop, na.rm=T),
                   mean(droughts, na.rm=T), mean(elecprod, na.rm=T) + mean(fracgdp.ener.cge, na.rm=T), mean(fracgdp.flood, na.rm=T),
                   mean(fracgdp.milk, na.rm=T) + mean(fracgdp.lamb, na.rm=T) + mean(fracgdp.algal), mean(fracgdp.bios + fracgdp.bios2),
                   mean(fracgdp.trad, na.rm=T), mean(missing, na.rm=T), mean(damage.cata, na.rm=T)))
pdf2 <- alldamage2 %>% group_by(scenario.label, period) %>%
    summarize(ci5=quantile(fracgdp.slr.cge + fracgdp.algal + fracgdp.mort +
                           fracgdp.labor + fracgdp.crop + droughts + elecprod + fracgdp.ener.cge + fracgdp.flood +
                           fracgdp.milk + fracgdp.lamb + fracgdp.bios + fracgdp.bios2 + fracgdp.trad + missing + damage.cata, .05, na.rm=T),
              ci95=quantile(fracgdp.slr.cge + fracgdp.algal + fracgdp.mort +
                            fracgdp.labor + fracgdp.crop + droughts + elecprod + fracgdp.ener.cge + fracgdp.flood +
                            fracgdp.milk + fracgdp.lamb + fracgdp.bios + fracgdp.bios2 + fracgdp.trad + missing + damage.cata, .95, na.rm=T))

## Construct table
source("../lib/report.R")
alldamage2$percent <- alldamage2$fracgdp.hydr * 100
tbl <- make.table(alldamage2, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

alldamage2$percent <- alldamage2$fracgdp.ecos * 100
tbl <- make.table(alldamage2, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

alldamage2$percent <- alldamage2$fracgdp.esad * 100
tbl <- make.table(alldamage2, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

alldamage2$percent <- alldamage2$fracgdp.slr.cge * 100
tbl <- make.table(alldamage2, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

alldamage2$percent <- alldamage2$missing * 100
tbl <- make.table(alldamage2, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

alldamage2$percent <- alldamage2$total.known * 100
tbl <- make.table(alldamage2, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

alldamage2$percent <- alldamage2$damage.cata * 100
tbl <- make.table(alldamage2, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

alldamage2$percent <- (alldamage2$total.known + alldamage2$missing + alldamage2$damage.cata) * 100
tbl <- make.table(alldamage2, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

## Calculate under risk aversion

alldamage2$uu.lo <- (1 - alldamage2$total)^(1 - 2) / (1 - 2)
alldamage2$uu.hi <- (1 - alldamage2$total)^(1 - 10) / (1 - 10)
alldamage2$uu.mu <- (1 - alldamage2$total)^(1 - 1.35) / (1 - 1.35)

pdf3 <- alldamage2 %>% group_by(scenario.label, period) %>%
    summarize(cc.lo=(mean(uu.lo, na.rm=T)*(1 - 2))^(1 / (1 - 2)), fracgdp.lo=1 - cc.lo,
              cc.hi=(mean(uu.hi, na.rm=T)*(1 - 10))^(1 / (1 - 10)), fracgdp.hi=1 - cc.hi,
              cc.mu=(mean(uu.mu, na.rm=T)*(1 - 1.35))^(1 / (1 - 1.35)), fracgdp.mu=1 - cc.mu)

library(ggplot2)
channel.order <- c('Droughts', 'River floods', 'Agriculture', 'Livestock & fisheries',
                   'Ecosystems', 'Energy supply & demand', 'Labour productivity', 'Health',
                   'Coastal impacts', 'Trade effects', 'Missing non-catastrophic', 'Catastrophic risk')
pdf1$channel <- factor(pdf1$channel, levels=rev(channel.order))
ggplot(pdf1, aes(x=period)) +
    facet_wrap(~ scenario.label) +
    geom_hline(yintercept=0) +
    geom_col(aes(y=mu, fill=channel), alpha=.8, position='stack') +
    geom_errorbar(data=pdf2, aes(ymin=ci5, ymax=ci95), width=.25) +
    geom_point(data=pdf3, aes(y=(fracgdp.lo + fracgdp.hi) / 2)) +
    scale_y_continuous("Annual welfare-equivalent damages (% GDP)", labels=scales::percent) + xlab(NULL) +
    theme_bw() + scale_fill_manual("Channel:", breaks=rev(channel.order),
                                   values=rev(c('#b15928', '#a6cee3', '#33a02c', '#ffff99',
                                                '#b2df8a', '#e31a1c', '#fdbf6f', '#fb9a99',
                                                '#1f78b4', '#ff7f00', '#808080', '#cab2d6')))
ggsave("barcharts.pdf", width=8, height=4)

pdf3$fracgdp.mid <- (pdf3$fracgdp.lo + pdf3$fracgdp.hi) / 2
pdf3

1 - (mean((1 - c(0, .1))^(1 - 2) / (1 - 2))*(1 - 2))^(1 / (1 - 2))
1 - (mean((1 - c(-.1, .2))^(1 - 2) / (1 - 2))*(1 - 2))^(1 / (1 - 2))
