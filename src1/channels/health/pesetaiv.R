setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

## Prepare data

pop.periods <- c(68, 68, 68, 68, 82, 82, 91, 91, 91) * 1e6
deaths.hot <- c(95, 942, 1470, 2259, 1112, 1735, 1237, 1929, 2955)
deaths.cold <- c(5, 2, 1, 1, 3, 1, 3, 1, 1)

rate.hot <- 1e5 * deaths.hot / pop.periods
rate.cold <- 1e5 * deaths.cold / pop.periods

## baseline is 1980-2016
global.cdiffs <- c(0.7, 1.5, 2., 3., 1.5, 2., 1.5, 2., 3.)
cdiffs <- c(0.7, (0.75 + 1)/2, (1 + 1.25)/2, (1.75 + 2)/2, (0.75 + 1)/2, (1 + 1.25)/2, (0.75 + 1)/2, (1 + 1.25)/2, (1.75 + 2)/2)

wheat.ci90 <- c(0.69 - -1.25, 0.68 - -1.76, 0.63 - -2.87) #c(1.24 - -.80, 1.21 - -0.82, 1.30 - -1.04)
wheat.med <- c(-0.4, -0.7, -1.3) #c(0, 0, -.1)
wheat.sd <- (wheat.ci90 / 2) / 1.64 # qnorm(.95, 0, wheat.sd) - qnorm(.05, 0, wheat.sd) ~= wheat.ci90
sd.scale <- abs(mean(wheat.sd) / mean(wheat.med))

## Estimate damage functions

source("lib/damagefunc.R")

cols <- c(1, 5:9)

la.total <- fit.damages(cdiffs[cols], rate.hot[cols] + rate.cold[cols], (rate.hot[cols] + rate.cold[cols]) * sd.scale)

ggplot.damagefunc(la.total, cdiffs[cols], rate.hot[cols] + rate.cold[cols], (rate.hot[cols] + rate.cold[cols]) * sd.scale, zeroneg=2, zeroneg.range=.5) +
    coord_cartesian(ylim=c(-5, 50)) +
    scale_x_continuous("Difference from pre-industrial (C)", expand=c(0, 0)) +
    ylab("Change in mortality rate (deaths / 100,000)")

save(la.total, file="channels/health/pesetaiv-dfs.RData")

## load("channels/health/pesetaiv-dfs.RData")

## Project damages at grid level

source("lib/project.R")

results <- grid.project(la.total, 1970, 2001, zeroneg=2, zeroneg.range=.5)

save(results, file="channels/health/pesetaiv-projs.RData")

## Project damages at UK level

source("lib/aggregate.R")

agginfo0 <- get.standard.agginfo(0)

uk.results <- grid.project(la.total, 1970, 2001, aggregate.cdiff=function(cdiffs) {
    data.frame(x=mean(cdiffs$x), y=mean(cdiffs$y), variable=county.aggregate(cdiffs$variable, agginfo0))
}, zeroneg=2, zeroneg.range=.5)

source("lib/report.R")

uk.results$deathrate <- uk.results$damage
uk.results$deathrate[uk.results$period == '2081-2100'] <- pmax(0, uk.results$damage[uk.results$period == '2081-2100'])

tbl <- make.table(uk.results, 'deathrate')

library(xtable)
print(xtable(tbl, digits=1), include.rownames=F)

save(results, uk.results, file="channels/health/pesetaiv-projs.RData")

load("channels/health/pesetaiv-projs.RData")

## TODO: Continue from here

gridpop <- get.gridded.pop()
gridpop$weight <- gridpop$w001001 / 100000

results$x.2 <- round(results$x, 2)
results$y.2 <- round(results$y, 2)

results2 <- force.match(results, uk.results, gridpop)

final <- results2 %>% group_by(scenario, period, x, y) %>% summarize(mu=mean(total, na.rm=T), ci5=quantile(total, .05, na.rm=T), ci95=quantile(total, .95, na.rm=T))

write.csv(final, "channels/health/pesetaiv-final.csv", row.names=F)

source("lib/report.R")

results$deathrate <- 100000 * mortrate * subres$damage / 100
tbl <- make.table(results, 'deathrate')
