setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/damagefunc.R")

library(reshape2)

df <- subset(read.csv("channels/labor/P3 Welfare and dd effects_for ERL compare article.csv"), analysis == "dd" & region == "UK & Irland")

df2 <- melt(df, c('analysis', 'scenario', 'region'))
## Compared to present day (1981 - 2010)
df2$temp <- 1 + 0.5 / 3 # from pesetatemps.png (also relative to 1981-2010)
df2$temp[df2$scenario == 'High'] <- c(2.7 + 3.0) / 2 # From PESETA III – Task1: Climate change projections, bias-adjustment, and selection of model runs

df3 <- subset(df2, variable == "Labour.productivity")

mods.30 <- c(8, 12, 34, 36, 72)

cdiffs <- c(0, df3$temp)
impacts <- c(0, -df3$value)
ses <- c(0, -df3$value * sd(mods.30) / mean(mods.30))
la <- fit.damages(cdiffs, impacts, ses)

info <- get.plotinfo(la, cdiffs, impacts, ses)

ggplot(info$ddf, aes(T, mu)) +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_point(data=info$pdf) +
    scale_x_continuous("Difference in temperature from 1981-2010 (C)", expand=c(0, 0)) +
    theme_bw() + ylab("Directed welfare damages (% GDP)")
ggsave("channels/labor/damagefunc.pdf", width=6.5, height=4)

## Project

source("lib/project.R")

results <- grid.project(la, 1981, 2010)
save(results, file="channels/labor/projects.RData")

source("lib/aggregate.R")

agginfo0 <- get.standard.agginfo(0)

uk.results <- grid.project(la, 1981, 2010, aggregate.cdiff=function(cdiffs) {
    data.frame(x=mean(cdiffs$x), y=mean(cdiffs$y), variable=county.aggregate(cdiffs$variable, agginfo0))
})

save(results, uk.results, file="channels/labor/projects.RData")

pop <- get.gridded.pop()
results$x.2 <- round(results$x, 2)
results$y.2 <- round(results$y, 2)

results.new <- force.match(results, uk.results, pop, col='w001001')
totweight <- sum(pop$w001001[pop$is.uk], na.rm=T)
results.new$damage <- results.new$total / totweight

save(uk.results, results.new, file="channels/labor/projects.RData")

uk.results %>% group_by(scenario, period) %>% summarize(damage=mean(damage))
results %>% group_by(scenario, period, run_id) %>% summarize(damage=sum(damage)) %>% group_by(scenario, period) %>% summarize(damage=mean(damage))
results.new %>% group_by(scenario, period, run_id) %>% summarize(damage=sum(damage, na.rm=T)) %>% group_by(scenario, period) %>% summarize(damage=mean(damage))

source("lib/report.R")

tbl <- make.table(uk.results, 'damage')
print(xtable(tbl, digits=3), include.rownames=F)

byadm3 <- aggregate.gridded.county(results.new, 'sum', col='damage')

save(byadm3, file="channels/labor/current.RData")

## load("channels/labor/projects.RData")
## load("channels/labor/current.RData")

byadm3$tot <- as.numeric(byadm3$tot)

library(dplyr)

want <- uk.results %>% group_by(scenario, period) %>% summarize(damage=mean(damage))
have <- byadm3 %>% group_by(scenario, period, run_id) %>% summarize(tot=sum(tot, na.rm=T)) %>% group_by(scenario, period) %>% summarize(tot=mean(tot))

ratio <- mean(want$damage / have$tot)

byadm3$damage <- ratio * byadm3$tot

byadm3 %>% group_by(scenario, period, run_id) %>% summarize(damage=sum(damage, na.rm=T)) %>% group_by(scenario, period) %>% summarize(damage=mean(damage))

save(byadm3, file="channels/labor/final.RData")

shp3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
polydata3 <- attr(shp3, 'PolyData')

gdp.2015.gbp <- 2089276e6 # GBP, from https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2
byadm3$damage.mgbp <- (byadm3$damage * gdp.2015.gbp / 100) / 1e6

make.maps.loop("channels/labor/maps", "p3", byadm3 %>% left_join(polydata3[, c('PID', 'NAME_3')], by=c('county'='NAME_3')), shp3, function(subres) {
    subres %>% group_by(PID) %>% summarize(mu=mean(damage.mgbp, na.rm=T), ci05=quantile(damage.mgbp, .05, na.rm=T), ci95=quantile(damage.mgbp, .95, na.rm=T))
}, 'Expected\nproductivity\nloss (£m)', '95% CI\nproductivity\nloss (£m)', 0, 10)


