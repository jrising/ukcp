### Combine by adm3

setwd("~/Dropbox/UK Economic Risks")

library(dplyr)

source("lib/constants.R")

load("~/Dropbox/COACCH/channels/SLR/results-final.RData")
byadm3.coacch.noa <- byadm3.match

load("~/Dropbox/COACCH/channels/SLR-AdA/results-final.RData")
byadm3.coacch.ada <- byadm3.match

stopifnot(all(paste(byadm3.coacch.noa$scenario, byadm3.coacch.noa$run_id, byadm3.coacch.noa$period, byadm3.coacch.noa$county) ==
              paste(byadm3.coacch.ada$scenario, byadm3.coacch.ada$run_id, byadm3.coacch.ada$period, byadm3.coacch.ada$county)))

byadm3.coacch <- byadm3.coacch.noa
byadm3.coacch$damage <- ifelse(sample(c(T, F), nrow(byadm3.coacch), replace=T), byadm3.coacch.noa$damage, byadm3.coacch.ada$damage)
byadm3.coacch$damage.mgbp <- byadm3.coacch$damage*(-1)

## Convert CIAM to match MGBP when multiplied by 2015 GBP
## value is in billions within the future economy
load("~/Dropbox/UK Economic Risks/channels/slr/slr-projections-adm3.RData")
byadm3.loss <- byadm3.mcs
byadm3.loss$run_id <- as.numeric(byadm3.loss$mcid)

## Growth rate
growrate <- mean(diff(log(c(1314.245, 1329.186, 1350.140, 1382.128, 1418.235, 1455.828, 1495.583))))
gdp.proj <- gdp.2015 * exp(growrate * (c(2020, 2050, 2090) - 2015))
byadm3.loss2 <- byadm3.loss %>% left_join(data.frame(gdp.proj, period=c("2011-2030", "2041-2060", "2081-2100")))

## Also add in CGE effects
byadm3.loss2$damage.mgbp <- 1.22205362 * (byadm3.loss2$value * 1e9 * convert.usd2gbp.2015 / byadm3.loss2$gdp.proj) * gdp.2015.gbp / 1e6

df <- byadm3.coacch %>% left_join(byadm3.loss2, by=c('scenario', 'run_id', 'period', 'county'='ADM3'),
                                  suffix=c('.coacch', '.ciam'))

source("lib/combine.R")

## First time: set combo to blank df
combo <- data.frame()
## All future times: Load it

if (file.exists("../COACCH/channels/SLR/adm3combo.RData"))
  load("../COACCH/channels/SLR/adm3combo.RData")
known <- unique(paste(combo$scenario, combo$period, combo$county))

maxrid = max(df$run_id)

for (scn in unique(df$scenario)) {
  for (per in unique(df$period)) {
    for (cty in unique(df$county)){
      if (paste(scn, per, cty) %in% known)
        next

      print(c(scn, per, cty))
      mcs <- cbind(subset(df, scenario == scn & period == per & county == cty)$damage.mgbp.coacch,
                   subset(df, scenario == scn & period == per & county == cty)$damage.mgbp.ciam)
      if (all(is.na(mcs[, 2])))
          combo <- rbind(combo, data.frame(scenario=scn, period=per, county=cty, run_id=0:maxrid, damage.mgbp=mcs[, 1]))
      else {
          mcs <- mcs[!is.na(mcs[, 1]) & !is.na(mcs[, 2]),]
          stopifnot(sum(is.na(mcs)) == 0)

          byrid <- hier.combine(mcs)
          combo <- rbind(combo, data.frame(scenario=scn, period=per, county=cty, run_id=0:(nrow(mcs)-1), damage.mgbp=byrid))
      }

      if (cty == unique(df$county)[200])
          save(combo, file="~/Dropbox/COACCH/channels/SLR/adm3combo.RData")
    }

    save(combo, file="~/Dropbox/COACCH/channels/SLR/adm3combo.RData")
  }
}

load("~/Dropbox/COACCH/channels/SLR/adm3combo.RData")

source("~/Dropbox/UK Economic Risks/lib/report.R")
source("~/Dropbox/UK Economic Risks/lib/constants.R")

library(dplyr)
combo.uk <- combo %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage.mgbp, na.rm=T))

combo.uk$percent <- combo.uk$damage.mgbp * 1e6 / gdp.2015.gbp * 100
tbl <- make.table(combo.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)
