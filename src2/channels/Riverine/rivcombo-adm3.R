### Combine by adm3

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)

source("lib/constants.R")

load("~/Open Modeling Group Dropbox/COACCH/channels/Riverine/results-final.RData")
byadm3.coacch <- byadm3.match
byadm3.coacch$damage.mgbp <- byadm3.coacch$damage*(-1)

load("~/Open Modeling Group Dropbox/COACCH/channels/Transport/results-final.RData")
stopifnot(all(paste(byadm3.match$scenario, byadm3.match$run_id, byadm3.match$period, byadm3.match$county) ==
              paste(byadm3.coacch$scenario, byadm3.coacch$run_id, byadm3.coacch$period, byadm3.coacch$county)))
byadm3.coacch$damage.mgbp <- byadm3.coacch$damage.mgbp + byadm3.match$damage*(-1)

## ## Produce combined COACCH flooding table here
## source("lib/report.R", chdir = T)

## library(dplyr)
## byadm3.coacch.uk <- byadm3.coacch %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage.mgbp))

## byadm3.coacch.uk$percent <- byadm3.coacch.uk$damage.mgbp / gdp.2015.gbp * 100 * 1e6
## tbl <- make.table(byadm3.coacch.uk, 'percent')
## print(xtable(tbl, digits=2), include.rownames=F)

load("~/Open Modeling Group Dropbox/UK Economic Risks/channels/flooding/flooding-final.RData")
byadm3.loss <- byadm3.match
byadm3.loss$run_id <- as.numeric(byadm3.loss$run_id)
byadm3.loss$damage.mgbp <- byadm3.loss$damage * 1.24570131 / 1e6  # x PESETA III CGE effect
byadm3.ccra <-byadm3.coacch[, c(1:3, 6)] %>% left_join(byadm3.loss)

load("~/Open Modeling Group Dropbox/UK Economic Risks/synthesis/pesetaiv-final-River Floods.RData")
byadm3.peseta <- byadm3.matched
byadm3.peseta$run_id <- as.numeric(byadm3.peseta$run_id)
byadm3.peseta$damage.mgbp <- (byadm3.peseta$damage / 100) * gdp.2015.gbp / 1e6
sort.peseta<-byadm3.coacch[, c(1:3, 6)] %>% left_join(byadm3.peseta)

## Check that scenario, run_id, ... order is the same
all(paste(byadm3.ccra$scenario, byadm3.ccra$run_id, byadm3.ccra$period, byadm3.ccra$county) ==
      paste(sort.peseta$scenario, sort.peseta$run_id, sort.peseta$period, sort.peseta$county))

all(paste(byadm3.ccra$scenario, byadm3.ccra$run_id, byadm3.ccra$period, byadm3.ccra$county) ==
    paste(byadm3.coacch$scenario, byadm3.coacch$run_id, byadm3.coacch$period, byadm3.coacch$county))

source("lib/combine.R")

## First time: set combo to blank df
combo <- data.frame()
## All future times: Load it

load("~/Open Modeling Group Dropbox/COACCH/channels/Riverine/rivercombo-adm3.RData")
known <- unique(paste(combo$scenario, combo$period, combo$county))

for (scn in unique(byadm3.ccra$scenario)) {
  for (per in unique(byadm3.ccra$period)) {
    for (cty in unique(byadm3.ccra$county)) {
      if (paste(scn, per, cty) %in% known)
        next

      print(c(scn, per, cty))
        mcs <- cbind(subset(byadm3.ccra, scenario == scn & period == per & county == cty)$damage.mgbp,
                     subset(sort.peseta, scenario == scn & period == per & county == cty)$damage.mgbp,
                     subset(byadm3.coacch, scenario == scn & period == per & county == cty)$damage.mgbp)
        stopifnot(sum(is.na(mcs)) == 0)

      byrid <- hier.combine(mcs)
      combo <- rbind(combo, data.frame(scenario=scn, period=per, county=cty, run_id=0:(length(byrid)-1), damage.mgbp=byrid))

      if (cty == unique(byadm3.ccra$county)[200])
        save(combo, file="~/Open Modeling Group Dropbox/COACCH/channels/Riverine/rivercombo-adm3.RData")
    }

    save(combo, file="~/Open Modeling Group Dropbox/COACCH/channels/Riverine/rivercombo-adm3.RData")
  }
}

load("~/Open Modeling Group Dropbox/COACCH/channels/Riverine/rivercombo-adm3.RData")
source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/report.R")
source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/constants.R")

library(dplyr)
combo.uk <- combo %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage.mgbp, na.rm=T))

combo.uk$percent <- combo.uk$damage.mgbp * 1e6 / gdp.2015.gbp * 100
tbl <- make.table(combo.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)
