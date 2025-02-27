setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)

source("lib/constants.R")

load("~/Open Modeling Group Dropbox/COACCH/channels/Energy-Supply/results-final.RData")
byadm3.coacch <- byadm3.match
byadm3.coacch$damage.mgbp <- byadm3.coacch$damage*(-1)

load("synthesis/pesetaiv-final-Electricity Production.RData")
byadm3.peseta <- byadm3.matched
byadm3.peseta$damage.mgbp <- (byadm3.peseta$damage / 100) * gdp.2015.gbp / 1e6

df <- byadm3.coacch %>% left_join(byadm3.peseta, by=c('scenario', 'run_id', 'period', 'county'), suffix=c('.coacch', '.peseta'))

source("lib/combine.R")

## First time: set combo to blank df
combo <- data.frame()
## All future times: Load it

load("~/Open Modeling Group Dropbox/COACCH/channels/Energy-Supply/adm3combo.RData")
known <- unique(paste(combo$scenario, combo$period, combo$county))

for (scn in unique(df$scenario)) {
  for (per in unique(df$period)) {
    for (cty in unique(df$county)){
      if (paste(scn, per, cty) %in% known)
        next

      print(c(scn, per, cty))
      mcs <- cbind(subset(df, scenario == scn & period == per & county == cty)$damage.mgbp.coacch,
                   subset(df, scenario == scn & period == per & county == cty)$damage.mgbp.peseta)
      stopifnot(sum(is.na(mcs)) == 0)

      byrid <- hier.combine(mcs)
      combo <- rbind(combo, data.frame(scenario=scn, period=per, county=cty, run_id=0:(length(byrid)-1), damage.mgbp=byrid))

      if (cty == unique(df$county)[200])
          save(combo, file="~/Open Modeling Group Dropbox/COACCH/channels/Energy-Supply/adm3combo.RData")
    }

    save(combo, file="~/Open Modeling Group Dropbox/COACCH/channels/Energy-Supply/adm3combo.RData")
  }
}

load("~/Open Modeling Group Dropbox/COACCH/channels/Energy-Supply/adm3combo.RData")
source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/report.R")
source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/constants.R")

library(dplyr)
combo.uk <- combo %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage.mgbp, na.rm=T))

combo.uk$percent <- combo.uk$damage.mgbp * 1e6 / gdp.2015.gbp * 100
tbl <- make.table(combo.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)
