### Combine by adm3

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)

source("lib/constants.R")

load("~/Open Modeling Group Dropbox/COACCH/channels/Forestry/results-final.RData")
byadm3.coacch <- byadm3.match
byadm3.coacch$damage.mgbp <- byadm3.coacch$damage*(-1)

load("~/Open Modeling Group Dropbox/UK Economic Risks/channels/biosphere/forest-projs-adm3.RData")
byadm3.loss <- byadm3.mcs
byadm3.loss$run_id <- as.numeric(byadm3.loss$mcid)
byadm3.loss$damage.mgbp <- byadm3.loss$value * 1e9 * convert.usd2gbp.2015 / 1e6

df <- byadm3.coacch %>% left_join(byadm3.loss, by=c('scenario', 'run_id', 'period', 'county'='ADM3'), 
                                  suffix=c('.coacch', '.ciam'))

source("lib/combine.R")

## First time: set combo to blank df
combo <- data.frame()
## All future times: Load it

load("~/Open Modeling Group Dropbox/COACCH/channels/Forestry/adm3combo.RData")
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
        save(combo, file="~/Open Modeling Group Dropbox/COACCH/channels/SLR/adm3combo.RData")
    }
    
    save(combo, file="~/Open Modeling Group Dropbox/COACCH/channels/SLR/adm3combo.RData")
  }
}

load("~/Open Modeling Group Dropbox/COACCH/channels/SLR/adm3combo.RData")

source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/report.R")
source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/constants.R")

library(dplyr)
combo.uk <- combo %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage.mgbp, na.rm=T))

combo.uk$percent <- combo.uk$damage.mgbp * 1e6 / gdp.2015.gbp * 100
tbl <- make.table(combo.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)
