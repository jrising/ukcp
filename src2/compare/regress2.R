setwd("~/Dropbox/COACCH/compare")

library(dplyr)
source("../../UK Economic Risks/lib/constants.R")

load("../synthesis/alldamage-uk.RData")

load("../channels/SLR/results-final.RData")
slr.noa <- byadm3.match %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage, na.rm=T))
slr.noa$fracgdp <- -slr.noa$damage.mgbp * 1e6 / gdp.2015.gbp

load("~/Dropbox/COACCH/channels/SLR-AdA/results-final.RData")
slr.ada <- byadm3.match %>% group_by(scenario, run_id, period) %>% summarize(damage.mgbp=sum(damage, na.rm=T))
slr.ada$fracgdp <- -slr.ada$damage.mgbp * 1e6 / gdp.2015.gbp

load("~/Dropbox/UK Economic Risks/channels/slr/slr-projections-adm3.RData")
slr.ciam <- byadm3.mcs %>% group_by(scenario, mcid, period) %>% summarize(value.b=sum(value, na.rm=T))
slr.ciam$run_id <- as.numeric(slr.ciam$mcid)

