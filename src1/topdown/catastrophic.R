setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

mu <- 0.260
se <- 0.267

alldraws <- read.csv("climate/gsat/ssps_26_70_posttp.csv")

alldraws$damage <- NA
for (rid in unique(alldraws$run_id)) {
    print(rid)
    coeff <- rnorm(1, mu, se)
    rows <- which(alldraws$run_id == rid)
    alldraws$damage[rows] <- coeff * alldraws$value[rows]^2 / 100
}

alldraws$period <- NA
alldraws$period[alldraws$year >= 2011 & alldraws$year <= 2030] <- "2011-2030"
alldraws$period[alldraws$year >= 2041 & alldraws$year <= 2060] <- "2041-2060"
alldraws$period[alldraws$year >= 2081 & alldraws$year <= 2100] <- "2081-2100"

library(dplyr)

results <- alldraws %>% group_by(scenario, period, run_id) %>% summarize(damage=mean(damage))

save(results, file="topdown/catastrophic.RData")
