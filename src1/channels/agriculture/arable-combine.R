setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

## 1. Load both projections
## 2. Combine along run_id

load("channels/agriculture/arable-proj-1-adm3.RData")
byadm31 <- byadm3.wlev
load("channels/agriculture/arable-proj-3-adm3.RData")
byadm33 <- byadm3.wlev

all(paste(byadm31$scenario, byadm31$period, byadm31$run_id, byadm31$x, byadm31$y) ==
    paste(byadm33$scenario, byadm33$period, byadm33$run_id, byadm33$x, byadm33$y))

source("lib/project.R")
source("channels/agriculture/amoc-calibrate.R")

if (F) {
    testing <- data.frame()

    for (scn in unique(alldraws$scenario)) {
        for (rid in unique(alldraws$run_id)) {
            print(c(scn, rid))
            temps <- alldraws[alldraws$scenario == scn & alldraws$run_id == rid,]
            amoccal <- get.amoc()

            share.years <- 2011:2100
            share.AMOC <- 1 - pmin(1, exp(-amoccal$bb * (temps$value[temps$year %in% share.years] - amoccal$dd)))

            testing <- rbind(testing, data.frame(scenario=scn, run_id=rid, year=share.years, AMOC=share.AMOC[1:length(share.years)]))
        }
    }

    testing2 <- testing %>% group_by(scenario, year) %>% summarize(frac=mean(AMOC > 0), mu=mean(AMOC), ci5=quantile(AMOC, .05), ci95=quantile(AMOC, .95))
    library(ggplot2)
    ggplot(testing2, aes(year, mu, colour=scenario)) +
        geom_line() + geom_ribbon(aes(ymin=ci5, ymax=ci95), alpha=.5)
}

byadm3 <- data.frame()
for (scn in unique(alldraws$scenario)) {
    for (rid in unique(alldraws$run_id)) {
        print(c(scn, rid))
        temps <- alldraws[alldraws$scenario == scn & alldraws$run_id == rid,]
        amoccal <- get.amoc()

        share.years <- 2011:2100
        share.AMOC <- 1 - pmin(1, exp(-amoccal$bb * (temps$value[temps$year %in% share.years] - amoccal$dd)))

        for (per in c('2011-2030', '2041-2060', '2081-2100')) {
            subbyadm31 <- subset(byadm31, scenario == scn & period == per & run_id == rid)
            subbyadm33 <- subset(byadm33, scenario == scn & period == per & run_id == rid)

            if (per == '2011-2030')
                period.share.AMOC <- mean(share.AMOC[share.years >= 2011 & share.years <= 2030])
            else if (per == '2041-2060')
                period.share.AMOC <- mean(share.AMOC[share.years >= 2041 & share.years <= 2060])
            else if (per == '2081-2100')
                period.share.AMOC <- mean(share.AMOC[share.years >= 2081 & share.years <= 2100])
            else
                print("Unknown period.")

            ## Combine scenario 1 and 3 projections
            byadm3 <- rbind(byadm3, cbind(data.frame(subbyadm31[, c('scenario', 'run_id', 'period', 'county')]), loss.mgbp=subbyadm31$loss.mgbp * (1 - period.share.AMOC) + subbyadm33$loss.mgbp * period.share.AMOC))
        }
    }
}

byadm3 %>% group_by(scenario, period, run_id) %>% summarize(loss.mgbp=sum(loss.mgbp, na.rm=T)) %>% group_by(scenario, period) %>% summarize(loss.mgbp=mean(loss.mgbp, na.rm=T))

byadm3.wlev <- byadm3
save(byadm3.wlev, file="channels/agriculture/arable-proj-mix-adm3.RData")
