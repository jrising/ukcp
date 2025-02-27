setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/damagefunc.R")

library(reshape2)

df <- subset(read.csv("synthesis/pesetaiv.csv"), Measure == "Welfare (% of GDP)")

uk.cdiff <- list('X1.5C'=1 - .5 / 3, 'X2C'=1 + .5 / 3, 'X3C'=1.5 + (2/3)*.5)

df2 <- melt(df, c('Channel', 'Measure'))
df2$temp <- sapply(df2$variable, function(ss) uk.cdiff[[ss]])

## Based on wheat sensitivity analysis for South Central Europe (most significantly estimated region)
wheat.ci90 <- c(0.69 - -1.25, 0.68 - -1.76, 0.63 - -2.87) #c(1.24 - -.80, 1.21 - -0.82, 1.30 - -1.04)
wheat.med <- c(-0.4, -0.7, -1.3) #c(0, 0, -.1)
wheat.sd <- (wheat.ci90 / 2) / 1.64 # qnorm(.95, 0, wheat.sd) - qnorm(.05, 0, wheat.sd) ~= wheat.ci90

sd.scale <- abs(mean(wheat.sd) / mean(wheat.med))

allpdf <- data.frame()
allddf <- data.frame()
alllas <- list()
for (chan in unique(df2$Channel)) {
    subdf <- subset(df2, Channel == chan)
    la <- fit.damages(c(0, subdf$temp), c(0, -subdf$value), sd.scale * c(0, -subdf$value), quadprior=T)

    info <- get.plotinfo(la, c(0, subdf$temp), c(0, -subdf$value), sd.scale * c(0, -subdf$value))

    allpdf <- rbind(allpdf, cbind(channel=chan, info$pdf))
    allddf <- rbind(allddf, cbind(channel=chan, info$ddf))
    alllas[[chan]] <- la
}

ggplot(allddf, aes(T, mu)) +
    facet_wrap(~ channel, scales='free_y') +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_point(data=allpdf) +
    scale_x_continuous("Difference in temperature from 1981-2010 (C)", expand=c(0, 0)) +
    theme_bw() + ylab("Welfare damages (% GDP)")
ggsave("synthesis/damagefuncs.pdf", width=6.5, height=4)

## Project

source("lib/project.R")
source("lib/aggregate.R")

if (do.proj.as.gmst) {
    results <- data.frame()
    for (scenario in unique(alldraws$scenario)) {
        for (run_id in unique(alldraws$run_id[alldraws$scenario == scenario])) {
            print(c(scenario, run_id))

            warming <- get.warming.eoc(scenario, run_id)
            pattern <- get.pattern(scenario, warming)

            for (period in names(periods)) {
                subres <- data.frame()
                for (year in periods[[period]][1]:periods[[period]][2]) {
                    warming.now <- alldraws$value[alldraws$scenario == scenario & alldraws$run_id == run_id & alldraws$year == year]
                    cdiffs <- grid.pattern.cdiff(pattern, scenario, year)

                    fullcdiff <- warming.now + cdiffs$variable
                    for (chan in names(alllas)) {
                        draw <- sample.int(dim(alllas[[chan]]$coeff)[1], 1)
                        coeffs <- alllas[[chan]]$coeff[draw, ]
                        damage <- coeffs[2] * fullcdiff + coeffs[3] * fullcdiff^2
                        county.damage <- county.aggregate(damage)
                        ## subres <- rbind(subres, data.frame(channel=chan, period, year, x=cdiffs$x, y=cdiffs$y, cdiff=cdiffs$variable, damage))
                        subres <- rbind(subres, data.frame(channel=chan, period, year, PID=1:nrow(county.polydata), damage=county.damage))
                    }
                }

                ##resrow <- subres %>% group_by(channel, period, x, y) %>% summarize(damage=mean(damage))
                resrow <- subres %>% group_by(channel, period, PID) %>% summarize(damage=mean(damage))
                results <- rbind(results, cbind(scenario, run_id, pattern, resrow))
            }
        }
    }

    save(results, file="synthesis/pesetaiv-project-new.RData")
}

agginfo0 <- get.standard.agginfo(0)

uk.results <- data.frame()
for (chan in names(alllas)) {
    print(chan)
    uk.results.chan <- grid.project(alllas[[chan]], 1981, 2010, aggregate.cdiff=function(cdiffs) {
        data.frame(x=mean(cdiffs$x), y=mean(cdiffs$y), variable=county.aggregate(cdiffs$variable, agginfo0))
    })
    uk.results <- rbind(uk.results, cbind(channel=chan, uk.results.chan))
}

save(uk.results, file="synthesis/pesetaiv-project-new.RData")
## load("synthesis/pesetaiv-project-new.RData")

source("lib/report.R")

## XXX: save didn't get run... uk.results didn't get saved. Do the mean lines again
## names(results)[1:3]  <- c('scenario', 'run_id', 'pattern')
## library(dplyr)
## uk.results <- results %>% group_by(scenario, run_id, channel, period) %>% summarize(damage=mean(damage, na.rm=T))

for (chan in unique(uk.results$channel)) {
    print(chan)
    tbl <- make.table(subset(uk.results, channel == chan), 'damage')
    if (chan == "Electricity Production")
        print(xtable(tbl, digits=3), include.rownames=F)
    else
        print(xtable(tbl, digits=2), include.rownames=F)
}

for (chan in names(alllas)) {
    if (file.exists(paste0("synthesis/pesetaiv-project-", chan, ".RData")) || chan == 'Coastal floods')
        next
    print(chan)
    results <- grid.project(alllas[[chan]], 1981, 2010)
    save(results, file= paste0("synthesis/pesetaiv-project-", chan, ".RData"))
}
