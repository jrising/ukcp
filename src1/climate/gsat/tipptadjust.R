setwd("~/Open Modeling Group Dropbox/UK Economic Risks/climate/gsat")

df <- read.csv("ssps_26_70_ukproject_allmembers.csv")
adds <- read.csv("../tippts/additions.csv")

quants <- unique(adds$quantile)

newmcs <- data.frame()
for (scen in unique(df$scenario)) {
    for (runid in unique(df$run_id[df$scenario == scen])) {
        print(c(scen, runid))
        subdf <- subset(df, scenario == scen & run_id == runid)
        ## Choose a random quantile
        qq <- sample(quants, 1)
        addsqq <- subset(adds, quantile == qq)
        ## Clean up and add data for extrapolation
        addsqq$adds[addsqq$adds < 0 & addsqq$indep < 4] <- 0
        addsqq <- rbind(addsqq, data.frame(indep=seq(-0.05, .25, by=.1), quantile=qq, adds=0),
                        data.frame(indep=seq(7.75, 13.15, by=.1), quantile=qq, adds=addsqq$adds[nrow(addsqq)]))
        ## Model changes
        mod <- loess(adds ~ indep, addsqq)
        toadd <- predict(mod, subdf$value[subdf$year > 2014])
        toadd <- toadd * (1 - exp(-(1:length(toadd)) / 10))

        newmcs <- rbind(newmcs, data.frame(model=subdf$model, scenario=scen, run_id=runid,
                                           year=subdf$year,
                                           value=c(subdf$value[subdf$year <= 2014],
                                                   subdf$value[subdf$year > 2014] + toadd)))
    }
}

## Don't need to adjust baseline-- not affecting pre-2015 values
## baselines <- subset(newmcs, year >= 1995 & year < 2015) %>% group_by(scenario) %>% summarize(mu=mean(value))
## for (scen in unique(df$scenario)) {
##     offset <- 0.85 - baselines$mu[baselines$scenario == scen]
##     newmcs$value[newmcs$scenario == scen] <- newmcs$value[newmcs$scenario == scen] + offset
## }

library(dplyr)
library(ggplot2)

## newmcs <- read.csv("ssps_26_70_posttp.csv")

df2 <- df %>% group_by(year, scenario) %>% summarize(mu=mean(value), ci5=quantile(value, .05),
                                                     ci95=quantile(value, .95))
newmcs2 <- newmcs %>% group_by(year, scenario) %>% summarize(mu=mean(value), ci5=quantile(value, .05),
                                                             ci95=quantile(value, .95))

pdf <- rbind(cbind(group='Before Tipping Points', df2), cbind(group='After Tipping Points', newmcs2))

pdf$ssplabel <- "SSP1-2.6"
pdf$ssplabel[pdf$scenario == 'ssp370'] <- "SSP3-7.0"

ggplot(pdf, aes(year, mu, colour=ssplabel, fill=ssplabel, linetype=group)) +
    geom_line(alpha=.5) +
    geom_ribbon(aes(ymin=ci5, ymax=ci95), alpha=.5) +
    scale_x_continuous(NULL, expand=c(0, 0)) + ylab("GSAT relative to pre-industrial") +
    theme_bw() + scale_colour_discrete("FaIR scenario:") + scale_fill_discrete("FaIR scenario:") +
    scale_linetype_discrete("Assumption:")

write.csv(newmcs, "ssps_26_70_posttp.csv", row.names=F)

subset(newmcs2, year == 2100)
