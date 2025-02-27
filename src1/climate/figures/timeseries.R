setwd("~/Open Modeling Group Dropbox/UK Economic Risks/climate")

library(dplyr)

df.fair.all <- read.csv("gsat/ssps_26_70_posttp.csv")

## Make distributions
library(ggplot2)
df.2100 <- subset(df.fair.all, year == 2100)
df.2100$label <- "High mitigation (SSP1-2.6)"
df.2100$label[df.2100$scenario == 'ssp370'] <- "Business-as-usual (SSP3-7.0)"

ggplot(df.2100, aes(value)) +
    facet_wrap(~ label, ncol=1) +
    geom_histogram(bins=50) + xlab("Warming in 2100, relative to pre-industrial (°C)") +
    ylab("Distribution of climate simulations") + theme_bw()

## Calculate difference from 1995 - 2005

as.data.frame(df.fair.all %>% filter(year >= 2081 & year <= 2100) %>% left_join(subset(df.fair.all, year >= 1995 & year <= 2005) %>% group_by(model, scenario, run_id) %>% summarize(baseline=mean(value))) %>% group_by(scenario, year) %>% summarize(mu=mean(value), p5=quantile(value, .05), p95=quantile(value, .95), mu.diff=mean(value - baseline), p5.diff=quantile(value - baseline, .05), p95=quantile(value - baseline, .95)))

df.fair <- df.fair.all %>% group_by(scenario, year) %>% summarize(p5=quantile(value, .05), p95=quantile(value, .95))
df.cmip <- read.csv("cmip6/gmst-annual.csv")

baselines <- data.frame()
for (model in unique(df.cmip$model)) {
    tas <- mean(df.cmip$gsat[df.cmip$model == model & df.cmip$scenario == 'historical' & df.cmip$year >= 1995 & df.cmip$year < 2015])
    baselines <- rbind(baselines, data.frame(model, baseline=tas))
}

baseline.fair <- 0.85 #mean(df.fair.all$value[df.fair.all$year >= 1995 & df.fair.all$year < 2015])

library(zoo)

df.cmip2 <- data.frame()
for (model in unique(df.cmip$model)) {
    print(model)
    for (scenario in c('ssp126', 'ssp370')) {
        baseline <- baselines$baseline[baselines$model == model]
        values <- c(df.cmip$gsat[df.cmip$model == model & df.cmip$scenario == 'historical'],
                    df.cmip$gsat[df.cmip$model == model & df.cmip$scenario == scenario])
        clims <- rollmean(values - baseline + baseline.fair, 30)
        df.cmip2 <- rbind(df.cmip2, data.frame(year=(1850+14):(2100-15), model, scenario, clim=clims))
    }
}

library(ggplot2)

df.fair$ssplabel <- "SSP1-2.6"
df.fair$ssplabel[df.fair$scenario == 'ssp370'] <- "SSP3-7.0"

ggplot(df.fair, aes(year)) +
    coord_cartesian(xlim=c(1850, 2100), ylim=c(-.5, 5.5)) +
    geom_ribbon(aes(fill=ssplabel, ymin=p5, ymax=p95), alpha=.5) +
    geom_line(data=df.cmip2, aes(y=clim, colour=model, group=paste(model, scenario))) +
    scale_x_continuous(NULL, expand=c(0, 0)) + ylab("GSAT relative to pre-industrial") +
    theme_bw() + scale_colour_discrete("GCM:") + scale_fill_discrete("FAIR scenario:")
