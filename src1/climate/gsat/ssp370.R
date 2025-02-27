setwd("~/Open Modeling Group Dropbox/UK Economic Risks/climate/gsat")

library(dplyr)
library(ggplot2)

df <- read.csv("ssps_26_70_posttp.csv")

write.csv(subset(df, year == 2050)[, c('model', 'scenario', 'value')], "~/groups/ccecon-2023/practica/posttp.csv", row.names=F)

df2 <- df %>% group_by(year, scenario) %>% summarize(mu=mean(value), ci5=quantile(value, .05),
                                                     ci95=quantile(value, .95))


df2$ssplabel <- "SSP1-2.6"
df2$ssplabel[df2$scenario == 'ssp370'] <- "SSP3-7.0"

ggplot(subset(df2, scenario == 'ssp370' & year >= 1950 & year <= 2100), aes(year, mu, colour=ssplabel, fill=ssplabel)) +
    geom_line(alpha=.5) +
    geom_ribbon(aes(ymin=ci5, ymax=ci95), alpha=.5) +
    scale_x_continuous(NULL, expand=c(0, 0)) + ylab("GSAT relative to pre-industrial") +
    theme_bw() + scale_colour_discrete("FaIR scenario:") + scale_fill_discrete("FaIR scenario:") +
    scale_linetype_discrete("Assumption:")
