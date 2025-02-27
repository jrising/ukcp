setwd("~/Open Modeling Group Dropbox/UK Economic Risks/climate/best")

df.global <- read.table("Land_and_Ocean_summary.txt", skip=58)[, c(1, 4, 8)]
names(df.global) <- c('year', 'gsat', 'gmst')

df.land <- read.table("Complete_TAVG_summary.txt", skip=22)[, c(1, 4)]
names(df.land) <- c('year', 'gsat')

df.europe <- read.table("europe-TAVG-Trend.txt", skip=71)[, c(1, 7)]
names(df.europe) <- c('year', 'gsat')

library(dplyr)
df.europe2 <- df.europe %>% group_by(year) %>% summarize(gsat=mean(gsat))

df.uk <- read.table("united-kingdom-TAVG-Trend.txt", skip=71)[, c(1, 7)]
names(df.uk) <- c('year', 'gsat')
df.uk2 <- df.uk %>% group_by(year) %>% summarize(gsat=mean(gsat))

df <- df.global[, c('year', 'gsat')] %>% left_join(df.land, by='year', suffix=c('', '.land')) %>%
    left_join(df.europe2, by='year', suffix=c('', '.eu')) %>%
    left_join(df.uk2, by='year', suffix=c('', '.uk'))

names(df) <- c('year', 'Global Ocean + Land', 'Global Land', 'Europe + UK', 'United Kingdom')

library(reshape2)
df2 <- melt(df, 'year')

library(ggplot2)

ggplot(df2, aes(year, value, colour=variable)) +
    geom_smooth(span=.5, se=F) + scale_x_continuous(expand=c(0, 0)) +
    theme_bw()

baselines <- subset(df2, year >= 1850 & year < 1900) %>% group_by(variable) %>% summarize(base=mean(value, na.rm=T))
subset(df2, year >= 1981 & year < 2010) %>% group_by(variable) %>% summarize(base=mean(value, na.rm=T))

df3 <- df2 %>% left_join(baselines)
df3$variable <- factor(df3$variable, levels=rev(c('Global Ocean + Land', 'Global Land', 'Europe + UK', 'United Kingdom')))

ggplot(df3, aes(year, value - base, colour=variable)) +
    geom_smooth(span=.5, se=F) + scale_x_continuous(NULL, expand=c(0, 0)) +
    theme_bw() + scale_colour_discrete(NULL) + ylab("Change in temperature (C),\n relative to 1850 - 1900")
ggsave("historical.pdf", width=6.5, height=3)
