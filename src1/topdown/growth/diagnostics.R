setwd("~/projects/ukcp/growth")

library(dplyr)

df <- read.csv("burkeetal.csv")
gdp0 <- read.csv("../socioeconomics/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_3469458/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_3469458.csv", skip=3)

## Cap growth at 5% per year

df.check <- df %>% group_by(rcp, cluster, region) %>% summarize(lo=min(diff(loss)), hi=max(diff(loss)))
quantile(df.check$lo)
quantile(df.check$hi)

df.clip <- df %>% group_by(rcp, cluster, region) %>% summarize(year, loss=c(loss[1], loss[1] + cumsum(pmax(-.05, pmin(diff(loss), .01)))))

write.csv(df.clip, "burkeetal2.csv", row.names=F)

library(reshape2)

gdp0.long <- melt(gdp0, id.vars=names(gdp0)[1:4])

gdp0.recent <- gdp0.long %>% group_by(Country.Code) %>% summarize(gdp=tail(value[!is.na(value)], 1))

df2 <- df.clip %>% left_join(gdp0.recent, by=c('region'='Country.Code'))
df2$gdp[is.na(df2$gdp)] <- 0

checks <- subset(df2, year == 2099 & rcp == 'rcp45') %>% group_by(cluster, region) %>% summarize(change=mean((exp(loss) - 1) * gdp), gdp=mean(gdp))
checks[which.max(checks$change),]

df3 <- df2 %>% group_by(rcp, cluster, year) %>% summarize(loss=sum(exp(loss) * gdp) / sum(gdp))

library(ggplot2)

ggplot(df3, aes(year, loss, group=factor(cluster))) +
    facet_wrap(~ rcp, ncol=1) +
    geom_line(alpha=.5) + coord_cartesian(ylim=c(0, 1.25)) +
    theme_bw() + ylab("Change in consumption") + xlab(NULL)

as.data.frame(df %>% group_by(year) %>% summarize(loss=mean(loss)))
