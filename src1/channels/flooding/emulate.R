setwd("~/Open Modeling Group Dropbox/UK Economic Risks/channels/flooding")

df <- read.csv("table6-9-tabula.csv")

library(reshape2)

df2 <- melt(df, c('Adaptation', 'Country', 'BaseEAD'))
df2$variable <- as.character(df2$variable)
df2$year <- substring(df2$variable, 2, 5)
df2$pop <- substring(df2$variable, 7, nchar(df2$variable) - 2)
df2$dt.2100 <- as.numeric(substring(df2$variable, nchar(df2$variable), nchar(df2$variable)))
df2$value <- as.character(df2$value)
df2$percchange <- as.numeric(substring(df2$value, 1, nchar(df2$value) - 1))
df2$EAD <- df2$BaseEAD * (1 + df2$percchange / 100)

## Estimate temperature in 2050 and 2080 under 2C and 4C -by-2100 scenarios



## Actually, this whole idea is crap. Need to figure out true appropriate delta T...

df.deltat.45 <- subset(read.csv("~/Dropbox/NextGen IAMs/Growth/data/temps/rcp45.csv"), region == "")
df.deltat.85 <- subset(read.csv("~/Dropbox/NextGen IAMs/Growth/data/temps/rcp85.csv"), region == "")

df.deltat.45$dt <- df.deltat.45$mean - mean(subset(df.deltat.45, year >= 1986 & year <= 2005)$mean) + 0.61
df.deltat.85$dt <- df.deltat.85$mean - mean(subset(df.deltat.85, year >= 1986 & year <= 2005)$mean) + 0.61

df.model <- data.frame(dt.2100=c(tail(df.deltat.45$dt, 1), tail(df.deltat.85$dt, 1)),
                       dt.2050=c(subset(rbind(df.deltat.45, df.deltat.85), year == 2050)$dt),
                       dt.2080=c(subset(rbind(df.deltat.45, df.deltat.85), year == 2080)$dt))

mod.2050 <- lm(dt.2050 ~ dt.2100, data=df.model)
mod.2080 <- lm(dt.2080 ~ dt.2100, data=df.model)

df2$dtatt[df2$year == 2050] <- predict(mod.2050, df2[df2$year == 2050, ])
df2$dtatt[df2$year == 2080] <- predict(mod.2080, df2[df2$year == 2080, ])

library(ggplot2)

ggplot(subset(df2, pop == 'None'), aes(
