setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(readxl)

## PESETA III

df.p3 <- read_excel("~/Dropbox/PESETA2-RiskyBusiness/data/P3 Welfare and dd effects_for ERL compare article.xlsx", sheet=2, skip=2)
df.p3.uk <- df.p3[!is.na(df.p3$...1) & df.p3$...1 == "UK & Irland",]
df.p3.uk$dgt <- c(2, 4)

library(reshape2)
df.p3.uk2 <- melt(df.p3.uk[, -which(names(df.p3.uk) %in% c('...1', 'TOTAL'))], 'dgt')

## PESETA IV

df.p4 <- subset(read.csv("synthesis/pesetaiv.csv"), Measure == "Welfare (% of GDP)")
gdp.2015 <- 2089276 / 1e3 # billion GBP

df.p42 <- melt(df.p4, c('Channel', 'Measure'))
df.p42$temp <- as.numeric(sapply(df.p42$variable, function(ss) substring(as.character(ss), 2, nchar(as.character(ss))-1)))
df.p42$damage <- -df.p42$value * gdp.2015 / 100
df.p42$templabel <- paste(df.p42$temp, 'C')

library(ggplot2)

ggplot(df.p42, aes(templabel, damage, fill=Channel)) +
    geom_bar(stat='identity') + theme_bw() + ylab("Welfare losses, billion GBP") +
    xlab("Global temperature change (C)")

## UK

df.bf <- read.csv("channels/infrastructure/flood-business2.csv")

df.ws <- read.csv("channels/other/windstorms.csv")
## baseline is 528 (2015 €million)

df.fl <- read.csv("channels/flooding/sayers-2020.csv")

