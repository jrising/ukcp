setwd("~/Open Modeling Group Dropbox/UK Economic Risks/climate/cmip6")

library(dplyr)

df <- read.csv("gmst-annual.csv")

df2 <- df %>% group_by(model) %>% summarize(base=mean(gsat[year >= 1850 & year < 1880]), x2012=mean(gsat[year == 2012]))
mean(df2$diff <- df2$x2012 - df2$base)
