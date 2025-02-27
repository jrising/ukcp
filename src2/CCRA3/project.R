setwd("~/Open Modeling Group Dropbox/COACCH/CCRA3")

library(dplyr)
library(ggplot2)

finres <- read.csv("missrisk-news.csv")

## growrate <- mean(diff(log(c(1314.245, 1329.186, 1350.140, 1382.128, 1418.235, 1455.828, 1495.583))))
## gdp.2015.gbp <- 2089276e6 # GBP, from https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2
## gdp.proj <- gdp.2015.gbp * exp(growrate * (c(2020, 2050, 2080) - 2015))

## gdp.proj.byt <- c(gdp.proj[1], gdp.proj[2], gdp.proj[3], gdp.proj[3])
## finres$gbp <- finres$affected * rep(gdp.proj.byt, each=max(finres$mc))

cdiffs <- c(0, 2, 4)
cdiffs2 <- cdiffs^2

coeff <- matrix(NA, 0, 3)
for (ii in 1:max(finres$mc)) {
    yy <- c(0, finres$affected[finres$mc == ii & finres$temp %in% c("2 C", "4 C")])
    mod <- lm(yy ~ cdiffs + cdiffs2)
    coeff <- rbind(coeff, mod$coeff)
}
la <- list(coeff=coeff)

ddf <- data.frame(T=seq(0, 6, length.out=100))
ddf$mu <- sapply(ddf$T, function(TT) mean(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2))
ddf$ci25 <- sapply(ddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .25))
ddf$ci75 <- sapply(ddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .75))

finres2 <- finres %>% group_by(temp) %>% summarize(mu=mean(affected), ci01=quantile(affected, .01), ci25=quantile(affected, .25),
                                                   ci50=median(affected), ci75=quantile(affected, .75), ci99=quantile(affected, .99))
finres2$T <- c(1, 1.5, 2, 4)

ggplot(ddf, aes(T, mu)) +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_boxplot(data=finres2, aes(group=temp, ymin=ci01, lower=ci25, middle=ci50, upper=ci75, ymax=ci99, fill=temp), stat='identity', alpha=.75) +
    geom_point(data=finres2) +
    theme_bw() + xlab("Change in GMST from pre-industrial (°C)") +
    ylab("GDP loss (%)") + scale_y_continuous(labels=scales::percent) +
    scale_x_continuous(expand=c(0, 0)) +
    scale_fill_manual("Warming", breaks=c('1 C', '1.5 C', '2 C', '4 C'), values=c('#808080', '#808080', '#FFFFFF', '#FFFFFF'))

alldraws <- read.csv("../../UK Economic Risks/climate/gsat/ssps_26_70_posttp.csv")

alldraws$period <- NA
alldraws$period[alldraws$year >= 2011 & alldraws$year <= 2030] <- "2011-2030"
alldraws$period[alldraws$year >= 2041 & alldraws$year <= 2060] <- "2041-2060"
alldraws$period[alldraws$year >= 2081 & alldraws$year <= 2100] <- "2081-2100"
periods <- c("2011-2030", "2041-2060", "2081-2100")

alldraws$damage <- NA
for (rid in unique(alldraws$run_id)) {
    print(rid)
    draw <- sample.int(dim(la$coeff)[1], 1)
    coeffs <- la$coeff[draw, ]
    for (period in periods) {
        rows <- which(alldraws$period == period & alldraws$run_id == rid)
        if (period == "2011-2030")
            alldraws$damage[rows] <- finres$affected[finres$mc == draw & finres$temp == '1 C']
        else if (period == "2041-2060")
            alldraws$damage[rows] <- finres$affected[finres$mc == draw & finres$temp == '1.5 C']
        else if (period == "2081-2100")
            alldraws$damage[rows] <- coeffs[2] * alldraws$value[rows] + coeffs[3] * alldraws$value[rows]^2
    }
}

results <- subset(alldraws, !is.na(period)) %>% group_by(scenario, period, run_id) %>% summarize(damage=mean(damage))

results %>% group_by(scenario, period) %>% summarize(damage=mean(damage))

source("../../UK Economic Risks/lib/report.R")

results$percent <- results$damage * 100
tbl <- make.table(results, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

save(results, file="missrisk-news.RData")
