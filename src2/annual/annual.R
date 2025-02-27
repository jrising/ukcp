setwd("~/Dropbox/COACCH")
sspextdir <- "~/research/iamup/SSP-Extensions/results"

library(dplyr)
library(EnvStats)

do.common.econ <- T
column <- 'fracgdp.slr' #'fracgdp.flood' #'total'
suffix <- '-coastal' #'-flood' #''

if (!do.common.econ) {
    ssp1 <- read.csv(file.path(sspextdir, "ssp2.0-OECD Env-Growth-SSP1-gdppc-usd.csv")) %>%
        filter(Region == 'GBR' & year <= 2100) %>%
        left_join(read.csv(file.path(sspextdir, "ssp2.0-IIASA GDP-SSP1-gdppc-usd.csv")),
                  by=c('Region', 'year'), suffix=c('', '.gdppc2')) %>%
        left_join(read.csv(file.path(sspextdir, "ssp2.0-OECD Env-Growth-SSP1-pop-million.csv")),
                  by=c('Region', 'year'), suffix=c('.gdppc1', '.pop'))
    ssp3 <- read.csv(file.path(sspextdir, "ssp2.0-OECD Env-Growth-SSP3-gdppc-usd.csv")) %>%
        filter(Region == 'GBR' & year <= 2100) %>%
        left_join(read.csv(file.path(sspextdir, "ssp2.0-IIASA GDP-SSP3-gdppc-usd.csv")),
                  by=c('Region', 'year'), suffix=c('', '.gdppc2')) %>%
        left_join(read.csv(file.path(sspextdir, "ssp2.0-OECD Env-Growth-SSP3-pop-million.csv")),
                  by=c('Region', 'year'), suffix=c('.gdppc1', '.pop'))
} else {
    ssp1 <- read.csv(file.path(sspextdir, "ssp2.0-OECD Env-Growth-SSP2-gdppc-usd.csv")) %>%
        filter(Region == 'GBR' & year <= 2100) %>%
        left_join(read.csv(file.path(sspextdir, "ssp2.0-IIASA GDP-SSP2-gdppc-usd.csv")),
                  by=c('Region', 'year'), suffix=c('', '.gdppc2')) %>%
        left_join(read.csv(file.path(sspextdir, "ssp2.0-OECD Env-Growth-SSP2-pop-million.csv")),
                  by=c('Region', 'year'), suffix=c('.gdppc1', '.pop'))
    ssp3 <- ssp1
}

all(ssp3$value.lb.pop == ssp3$value.ub.pop)
all(ssp3$value.lb.gdppc1 == ssp3$value.ub.gdppc1)
all(ssp3$value.lb.gdppc2 == ssp3$value.ub.gdppc2)
all(ssp3$value.lb.gdppc2 <= ssp3$value.ub.gdppc1)

load("synthesis/alldamage-uk-dblcnt.RData")

results <- data.frame()
for (ssp in unique(alldamage.uk.dblcnt$scenario)) {
    for (mc in 0:max(alldamage.uk.dblcnt$run_id)) {
        print(c(ssp, mc))

        if (ssp == 'ssp126')
            gdps <- runif(nrow(ssp1), pmin(ssp1$value.gdppc1, ssp1$value.gdppc2) * ssp1$value.pop * 1e6,
                          pmax(ssp1$value.gdppc1, ssp1$value.gdppc2) * ssp1$value.pop * 1e6)
        else
            gdps <- runif(nrow(ssp3), pmin(ssp3$value.gdppc1, ssp3$value.gdppc2) * ssp3$value.pop * 1e6,
                          pmax(ssp3$value.gdppc1, ssp3$value.gdppc2) * ssp3$value.pop * 1e6)

        values <- alldamage.uk.dblcnt[alldamage.uk.dblcnt$run_id == mc & alldamage.uk.dblcnt$scenario == ssp, column, drop=T]
        years <- c(2020, 2050, 2090)
        total <- predict(lm(values ~ poly(years, 2)), data.frame(years=2015:2100))

        results <- rbind(results, data.frame(scenario=ssp, year=2015:2100, run_id=mc, total, gdps, damage=total * gdps))
    }
}

## Have it in 2015 USD; Want 2023 GBP
## GDP price deflator from https://fred.stlouisfed.org/series/GDPDEF#0
## 2023 exchange rate from https://www.exchangerates.org.uk/USD-GBP-spot-exchange-rates-history-2023.html
results$gdps.gbp2023 <- (results$gdps * 122.266 / 97.315) * 0.8043 / 1e9
results$damage.gbp2023 <- (results$damage * 122.266 / 97.315) * 0.8043 / 1e9
results$total.percent <- results$total * 100

results2 <- results %>% group_by(scenario, year) %>%
    reframe(group=c('Damage (%)', 'GDP (billion 2023 GBP)', 'Damage (billion 2023 GBP)'),
            value.lb=c(quantile(total.percent, .05), quantile(gdps.gbp2023, .05), quantile(damage.gbp2023, .05)),
            value.ub=c(quantile(total.percent, .95), quantile(gdps.gbp2023, .95), quantile(damage.gbp2023, .95)),
            value=c(mean(total.percent), mean(gdps.gbp2023), mean(damage.gbp2023)))

library(ggplot2)

results2$group <- factor(results2$group, c('Damage (%)', 'GDP (billion 2023 GBP)', 'Damage (billion 2023 GBP)'))

if (!do.common.econ) {
    ggplot(results2, aes(year, value, group=scenario)) +
        facet_wrap(~ group, ncol=1, scales='free_y') +
        geom_line(aes(colour=scenario)) +
        geom_ribbon(aes(ymin=value.lb, ymax=value.ub), alpha=.33) +
        scale_x_continuous(NULL, expand=c(0, 0)) + ylab("Annual values (units differ by panel)") +
        scale_colour_manual("Scenario:", breaks=c('ssp126', 'ssp370'), labels=c("High Mitigation", "Baseline"), values=c('#1b9e77', '#d95f02')) +
        theme_bw()
    ggsave("annual.pdf", width=6.5, height=6)
    write.csv(results2, "annual.csv", row.names=F)
} else {
    ggplot(subset(results2, group != "GDP (billion 2023 GBP)"), aes(year, value, group=scenario)) +
        facet_wrap(~ group, ncol=1, scales='free_y') +
        geom_line(aes(colour=scenario)) +
        geom_ribbon(aes(ymin=value.lb, ymax=value.ub), alpha=.33) +
        scale_x_continuous(NULL, expand=c(0, 0)) + ylab("Annual values (units differ by panel)") +
        scale_colour_manual("Scenario:", breaks=c('ssp126', 'ssp370'), labels=c("High Mitigation", "Baseline"), values=c('#1b9e77', '#d95f02')) +
        theme_bw()
    ggsave(paste0("annual", suffix, "-ssp2.pdf"), width=6.5, height=6)
    write.csv(results2, paste0("annual", suffix, "-ssp2.csv"), row.names=F)
}
