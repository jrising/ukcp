setwd("~/Open Modeling Group Dropbox/UK Economic Risks/channels/slr")

library(readxl)
source("n95mc.R")

convert.2020usdgbp <- 0.80930 # https://www1.oanda.com/currency/converter/ for 06/01/2020

do.byregion <- F

if (!do.byregion) {
    df <- read_excel("UK results.xlsx", skip=1)
    regions <- 'UK'
} else {
    sheetnames <- excel_sheets("UK results.xlsx")
    sheets <- grep("total", sheetnames)
    bysheet <- list()
    for (sh in sheets) {
        df <- read_excel("UK results.xlsx", sheet=sh)
        bysheet[[sheetnames[sh]]] <- df
    }
    regions <- bysheet[['126_50_noadapt_total']][, 1, drop=T]
}

allmcs <- data.frame()
results <- data.frame()

for (period in c(2020, 2050, 2090)) {
    for (scenario in c('ssp126', 'ssp370')) {
        for (region in regions) {
            print(c(period, scenario, region))

            params <- c()
            for (adapt in c('noadapt', 'optimal')) {
                col1 <- as.character(period - 10)
                col2 <- as.character(period)
                col3 <- as.character(period + 10)

                if (!do.byregion) {
                    rr <- which(df$Scenario == paste0(scenario, '_50_', adapt)) + ifelse(adapt == 'noadapt', 4, 5)
                    stopifnot(df$`Cost category`[rr] == 'Total')

                    mu <- (df[rr, col1, drop=T] + 2 * df[rr, col2, drop=T] + df[rr, col3, drop=T]) / 4
                    ci5 <- (df[which(df$Scenario == paste0(scenario, '_5_', adapt)), col1, drop=T] + 2 * df[which(df$Scenario == paste0(scenario, '_5_', adapt)), col2, drop=T] + df[which(df$Scenario == paste0(scenario, '_5_', adapt)), col3, drop=T]) / 4
                    ci95 <- (df[which(df$Scenario == paste0(scenario, '_95_', adapt)), col1, drop=T] + 2 * df[which(df$Scenario == paste0(scenario, '_95_', adapt)), col2, drop=T] + df[which(df$Scenario == paste0(scenario, '_95_', adapt)), col3, drop=T]) / 4
                } else {
                    sheetname.prefix <- paste0(gsub("ssp", "", scenario), '_')
                    sheetname.suffix <- paste0('_', adapt, '_total')
                    rr <- which(regions == region)
                    dfmu <- bysheet[[paste0(sheetname.prefix, '50', sheetname.suffix)]]
                    df5 <- bysheet[[paste0(sheetname.prefix, '5', sheetname.suffix)]]
                    df95 <- bysheet[[paste0(sheetname.prefix, '95', sheetname.suffix)]]

                    mu <- (dfmu[rr, col1, drop=T] + 2 * dfmu[rr, col2, drop=T] + dfmu[rr, col3, drop=T]) / 4
                    ci5 <- (df5[rr, col1, drop=T] + 2 * df5[rr, col2, drop=T] + df5[rr, col3, drop=T]) / 4
                    ci95 <- (df95[rr, col1, drop=T] + 2 * df95[rr, col2, drop=T] + df95[rr, col3, drop=T]) / 4
                }

                print(c(mu, ci5, ci95))
                params <- c(params, fit.skewnormal(mu, ci5, ci95))
            }

            if (!do.byregion)
                values <- c(r.twoskew(1, params, 4000), r.twoskew(2, params, 4000))
            else
                values <- c(r.twoskew(1, params, 1500), r.twoskew(2, params, 1500))
            allmcs <- rbind(allmcs, data.frame(period, scenario, region, values))

            qqs <- quantile(values, c(.05, .5, .95))

            results <- rbind(results, data.frame(period, scenario, region, mu=mean(values), ci5=qqs[1], ci50=qqs[2], ci95=qqs[3]))
        }
    }
}

library(ggplot2)
library(scales)

results$scenario.label <- "SSP1-2.6"
results$scenario.label[results$scenario == 'ssp370'] <- "SSP3-7.0"

if (!do.byregion) {
    write.csv(allmcs, "slr-projections.csv", row.names=F)

    ggplot(results, aes(x=paste(period-10, '-', period+10), mu)) +
        facet_wrap(~ scenario.label) +
        geom_col(alpha=.8, fill=muted('blue')) + geom_errorbar(aes(ymin=ci5, ymax=ci95), width=.25) +
        scale_y_continuous("Annual cossts (Billions of $2020)") + xlab(NULL) +
        theme_bw()
} else {
    save(allmcs, file="slr-projections-segment.RData")

    library(dplyr)

    diva <- read.csv("diva_published/data/gis/diva.csv")
    matching <- read.csv("matching.csv")
    matching2 <- matching %>% left_join(diva, by=c('segment'='locationid'))
    matching2$region <- sapply(matching2$locationna, function(ss) paste0("UnitedKingdom", as.numeric(gsub(" United Kingdom", "", ss))))

    library(PBSmapping)

    shp <- importShapefile("../../regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
    polydata <- attr(shp, 'PolyData')

    setwd("~/Open Modeling Group Dropbox/UK Economic Risks")
    source("lib/report.R")

    byadm3 <- data.frame()
    for (scen in c('ssp126', 'ssp370')) {
        for (per in c(2020, 2050, 2090)) {
            subres <- subset(results, scenario == scen & period == per)
            subres2 <- matching2 %>% left_join(subres) %>% group_by(Country, ADM2, ADM3) %>%
                summarize(mu=sum(mu * share, na.rm=T) / sum(share[!is.na(mu)]),
                          ci5=sum(ci5 * share, na.rm=T) / sum(share[!is.na(ci5)]),
                          ci95=sum(ci95 * share, na.rm=T) / sum(share[!is.na(ci95)]))

            subres3 <- subres2 %>% left_join(polydata, by=c('Country'='NAME_1', 'ADM2'='NAME_2', 'ADM3'='NAME_3'))
            shp2 <- shp %>% left_join(subres3[, c('PID', 'mu', 'ci5', 'ci95')])

            make.maps("channels/slr/figures", paste(scen, per, sep='-'), shp2, "Expected\ndamages\nper year\n(billion £)",
                      "95% CI\ndamages\nper year\n(billion £)", 0, 1.0, gradient=1)

            byadm3 <- rbind(byadm3, cbind(scenario=scen, period=paste0(per-9, '-', per+10), subres2))
        }
    }

    write.csv(byadm3, "channels/slr/slr-byadm3.csv", row.names=F)

    allmcs$mcid <- rep(1:3000, nrow(allmcs) / 3000)

    byadm3.mcs <- data.frame()
    for (scen in c('ssp126', 'ssp370')) {
        for (per in c(2020, 2050, 2090)) {
            print(c(scen, per))
            submcs <- subset(allmcs, scenario == scen & period == per)
            submcs2 <- matching2 %>% left_join(submcs) %>% group_by(Country, ADM2, ADM3, mcid) %>%
                summarize(value=sum(values * share, na.rm=T) / sum(share[!is.na(values)]))

            byadm3.mcs <- rbind(byadm3.mcs, cbind(scenario=scen, period=paste0(per-9, '-', per+10), submcs2))
        }
    }

    save(byadm3.mcs, file="slr-projections-adm3.RData")
}

## Questions:

## Instantaneous year, or decade? [assuming decade]
## Per year [per year]
## Units? [bn $2020]
