setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(xtable)
library(readxl)
library(dplyr)
source("channels/slr/n95mc.R")
source("lib/report.R")
source("lib/constants.R")

convert.2020usdgbp <- 0.80930 # https://www1.oanda.com/currency/converter/ for 06/01/2020
inflation.factor <- 1.2

sheetnames <- excel_sheets("channels/slr/UK results.xlsx")

for (output in c('Inundation', 'Relocation', 'Storms', 'Wetland', 'Protection', 'total')) {
    allmcs <- data.frame() # scenario, period,
    for (period in c(2020, 2050, 2090)) {
        for (scenario in c('126', '370')) {
            print(c(output, period, scenario))

            params <- c()
            for (adapt in c('noadapt', 'optimal')) {
                col1 <- as.character(period - 10)
                col2 <- as.character(period)
                col3 <- as.character(period + 10)

                sheet <- which(sheetnames == paste(scenario, '50', adapt, tolower(output), sep='_'))
                if (length(sheet) == 0)
                    mu <- 0
                else {
                    df <- read_excel("channels/slr/UK results.xlsx", sheet=sheet)
                    mu <- inflation.factor * sum((df[, col1, drop=T] + 2 * df[, col2, drop=T] + df[, col3, drop=T]) / 4)
                }

                if (output == 'total') {
                    sheet <- which(sheetnames == paste(scenario, '5', adapt, output, sep='_'))
                    df <- read_excel("channels/slr/UK results.xlsx", sheet=sheet)

                    ci5 <- inflation.factor * sum((df[, col1, drop=T] + 2 * df[, col2, drop=T] + df[, col3, drop=T]) / 4)

                    sheet <- which(sheetnames == paste(scenario, '95', adapt, output, sep='_'))
                    df <- read_excel("channels/slr/UK results.xlsx", sheet=sheet)

                    ci95 <- inflation.factor * sum((df[, col1, drop=T] + 2 * df[, col2, drop=T] + df[, col3, drop=T]) / 4)

                    params <- c(params, fit.skewnormal(mu, ci5, ci95))
                } else {
                    sheet <- which(sheetnames == "Further pctiles")
                    df <- read_excel("channels/slr/UK results.xlsx", sheet=sheet, skip=1)

                    row1 <- which(df$Scenario == paste0('ssp', scenario, '_5_', adapt))
                    drp1 <- which(df$`Cost category`[row1 + (0:4)] == output)
                    if (length(drp1) == 0)
                        ci5 <- 0
                    else
                        ci5 <- (df[row1 + drp1 - 1, col1, drop=T] + 2 * df[row1 + drp1 - 1, col2, drop=T] + df[row1 + drp1 - 1, col3, drop=T]) / 4

                    row1 <- which(df$Scenario == paste0('ssp', scenario, '_95_', adapt))
                    drp1 <- which(df$`Cost category`[row1 + (0:4)] == output)
                    if (length(drp1) == 0)
                        ci95 <- 0
                    else
                        ci95 <- (df[row1 + drp1 - 1, col1, drop=T] + 2 * df[row1 + drp1 - 1, col2, drop=T] + df[row1 + drp1 - 1, col3, drop=T]) / 4

                    params <- c(params, fit.skewnormal(mu, ci5, ci95))
                }
            }

            values <- c(r.twoskew(1, params, 4000), r.twoskew(2, params, 4000))
            allmcs <- rbind(allmcs, data.frame(period=paste(period-9, period+10, sep=' - '),
                                               scenario=paste0('ssp', scenario), values))
        }
    }

    tbl <- make.table(allmcs, 'values')
    # print(xtable(tbl, digits=1), include.rownames=F)

    growrate <- mean(diff(log(c(1314.245, 1329.186, 1350.140, 1382.128, 1418.235, 1455.828, 1495.583))))
    gdp.proj <- gdp.2015 * exp(growrate * (c(2020, 2050, 2090) - 2015))
    allmcs2 <- allmcs %>% left_join(data.frame(gdp.proj, period=c("2011 - 2030", "2041 - 2060", "2081 - 2100")))
    allmcs2$percent <- 100 * allmcs2$value * 1e9 / allmcs2$gdp.proj
    print(xtable(make.table(allmcs2, 'percent'), digits=3), include.rownames=F)
}
