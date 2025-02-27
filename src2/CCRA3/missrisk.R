setwd("~/Dropbox/COACCH/CCRA3")

do.checks <- F
do.newonly <- F

library(readxl)
library(reshape2)
library(dplyr)
library(Matrix)

df <- read_excel("UK All Risks.xlsx", skip=1)
df <- subset(df, !is.na(Code) & !(Code %in% c('H1', 'H2', 'H3a', 'NE9')))

if (do.newonly) {
    df <- subset(df, !(Code %in% c('NE6a', 'I1', 'B2', 'H3b', 'H4', 'I3', 'I11', 'N10', 'NE17', 'B3', 'H10a', 'I8', 'H6a', 'H6b',
                                   'I6', 'I9', 'I10', # COACCH only does wind and hydro, but the CCRA categories overlap these.
                                   'NE6b', 'N11', 'B1', 'H3a', 'I2', 'I4', 'B5', 'H11', 'NE5', 'NE18',
                                   'NE14', 'NE15',
                                   'H1a',
                                   'ID8',
                                   'B6', 'I5', 'I12')))
}

## Drop Original and COACCH
df <- df[!(substring(df$Code, 1, 1) %in% c('A', 'C')), c(2:6, 12:29)]
df <- df %>% mutate_at(14:21, as.double)

## Generate correlation and double-counting matrices
## Hazard hierarchy: General -> (Heat, International, Disaster -> (Storm, Drought), Biological, SLR)
get.hazard.corr <- function(aa, bb) {
    if (grepl(',', aa)) {
        invp <- 1
        for (subaa in strsplit(aa, ', ')[[1]])
            invp <- invp * (1 - get.hazard.corr(subaa, bb))
        return(1 - invp)
    }
    if (grepl(',', bb)) {
        invp <- 1
        for (subbb in strsplit(bb, ', ')[[1]])
            invp <- invp * (1 - get.hazard.corr(aa, subbb))
        return(1 - invp)
    }
    if (aa == "Unique" || bb == "Unique")
        return(0)
    if (aa == "General" || bb == "General")
        return(.25)
    if (aa %in% c('Heat', 'International', 'Disaster', 'Biological', 'SLR')
        && bb %in% c('Heat', 'International', 'Disaster', 'Biological', 'SLR'))
        if (aa == bb)
            return(.5)
        else
            return(.25)
    if (aa %in% c('Storm', 'Drought') && bb %in% c('Storm', 'Drought'))
        if (aa == bb)
            return(.75)
        else
            return(.5)
    if (aa %in% c('Storm', 'Drought'))
        return(get.hazard.corr('Disaster', bb))
    if (bb %in% c('Storm', 'Drought'))
        return(get.hazard.corr(aa, 'Disaster'))
    if (aa %in% c('Heat', 'International', 'Disaster', 'Biological', 'SLR'))
        return(get.hazard.corr('General', bb))
    if (bb %in% c('Heat', 'International', 'Disaster', 'Biological', 'SLR'))
        return(get.hazard.corr(aa, 'General'))
}

## How much of aa is included in code
get.doublecount <- function(aa, code) {
    if (is.na(aa))
        return(0)
    if (grepl(code, aa))
        return(.5)
    if (aa == "Multiple")
        return(.25)
    return(0)
}

CC <- matrix(NA, nrow(df), nrow(df))
DD <- matrix(NA, nrow(df), nrow(df))
for (ii in 1:nrow(df)) {
    for (jj in 1:nrow(df)) {
        if (ii == jj) {
            CC[ii, jj] <- 1
            DD[ii, jj] <- 0
        } else {
            CC[ii, jj] <- get.hazard.corr(df$Hazards[ii], df$Hazards[jj])
            DD[ii, jj] <- get.doublecount(df$`Double Counting`[jj], df$Code[ii])
        }
    }
}
CC <- nearPD(CC, corr=T)$mat
rownames(CC) <- df$Code
colnames(CC) <- df$Code
rownames(DD) <- df$Code
colnames(DD) <- df$Code

## Convert GBP year to 2020
source("../../UK Economic Risks/lib/constants.R")
for (rr in 1:nrow(df)) {
    if (!is.na(df$`GBP Year`[rr])) {
        ratio <- deflator$wb[deflator$year == 2020] / deflator$wb[deflator$year == df$`GBP Year`[rr]]
        stopifnot(!is.na(ratio))
        for (cc in 14:21) # all mu and se
            df[rr, cc] <- df[rr, cc] * ratio
        df$`GBP Year`[rr] <- 2020
    }
}

## Use quadratic to account for baseline year
source("../../UK Economic Risks/lib/damagefunc.R")

gmsts <- read.csv("noaa-1880-2021.csv")
gmsts$c11 <- c(((cumsum(gmsts$Value) - c(rep(NA, 10), 0, cumsum(gmsts$Value[1:(nrow(gmsts)-11)]))) / 11)[6:nrow(gmsts)], rep(NA, 5))

df2.mu <- melt(df[, c(1:5, 14:17, 22)], names(df)[c(1:5, 22)], value.name='muval')
df2.mu$muval <- as.numeric(df2.mu$muval)
df2.mu$variable2 <- substring(df2.mu$variable, 1, nchar(as.character(df2.mu$variable)) - 2)
df2.se <- melt(df[, c(1:5, 18:21, 22)], names(df)[c(1:5, 22)], value.name='seval')
df2.se$seval <- as.numeric(df2.se$seval)
df2.se$variable2 <- substring(df2.se$variable, 1, nchar(as.character(df2.se$variable)) - 2)

offsets <- c()
for (code in unique(df2.mu$Code)) {
    baseyear <- df$`Baseline year`[df$Code == code]
    if (is.na(baseyear) || baseyear < 1885)
        next
    cdiffs <- c(gmsts$c11[gmsts$Year == baseyear], c(1, 1.5, 2, 4))
    impacts <- c(0, df2.mu$muval[df2.mu$Code == code])
    ses <- c(0, df2.se$seval[df2.se$Code == code])
    ## la <- fit.damages(cdiffs, impacts, ses)
    ## ggplot.damagefunc(la, cdiffs, impacts, ses)

    cdiffs2 <- cdiffs^2
    mod <- lm(impacts ~ cdiffs + cdiffs2)

    offset <- -mod$coeff[1]
    offse <- sqrt(vcov(mod)[1, 1])

    df2.mu$muval[df2.mu$Code == code] <- df2.mu$muval[df2.mu$Code == code] + offset
    df2.se$seval[df2.se$Code == code] <- sqrt(df2.se$seval[df2.se$Code == code]^2 + offse^2)

    offsets <- c(offsets, offset)
}

## Convert to long
df2.lo <- melt(df[, c(1:9, 22)], names(df)[c(1:5, 22)], value.name='loval')
df2.lo$loval <- as.numeric(df2.lo$loval)
df2.lo$variable2 <- substring(df2.lo$variable, 1, nchar(as.character(df2.lo$variable)) - 2)
df2.hi <- melt(df[, c(1:5, 10:13, 22)], names(df)[c(1:5, 22)], value.name='hival')
df2.hi$hival <- as.numeric(df2.hi$hival)
df2.hi$variable2 <- substring(df2.hi$variable, 1, nchar(as.character(df2.hi$variable)) - 2)

df2 <- df2.lo %>% left_join(df2.hi, by=c(names(df)[c(1:5, 22)], 'variable2')) %>%
    left_join(df2.mu, by=c(names(df)[c(1:5, 22)], 'variable2')) %>% left_join(df2.se, by=c(names(df)[c(1:5, 22)], 'variable2'))

df2$period <- NA
df2$period[grep('2020', df2$variable2)] <- 2020
df2$period[grep('2050', df2$variable2)] <- 2050
df2$period[grep('2080', df2$variable2)] <- 2080
df2$cdiff <- NA
df2$cdiff[grep('2020', df2$variable2)] <- 1
df2$cdiff[grep('2050', df2$variable2)] <- 1.5
df2$cdiff[grep('2080s, 2', df2$variable2)] <- 2
df2$cdiff[grep('2080s, 4', df2$variable2)] <- 4

sum(is.na(df2$loval) != is.na(df2$hival))

df2 <- df2[, c('Code', 'Risk...3', 'Independents', 'Double Counting', 'Hazards', 'Baseline year', 'loval', 'hival', 'muval', 'seval', 'period', 'cdiff')]
names(df2)[2] <- "Risk"

if (do.checks) {
    df2$problem <- NA
    df2$problem[df2$muval + df2$seval > df2$hival] <- "OoB-unk"
    df2$problem[df2$muval - df2$seval < df2$loval] <- "OoB-unk"
    df2$problem[df2$muval + df2$seval > df2$hival & sign(df2$muval) == -1 & sign(df2$hival) == -1] <- "OoB-low"
    df2$problem[df2$muval + df2$seval > df2$hival & sign(df2$muval) == 1 & sign(df2$hival) == 1] <- "OoB-high"
    df2$problem[df2$muval - df2$seval < df2$loval & sign(df2$muval) == -1 & sign(df2$hival) == -1] <- "OoB-high"
    df2$problem[df2$muval - df2$seval < df2$loval & sign(df2$muval) == 1 & sign(df2$hival) == 1] <- "OoB-low"

    table(subset(df2, !is.na(problem))$Code)
    subset(df2[, -2], !is.na(problem))
    ## B2 out of bounds (low), but no further information
    ## H3b out of bounds (sign varies), but consistent with paper
    ## H5 out of bounds (sign varies), but consistent with paper
    ## I2 mentions possibility of VH, so leaving
    ## ID7 out of bounds (high), but our translation seems plausible
    ## NE17 out of bounds (low), explained by partial habitat coverage in table

    ## Plot comparison
    library(ggplot2)
    ggplot(subset(df2, is.na(problem) & loval > 0 & muval > seval), aes(x=Code, y=muval)) +
        facet_wrap(~ period + cdiff) +
        coord_flip() +
        geom_errorbar(aes(ymin=loval, ymax=hival)) +
        geom_crossbar(aes(ymin=muval - seval, ymax=muval + seval)) +
        scale_y_log10()

    ggplot(subset(df2, is.na(problem) & hival < 0 & -muval > seval), aes(x=Code, y=-muval)) +
        facet_wrap(~ period + cdiff) +
        coord_flip() +
        geom_errorbar(aes(ymin=-loval, ymax=-hival)) +
        geom_crossbar(aes(ymin=-muval - seval, ymax=-muval + seval)) +
        scale_y_log10()

    require(scales)
    pseudolog_trans = function() trans_new("pseudolog", function(x) sign(x) * log(1 + abs(x)), function(x) sign(x) * (exp(abs(x)) - 1))

    ## Either good matches or estimates near estreme ends of bounds. Either is fine.
    ggplot(subset(df2, is.na(problem) & !(loval > 0 & muval > seval) & !(hival < 0 & -muval > seval)), aes(x=Code, y=muval)) +
        facet_wrap(~ period + cdiff) +
        coord_flip() +
        geom_errorbar(aes(ymin=loval, ymax=hival)) +
        geom_crossbar(aes(ymin=muval - seval, ymax=muval + seval)) +
        scale_y_continuous(trans="pseudolog", breaks=c(-1e10, -1e6, 0, 1e6, 1e10))
}

## Fill in blanks and sample

knowns <- df2 %>% group_by(Code) %>%
    summarize(bounds=sum(!is.na(loval)) > 1, mus=sum(!is.na(muval)) > 1,
              ses=sum(!is.na(seval)) > 1,
              signconsist=all(sign(loval[!is.na(loval) & loval != 0])[1] == sign(loval[!is.na(loval) & loval != 0])) &
                  all(sign(hival[!is.na(hival) & hival != 0])[1] == sign(hival[!is.na(hival) & hival != 0])) &
                  all(sign(muval[!is.na(muval) & muval != 0])[1] == sign(muval[!is.na(muval) & muval != 0])))

## Model increase where known
df2$cdiff2 <- df2$cdiff^2

df2$logabslo <- log(abs(df2$loval))
df2$logabslo[!is.finite(df2$logabslo)] <- NA
mod.lal <- lm(logabslo ~ cdiff + cdiff2 + Code, data=df2)

df2$logabshi <- log(abs(df2$hival))
df2$logabshi[!is.finite(df2$logabshi)] <- NA
mod.lah <- lm(logabshi ~ cdiff + cdiff2 + Code, data=df2)

df2$logabsmu <- log(abs(df2$muval))
df2$logabsmu[!is.finite(df2$logabsmu)] <- NA
mod.lam <- lm(logabsmu ~ cdiff + cdiff2 + Code, data=df2)

df2$logabsse <- log(abs(df2$seval))
df2$logabsse[!is.finite(df2$logabsse)] <- NA
mod.las <- lm(logabsse ~ cdiff + cdiff2 + Code, data=df2)

library(stargazer)
stargazer(list(mod.lal, mod.lah, mod.lam, mod.las), style='ajps')

get.exppred <- function(mod, dfrow, ii, col) {
    tryCatch({
        exp(predict(mod, dfrow) + sample(mod$residuals, 1))
    }, error=function(e) {
        ## This can happen if all known values for this Code where the same
        vals <- df2[df2$Code == df2$Code[ii], col]
        if (all(vals[!is.na(vals)] == vals[!is.na(vals)][1])) {
            avg <- vals[!is.na(vals)][1]
            cdiff <- df2$cdiff[df2$Code == df2$Code[ii] & !is.na(vals)]
            cdiff.mean <- mean(cdiff)
            cdiff2.mean <- mean(cdiff^2)
            return(mod$coeff[2] * (df2$cdiff[ii] - cdiff.mean) + mod$coeff[3] * (df2$cdiff2[ii] - cdiff2.mean) + avg)
        }
        error("Unknown reason for mod failure.")
    })
}

alldraws <- matrix(NA, nrow(df2), 0)
results <- data.frame()
for (iter in 1:4000) {
    print(iter)

    df2$draw <- NA
    for (ii in 1:nrow(df2)) {
        ## Step 1: Impute as needed

        ## Bounds
        loval <- df2$loval[ii]
        hival <- df2$hival[ii]
        if (is.na(loval)) {
            if (knowns$bounds[knowns$Code == df2$Code[ii]]) {
                loval <- get.exppred(mod.lal, df2[ii,], ii, 'loval')
                hival <- get.exppred(mod.lah, df2[ii,], ii, 'loval')
                ## Apply a random sign from known
                if (sample(df2$loval[!is.na(df2$loval) & df2$Code == df2$Code[ii]], 1) < 0) {
                    loval <- -loval
                    hival <- -hival
                }
            } else {
                jj <- sample(which(!is.na(df2$loval) & df2$period == df2$period[ii] & df2$cdiff == df2$cdiff[ii]), 1)
                loval <- df2$loval[jj]
                hival <- df2$hival[jj]
            }

            if (hival < loval) {
                saved <- loval
                loval <- hival
                hival <- saved
            }
        }

        ## Mus and Ses
        muval <- df2$muval[ii]
        seval <- df2$seval[ii]
        if (is.na(muval) || is.na(seval)) {
            if (knowns$mus[knowns$Code == df2$Code[ii]]) {
                muval <- get.exppred(mod.lam, df2[ii,], ii, 'muval')
                ## Apply a random sign from known
                if (sample(df2$muval[!is.na(df2$muval) & df2$Code == df2$Code[ii]], 1) < 0)
                    muval <- -muval
            }
            if (knowns$ses[knowns$Code == df2$Code[ii]])
                seval <- get.exppred(mod.las, df2[ii,], ii, 'seval')
            if (is.na(muval) || is.na(seval)) {
                jj <- sample(which(!is.na(df2$muval) & !is.na(df2$seval) & df2$period == df2$period[ii] & df2$cdiff == df2$cdiff[ii]), 1)
                if (is.na(muval))
                    muval <- df2$muval[jj]
                if (is.na(seval))
                    seval <- df2$seval[jj]
            }
        }

        stopifnot(!is.na(loval) && !is.na(hival) && !is.na(muval) && !is.na(seval))

        ## Step 2: Combine with bounds
        while (is.na(df2$draw[ii]) || df2$draw[ii] > hival || df2$draw[ii] < loval) {
            if (runif(1) > .5)
                df2$draw[ii] <- rnorm(1, muval, seval)
            else
                df2$draw[ii] <- runif(1, loval, hival)
        }
    }

    results <- rbind(results, df2 %>% group_by(period, cdiff) %>% summarize(total=sum(draw)))
    alldraws <- cbind(alldraws, df2$draw)
}

growrate <- mean(diff(log(c(1314.245, 1329.186, 1350.140, 1382.128, 1418.235, 1455.828, 1495.583))))
gdp.2015.gbp <- 2089276e6 # GBP, from https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2
gdp.proj <- gdp.2015.gbp * exp(growrate * (c(2020, 2050, 2080) - 2015))

if (F) {
    ggplot(results, aes(cdiff, total)) +
        geom_point()

    results2 <- results %>% left_join(data.frame(period=c(2020, 2050, 2080), gdp.proj))
    results2$fracgdp <- results2$total / results2$gdp.proj

    ggplot(results2, aes(factor(cdiff), fracgdp)) +
        geom_boxplot() + theme_bw() + xlab("Global warming level (C)") +
        scale_y_continuous("Fraction of GDP-equivalent lost (%)", labels=scales::percent)
}

## Produce saved version
allfracs <- matrix(NA, nrow(alldraws), ncol(alldraws))
for (ii in 1:nrow(df2)) {
    period.gdp.proj <- gdp.proj[c(2020, 2050, 2080) == df2$period[ii]]
    allfracs[ii, ] <- alldraws[ii, ] / period.gdp.proj
}
df.allfracs <- df2[, -which(names(df2) == 'draw')]

save(df.allfracs, allfracs, file="ccradraws.RData")

source("~/research/missrisks/missing-risks/src/combine.R")

risks <- list()
for (ii in 1:nrow(df)) {
    letter <- function() {
        rows <- df2$Code == df$Code[ii]

        function(warming, numdraws) {
            myrows <- rows & (df2$cdiff == warming)
            stopifnot(sum(myrows) == 1)
            gdp.total <- list('1'=gdp.proj[1], '1.5'=gdp.proj[2], '2'=gdp.proj[3], '4'=gdp.proj[3])[[as.character(warming)]]
            alldraws[myrows, ] / gdp.total
        }
    }
    risks[[df$Code[ii]]] <- list(letter())
}

udraws <- get.udraws(CC, 4000)
finres <- combine.risks(risks, 4000, udraws, CC, 1 - DD, warmings=c(1, 1.5, 2, 4))
finres$mc <- rep(1:4000, nrow(finres) / 4000)

if (do.newonly) {
    write.csv(finres, "missrisk-news.csv", row.names=F)
} else {
    write.csv(finres, "missrisk-total.csv", row.names=F)
}

finres2 <- finres %>% group_by(temp) %>% summarize(mu=mean(affected), ci01=quantile(affected, .01), ci25=quantile(affected, .25),
                                                   ci50=median(affected), ci75=quantile(affected, .75), ci99=quantile(affected, .99))

ggplot(finres2, aes(temp, mu)) +
    geom_boxplot(aes(ymin=ci01, lower=ci25, middle=ci50, upper=ci75, ymax=ci99), stat='identity') +
    geom_point() +
    theme_bw() + xlab("Change in GMST from pre-industrial (°C)") +
    ylab("GDP loss (%)") + scale_y_continuous(labels=scales::percent)

finres %>% group_by(temp) %>% summarize(affected=sum(affected))
