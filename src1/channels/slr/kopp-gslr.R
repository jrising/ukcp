## Reports height relative to 2000
## GSATs should be relative to 1850-2000 average

source("n95mc.R")

use.fit <- NA
use.pointest <- F

outsuffix <- paste0(ifelse(!is.na(use.fit), paste0("-", use.fit), ""), ifelse(use.pointest, "-pe", ""))


c.2000.twoskew <- c(fit.skewnormal(0.14, 0.05, 0.29), fit.skewnormal(0.03, 0.01, 0.06))
T0.2000.twoskew <- c(fit.skewnormal(-0.05, -0.12, 0.07), fit.skewnormal(0.04, -0.10, 0.16))
aa.twoskew <- c(fit.skewnormal(4.0, 3.2, 5.4), fit.skewnormal(4.7, 3.4, 7.0))
tau1.twoskew <- c(fit.skewnormal(174, 87, 366), fit.skewnormal(102, 64, 203))
tau2.twoskew <- c(100, 100, 1, 100, 100, 1) * c(fit.skewnormal(4175 / 100, 1140 / 100, 17670 / 100), fit.skewnormal(3392 / 100, 1124 / 100, 16155 / 100))

df <- read.csv("~/projects/ukcp/gsat/ssps_26_70_ukproject_allmembers.csv")

allresult <- data.frame()
for (scenario in unique(df$scenario)) {
    for (model in unique(df$model[df$scenario == scenario])) {
        for (runid in unique(df$run_id[df$scenario == scenario & df$model == model])) {
            print(c(scenario, model, runid))
            subdf <- df[df$scenario == scenario & df$model == model & df$run_id == runid & df$year >= 2000,]

            ## Correct from relative to pre-industrial to relative to 1850-2000
            ## Choose a random HadCRUT ensemble member
            gmst.hist <- read.table(paste0("~/projects/ukcp/gsat/HadCRUT.4.6.0.0.annual_ns_avg_realisations/HadCRUT.4.6.0.0.annual_ns_avg.", sample(1:100, 1), ".txt"))
            gmst.1850.2000 <- mean(gmst.hist$V2[gmst.hist$V1 >= 1850 & gmst.hist$V1 <= 2000]) # relative to 1961-1990
            gmst.1970.1999 <- mean(gmst.hist$V2[gmst.hist$V1 >= 1970 & gmst.hist$V1 <= 1999]) # relative to 1961-1990
            gsat.1970.1999 <- mean(df$value[df$scenario == scenario & df$model == model & df$run_id == runid & df$year >= 1970 & df$year <= 1999])

            ## GSAT - GSAT_1850-2000 ~= ( (GSAT - GSAT_1850) - (GSAT_1970-2000 - GSAT_1850) ) - ( (GMST_1850-2000 - GMST_1961-1990) - (GMST_1970-1999 - GMST_1961-1990) )
            subdf$gsat <- subdf$value - gsat.1970.1999 - (gmst.1850.2000 - gmst.1970.1999)

            fit <- sample(1:2, 1)

            c.2000 <- r.twoskew(fit, c.2000.twoskew) # mm/yr
            h.2000 <- 0 # mm
            T0.2000 <- r.twoskew(fit, T0.2000.twoskew) # K

            aa <- r.twoskew(fit, aa.twoskew) # mm/yr/K
            tau1 <- r.twoskew(fit, tau1.twoskew) # yr
            tau2 <- r.twoskew(fit, tau2.twoskew) # yr

            result <- data.frame(year=2000, gslr=h.2000, T0=T0.2000, cc=c.2000)
            for (ii in 1:nrow(subdf)) {
                dhdt <- aa * (subdf$gsat[ii] - result$T0[ii]) + result$cc[ii]
                dT0dt <- (subdf$gsat[ii] - result$T0[ii]) / tau1
                dcdt <- -result$cc[ii] /tau2

                result <- rbind(result, data.frame(year=subdf$year[ii] + 1, gslr=result$gslr[ii] + dhdt,
                                                   T0=result$T0[ii] + dT0dt, cc=result$cc[ii] + dcdt))
            }

            allresult <- rbind(allresult, data.frame(scenario, model, runid, year=subdf$year, gsat=subdf$value, gslr=result$gslr[-nrow(result)]))
        }
    }
}


write.csv(allresult, paste0("~/projects/ukcp/slr/gsatgslr", outsuffix, ".csv"), row.names=F)
allresult <- read.csv(paste0("~/projects/ukcp/slr/gsatgslr", outsuffix, ".csv"))

## Select bootstrap of MCs around quantiles
library(dplyr)

allresult$modelrunid <- paste(allresult$scenario, allresult$model, allresult$runid)

quantres <- data.frame()
for (scenario in unique(allresult$scenario)) {
    allresult.end <- allresult[allresult$scenario == scenario & allresult$year == max(allresult$year), ]

    for (qq in c(.05, .5, .95)) {
        bootstraps <- data.frame()
        for (rr in 1:1000) {
            rows <- sample(1:nrow(allresult.end), nrow(allresult.end), replace=T)
            chosen <- which(rank(allresult.end$gslr[rows], ties.method="random") == round(qq * nrow(allresult.end)))
            point <- quantile(allresult.end$gslr, c(.05, .5, .95))
            chosenmc <- subset(allresult, modelrunid == allresult.end$modelrunid[rows[chosen]])
            bootstraps <- rbind(bootstraps, chosenmc)
        }
        combo <- bootstraps %>% group_by(year) %>% summarize(gsat.mu=mean(gsat), gsat.se=sd(gsat), gslr.mu=mean(gslr), gslr.se=sd(gslr))
        combo$quantile <- qq
        quantres <- rbind(quantres, cbind(scenario, combo))
    }
}

write.csv(quantres, paste0("~/projects/ukcp/slr/gslrquants", outsuffix, ".csv"), row.names=F)

library(ggplot2)

pdf <- quantres
pdf$quantlabel <- paste0(100 * pdf$quantile, "%")

gp <- ggplot(pdf, aes(year, gslr.mu, group=quantlabel, colour=quantlabel)) +
    facet_wrap(~ scenario, ncol=1, scales="free_y") +
    geom_line() + geom_ribbon(aes(ymin=gslr.mu - 1.96*gslr.se, ymax=gslr.mu + 1.96*gslr.se), alpha=.5) +
    theme_bw() + xlab(NULL) + ylab("SLR relative to 2000") + scale_colour_discrete("Quantile")
ggsave(paste0("~/projects/ukcp/slr/gslr", outsuffix, ".pdf"), gp, width=5, height=6)
