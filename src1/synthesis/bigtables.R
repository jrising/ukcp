setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)
library(reshape2)

load("synthesis/alldamage-uk.RData")
names(alldamage.uk)[names(alldamage.uk) == 'damage.cata'] <- 'fracgdp.cata'
load("synthesis/alldamage-reg.RData")
load("synthesis/alldamage-adm3.RData")

collabs <- list('fracgdp.slr.cge'='Coastal impacts', 'fracgdp.mort'='Health', 'fracgdp.labor'='Labour productivity',
                'fracgdp.crop'='Agriculture', 'fracgdp.hydr'='Droughts and flooding', 'fracgdp.esad'='Energy supply & demand',
                'fracgdp.live'='Livestock & fisheries', 'fracgdp.ecos'='Ecosystems')
## 'fracgdp.trad'='Trade effects', 'missing'='Missing non-catastrophic', 'fracgdp.cata'='Catastrophic risk',
get.range <- function(xx) {
    if (all(xx == 0))
        'NA'
    else
        paste0('[', paste(paste0(round(100 * quantile(xx, c(.05, .95), na.rm=T), 2), '%'), collapse=' - '), ']')
}

get.tosave <- function(alldamage.uk, alldamage.reg, alldamage.adm3, col) {
    alldamage.uk$value <- alldamage.uk[, col, drop=T]
    tosave <- alldamage.uk %>% group_by(period, scenario) %>%
        summarize(mu=mean(value, na.rm=T), rng=get.range(value))
    tosave.mu.uk <- dcast(tosave, 'UK' ~ scenario + period, value.var='mu')
    tosave.rng.uk <- dcast(tosave, 'UK' ~ scenario + period, value.var='rng')

    alldamage.reg$value <- alldamage.reg[, col, drop=T]
    alldamage.reg$nutsreg <- factor(alldamage.reg$nutsreg,
                                    levels=c("Northern Ireland", "Scotland", "Wales", "North West (England)",
                                             "North East (England)", "Yorkshire And The Humber",
                                             "West Midlands (England)", "East Midlands (England)",
                                             "South West (England)", "South East (England)",
                                             "East Of England", "London"))
    tosave <- alldamage.reg %>% group_by(period, scenario, nutsreg) %>%
        summarize(mu=mean(value, na.rm=T), rng=get.range(value))
    tosave.mu.reg <- dcast(tosave, nutsreg ~ scenario + period, value.var='mu')
    tosave.rng.reg <- dcast(tosave, nutsreg ~ scenario + period, value.var='rng')

    alldamage.adm3$value <- alldamage.adm3[, col, drop=T]
    tosave <- alldamage.adm3 %>% group_by(period, scenario, county) %>%
        summarize(mu=mean(value, na.rm=T), rng=get.range(value))
    tosave.mu.adm3 <- dcast(tosave, county ~ scenario + period, value.var='mu')
    tosave.rng.adm3 <- dcast(tosave, county ~ scenario + period, value.var='rng')

    tosave.all <- rbind(data.frame(Region=tosave.mu.uk[, 1],
                                   `ssp370_2011-2030`=tosave.mu.uk[, 5], `ssp370_2011-2030_rng`=tosave.rng.uk[, 5],
                                   `ssp370_2041-2060`=tosave.mu.uk[, 6], `ssp370_2041-2060_rng`=tosave.rng.uk[, 6],
                                   `ssp370_2081-2100`=tosave.mu.uk[, 7], `ssp370_2081-2100_rng`=tosave.rng.uk[, 7],
                                   `ssp126_2011-2030`=tosave.mu.uk[, 2], `ssp126_2011-2030_rng`=tosave.rng.uk[, 2],
                                   `ssp126_2041-2060`=tosave.mu.uk[, 3], `ssp126_2041-2060_rng`=tosave.rng.uk[, 3],
                                   `ssp126_2081-2100`=tosave.mu.uk[, 4], `ssp126_2081-2100_rng`=tosave.rng.uk[, 4]),
                        data.frame(Region=tosave.mu.reg[, 1],
                                   `ssp370_2011-2030`=tosave.mu.reg[, 5], `ssp370_2011-2030_rng`=tosave.rng.reg[, 5],
                                   `ssp370_2041-2060`=tosave.mu.reg[, 6], `ssp370_2041-2060_rng`=tosave.rng.reg[, 6],
                                   `ssp370_2081-2100`=tosave.mu.reg[, 7], `ssp370_2081-2100_rng`=tosave.rng.reg[, 7],
                                   `ssp126_2011-2030`=tosave.mu.reg[, 2], `ssp126_2011-2030_rng`=tosave.rng.reg[, 2],
                                   `ssp126_2041-2060`=tosave.mu.reg[, 3], `ssp126_2041-2060_rng`=tosave.rng.reg[, 3],
                                   `ssp126_2081-2100`=tosave.mu.reg[, 4], `ssp126_2081-2100_rng`=tosave.rng.reg[, 4]),
                        data.frame(Region=tosave.mu.adm3[, 1],
                                   `ssp370_2011-2030`=tosave.mu.adm3[, 5], `ssp370_2011-2030_rng`=tosave.rng.adm3[, 5],
                                   `ssp370_2041-2060`=tosave.mu.adm3[, 6], `ssp370_2041-2060_rng`=tosave.rng.adm3[, 6],
                                   `ssp370_2081-2100`=tosave.mu.adm3[, 7], `ssp370_2081-2100_rng`=tosave.rng.adm3[, 7],
                                   `ssp126_2011-2030`=tosave.mu.adm3[, 2], `ssp126_2011-2030_rng`=tosave.rng.adm3[, 2],
                                   `ssp126_2041-2060`=tosave.mu.adm3[, 3], `ssp126_2041-2060_rng`=tosave.rng.adm3[, 3],
                                   `ssp126_2081-2100`=tosave.mu.adm3[, 4], `ssp126_2081-2100_rng`=tosave.rng.adm3[, 4]))
    tosave.all
}

for (col in names(collabs)) {
    tosave.all <- get.tosave(alldamage.uk, alldamage.reg, alldamage.adm3, col)
    write.csv(tosave.all, paste0("synthesis/output-bigtables/", collabs[[col]], ".csv"), row.names=F)
}

load("synthesis/alldamage-uk-dblcnt.RData")
names(alldamage.uk.dblcnt)[names(alldamage.uk.dblcnt) == 'damage.cata'] <- 'fracgdp.cata'
load("synthesis/alldamage-reg-dblcnt.RData")
load("synthesis/alldamage-adm3-dblcnt.RData")

tosave.all <- get.tosave(alldamage.uk.dblcnt, alldamage.reg.dblcnt, alldamage.adm3.dblcnt, 'total')
write.csv(tosave.all, paste0("synthesis/output-bigtables/Total costs.csv"), row.names=F)
