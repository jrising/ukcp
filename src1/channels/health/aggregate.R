setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

do.redo.uk <- F

## 0. Load gridded results for patterns
gridded <- read.csv("channels/health/Bressleretal-project.csv")
names(gridded)[1:3] <- c('scenario', 'run_id', 'pattern')

if (F) {
    library(ggplot2)
    library(dplry)

    ggplot(subset(gridded, scenario == 'ssp370' & period == '2081-2100') %>% group_by(x, y) %>% summarize(damage=mean(damage)),
           aes(x, y, fill=damage)) + geom_raster()
}

library(dplyr)

gridpatts <- gridded %>% group_by(scenario, run_id) %>% summarize(check=all(pattern == pattern[1]), pattern=pattern[1])
all(gridpatts$check)

## 1. Determine UK-level effects
source("lib/aggregate.R")

if (do.redo.uk) {
    source("lib/project.R")

    agginfo0 <- get.standard.agginfo(0)

    load("channels/health/Bressler et al la.RData")

    results <- grid.project(la, 1970, 2001, aggregate.cdiff=function(cdiffs) {
        data.frame(x=mean(cdiffs$x), y=mean(cdiffs$y), variable=county.aggregate(cdiffs$variable, agginfo0))
    }, gridpatts=gridpatts)

    write.csv(results, "channels/health/Bressleretal-project-uk.csv", row.names=F)
} else {
    results <- read.csv("channels/health/Bressleretal-project-uk.csv")
}

## Create the master table

mortrate <- 9 / 1000

source("lib/report.R")

results$deathrate <- 100000 * mortrate * subres$damage / 100
tbl <- make.table(results, 'deathrate')

library(xtable)
print(xtable(tbl, digits=1), include.rownames=F)

## 2. For each MC, ensure that values match

pops2.df <- get.gridded.pop()

## totpop <- sum(pops2.df$w001001[found$EID], na.rm=T)
source("lib/constants.R")
totpop <- pop.2015

## For matching

gridded$x.2 <- round(gridded$x, 2)
gridded$y.2 <- round(gridded$y, 2)

ratios <- c()
gridded$deaths <- NA
for (scn in unique(results$scenario)) {
    for (rid in unique(results$run_id[results$scenario == scn])) {
        for (per in unique(results$period)) {
            print(c(scn, rid, per))
            ## Ensure that sum(gridded_deaths) = uk_deaths
            rows <- which(gridded$scenario == scn & gridded$run_id == rid & gridded$period == per)
            subgrid <- gridded[rows,]
            uklevel <- subset(results, scenario == scn & run_id == rid & period == per)
            ukdeaths <- totpop * mortrate * uklevel$damage / 100

            subgrid2 <- subgrid %>% left_join(pops2.df, by=c('x.2'='X.2', 'y.2'='Y.2'))
            ## sum(subgrid2$w001001[subgrid2$is.uk], na.rm=T) == totpop
            griddeaths <- subgrid2$w001001[subgrid2$is.uk] * mortrate * subgrid2$damage[subgrid2$is.uk] / 100
            if (sign(sum(griddeaths)) != sign(ukdeaths)) {
                extradeaths <- ukdeaths - sum(griddeaths)

                gridded$deaths[rows[subgrid2$is.uk]] <- griddeaths + subgrid2$w001001[subgrid2$is.uk] * extradeaths / sum(subgrid2$w001001[subgrid2$is.uk])
            } else {
                ratio <- ukdeaths / sum(griddeaths)
                ratios <- c(ratios, ratio)

                gridded$deaths[rows[subgrid2$is.uk]] <- griddeaths * ratio
            }

            if (F) {
                ggplot(subset(gridded, scenario == scn & period == per & run_id == rid), aes(x, y, fill=deaths)) + geom_raster()
            }
        }
    }
}

save(gridded, file="channels/health/Bressleretal-gridded-final.RData")

## Aggregate

final <- gridded %>% group_by(scenario, period, x, y) %>% summarize(mu=mean(deaths, na.rm=T), ci5=quantile(deaths, .05, na.rm=T), ci95=quantile(deaths, .95, na.rm=T))

write.csv(final, "channels/health/Bressleretal-final.csv", row.names=F)

## Maps

source("lib/aggregate.R")
source("lib/report.R")

library(PBSmapping)
library(ggplot2)
library(scales)

shp1 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_1.shp")
shp <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
polydata <- attr(shp, 'PolyData')

loval <- -10 #quantile(final$mu, .1, na.rm=T)
hival <- 10 #quantile(final$ci95, .9, na.rm=T)

byadm3 <- make.maps.loop("channels/health/figures", "bressler", final, shp, function(subfin) {
    subfin$x.2 <- round(subfin$x, 2)
    subfin$y.2 <- round(subfin$y, 2)

    allcells <- data.frame(x.2=round(rrdf$x, 2), y.2=round(rrdf$y, 2))
    allcells2 <- allcells %>% left_join(subfin)

    byadm3rows <- data.frame(PID=1:nrow(polydata))
    for (col in c('mu', 'ci5', 'ci95')) {
        adm3vals <- county.aggregate(allcells2[, col])
        byadm3rows[, col] <- adm3vals
    }

    byadm3rows
}, "Expected\ndeaths\nper year", "95% CI\ndeaths\nper year", loval, hival)

write.csv(byadm3, "channels/health/Bressleretal-byadm3.csv", row.names=F)

## Also generate MC version of ADM3

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")
library(dplyr)

source("lib/aggregate.R")
load("channels/health/Bressleretal-gridded-final.RData")

## ggplot(subset(gridded, scenario == 'ssp370' & period == '2081-2100') %>%
##        left_join(pops2.df, by=c('x.2'='X.2', 'y.2'='Y.2')) %>% group_by(x, y) %>% summarize(deathrate=mean(deaths / w001001)),
##        aes(x, y, fill=deathrate)) + geom_raster()

byadm3 <- aggregate.gridded.county(gridded, 'mean', round.digits=2)
byadm3$avg <- as.numeric(byadm3$avg)
byadm3$run_id <- as.numeric(byadm3$run_id)

shp.adm3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.shp")
adm3info <- attr(shp.adm3, 'PolyData')
byadm32 <- byadm3 %>% left_join(adm3info, by=c('county'='NAME_3'))

results.uk <- read.csv("channels/health/Bressleretal-project-uk.csv")

byadm3.touk <- byadm32 %>% group_by(scenario, run_id, period) %>% summarize(avg=sum(avg * popsum) / sum(popsum)) %>% left_join(results.uk, by=c('scenario', 'run_id', 'period'), suffix=c('', '.uk'))
mod <- lm(damage ~ 0 + avg, data=byadm3.touk)

byadm32$damage.scaled <- byadm32$avg * mod$coeff[1]

byadm33 <- byadm32 %>% left_join(results.uk, by=c('scenario', 'run_id', 'period'), suffix=c('', '.uk'))
byadm3.matched <- byadm33 %>% group_by(scenario, run_id, period) %>% summarize(county=county, damage=force.match.vector(damage.scaled, damage[1], weight=popsum / sum(popsum)))

save(byadm3.matched, file="channels/health/Bressleretal-adm3-final.RData")
