setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

do.group <- 'local'

df <- read.csv("channels/biosphere/results.csv") # Units are 1000 GBP 2020 per year

median(diff(df$longitude)[diff(df$longitude) > 0])
median(diff(df$latitude)[diff(df$latitude) > 0])
min(diff(df$longitude)[diff(df$longitude) > 0])

plot(df$longitude, df$latitude)

## If biodiversity increases, only count carbon sequestration
cc.ratio <- mean(c(160 / 641, 581 / 2320)) # both almost exactly 0.25.
df$value_change_global[df$value_change_global > 0] <- df$value_change_global[df$value_change_global > 0] * cc.ratio
df$value_change_global_hightree[df$value_change_global_hightree > 0] <- df$value_change_global_hightree[df$value_change_global_hightree > 0] * cc.ratio
df$value_change_global_lowtree[df$value_change_global_lowtree > 0] <- df$value_change_global_lowtree[df$value_change_global_lowtree > 0] * cc.ratio
df$value_change_total <- df$value_change_global + df$value_change_local
df$value_change_total_hightree <- df$value_change_global_hightree + df$value_change_local_hightree
df$value_change_total_lowtree <- df$value_change_global_lowtree + df$value_change_local_lowtree

library(ncdf4)

nc <- nc_open("channels/agriculture/Boulton/Temp_35K_A1B_CO2.nc") # Standard HadGEM3/HadRM3-PPE-UK ECS
lats <- nc$dim$lat_mean$vals
lons <- nc$dim$lon_mean$vals
year <- nc$dim$year$vals
temp <- ncvar_get(nc, 'Temp')

temp.baseline <- apply(temp[,, year <= 2008], c(1, 2), mean)
## temp.future <- apply(temp[,, year >= 2088], c(1, 2), mean)

library(raster)

rr.rcp85 <- raster("channels/biosphere/wc2.1_10m_bioc_HadGEM3-GC31-LL_ssp585_2081-2100.tif", band=1)
rr.rcp852 <- crop(rr.rcp85, extent(-10.716305, 2.159672, 49.731803, 61.021214))

source("lib/damagefunc.R")

library(raster)
rr <- raster("climate/worldclim/uk-present/uk_bio.nc", band=1)

coords <- coordinates(rr)
coords.lon <- unique(coords[, 1])
coords.lat <- unique(coords[, 2])

df$lonii <- NA
df$latii <- NA
df$templonii <- NA
df$templatii <- NA
for (ii in 1:nrow(df)) {
    if (ii %% 100 == 0)
        print(ii / nrow(df))
    df$lonii[ii] <- which.min(abs(df$longitude[ii] - coords.lon))
    df$latii[ii] <- which.min(abs(df$latitude[ii] - coords.lat))
    df$templonii[ii] <- which.min(abs(df$longitude[ii] - lons))
    df$templatii[ii] <- which.min(abs(df$latitude[ii] - lats))
}

cells <- unique(df[, c('lonii', 'latii')])

results <- list()
for (ii in 1:nrow(cells)) {
    print(ii / nrow(cells))
    subdf <- subset(df, lonii == cells$lonii[ii] & latii == cells$latii[ii])

    temp.future <- rr.rcp852[cells$latii[ii], cells$lonii[ii]] + 273.15
    cpost <- sapply(1:nrow(subdf), function(jj) temp.future - temp.baseline[subdf$templonii[jj], subdf$templatii[jj]])
    valid <- !is.na(cpost) & !is.na(subdf$value_change_total)

    if (sum(valid) == 0)
        next

    if (do.group == 'total') {
        ddiff <- sum(subdf$value_change_total[valid])
        ddiff.q975 <- sum(subdf$value_change_total_hightree[valid] - subdf$value_change_total[valid])
        ddiff.q025 <- sum(subdf$value_change_total_lowtree[valid] - subdf$value_change_total[valid])
    } else if (do.group == 'local') {
        ddiff <- sum(subdf$value_change_local[valid])
        ddiff.q975 <- sum(subdf$value_change_local_hightree[valid] - subdf$value_change_local[valid])
        ddiff.q025 <- sum(subdf$value_change_local_lowtree[valid] - subdf$value_change_local[valid])
    } else {
        print("Unknownn group")
    }

    ## Construct all MC draws
    qq <- runif(1000)
    soln <- optimize(function(qmu) {
        dd <- rep(ddiff, 1000)
        dd[qq < qmu] <- (ddiff - ddiff.q025) * (qq[qq < qmu] - .025) / (qmu - .025) + ddiff.q025
        dd[qq > qmu] <- (ddiff.q975 - ddiff) * (qq[qq > qmu] - qmu) / (.975 - qmu) + ddiff
        dd[qq <= .025] <- ddiff.q025
        dd[qq >= .975] <- ddiff.q975
        (mean(dd) - ddiff)^2
    }, c(.025, .975))

    qmu <- soln$minimum
    dd <- rep(ddiff, 1000)
    dd[qq < qmu] <- (ddiff - ddiff.q025) * (qq[qq < qmu] - .025) / (qmu - .025) + ddiff.q025
    dd[qq > qmu] <- (ddiff.q975 - ddiff) * (qq[qq > qmu] - qmu) / (.975 - qmu) + ddiff
    dd[qq <= .025] <- ddiff.q025
    dd[qq >= .975] <- ddiff.q975

    cdiffs <- c(0, mean(cpost[valid]))

    la <- fit.damages(cdiffs, NULL, NULL, draws=1000, force.spec='linols', impact.draws=cbind(rep(0, 1000), dd))
    results[[ii]] <- la
}

save(cells, results, file=paste0("channels/biosphere/forest-coeffs-", do.group, ".RData"))

pdf <- data.frame()
for (ii in 1:nrow(cells)) {
    if (ii > length(results) || is.null(results[[ii]]))
        next

    beta1 <- mean(results[[ii]]$coeff[, 2])

    TT <- 0:6
    impact <- beta1 * TT

    pdf <- rbind(pdf, data.frame(lon=coords.lon[cells$lonii[ii]], lat=coords.lat[cells$latii[ii]], TT, impact))
}

library(ggplot2)
library(dplyr)

pdf$logimpact <- log(pdf$impact)
pdf$logimpact[pdf$impact < 1] <- 0

yscale <- max(pdf$logimpact)
ggplot(pdf) +
    geom_line(data=pdf %>% group_by(lat, lon) %>% summarize(x=c(lon, lon, lon + 1/6) - 1/12, y=c(lat + 1/6, lat, lat) - 1/12), aes(x, y, group=paste(lat, lon)), size=.1) +
    geom_line(aes(lon - 1/12 + TT / 36, lat - 1/12 + logimpact / yscale / 6, group=paste(lat, lon)), colour='brown') +
    theme_bw() + xlab(NULL) + ylab(NULL)
ggsave(paste0("channels/biosphere/forest-dfs-", do.group, ".pdf"), width=4, height=6)

## Project it!

source("lib/aggregate.R")

cells$havecoeff <- F
for (ii in 1:nrow(cells)) {
    if (ii > length(results) || is.null(results[[ii]]))
        next
    cells$havecoeff[ii] <- T
}

validrrdf <- rrdf[!is.na(rrdf$variable),]

cells$lon <- coords.lon[cells$lonii]
cells$lat <- coords.lat[cells$latii]

grid2region <- rep(NA, nrow(validrrdf))
for (ii in 1:nrow(validrrdf)) {
    dists <- (cells$lon - validrrdf$x[ii])^2 + (cells$lat - validrrdf$y[ii])^2
    dists[!cells$havecoeff] <- Inf
    grid2region[ii] <- which.min(dists)
}

## Use strings for region names
results2 <- list()
for (ii in 1:length(results)) {
    if (!is.null(results[[ii]]))
        results2[[as.character(ii)]] <- results[[ii]]
}
grid2region <- as.character(grid2region)

source("lib/project.R")

## projs.test <- grid.project(results2, zero.y0=year[1], zero.y1=year[12], grid2region=grid2region, run_id.limit=100)
projs <- grid.project(results2, zero.y0=year[1], zero.y1=year[12], grid2region=grid2region)

save(projs, file=paste0("channels/biosphere/forest-projs-", do.group, ".RData"))
## load("channels/biosphere/forest-projs-local.RData")

projs %>% group_by(scenario, period) %>% summarize(cdiff=mean(cdiff), damage=mean(damage))

## Report for UK

library(dplyr)
projs.uk <- projs %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage * 1000 * 0.7798 / 1e6, na.rm=T)) # mgbp

source("lib/report.R")

print(xtable(make.table(projs.uk, 'damage'), digits=1), include.rownames=F)

source("lib/constants.R")
projs.uk$percent <- -100 * projs.uk$damage * 1e6 / gdp.2015.gbp
print(xtable(make.table(projs.uk, 'percent'), digits=3), include.rownames=F)

## Aggregate to ADM3 level

source("lib/aggregate.R")

byadm3 <- aggregate.gridded.county(projs, 'sum')
byadm3$tot <- as.numeric(byadm3$tot)

formod <- projs.uk %>% group_by(scenario, period) %>% summarize(damage=mean(damage)) %>% left_join(byadm3 %>% group_by(scenario, run_id, period) %>% summarize(tot=sum(tot * 1000 * 0.7798 / 1e6, na.rm=T)) %>% group_by(scenario, period) %>% summarize(tot=mean(tot)))
mod <- lm(damage ~ 0 + tot, data=formod)

byadm3$damage <- byadm3$tot * mod$coeff[1]

save(byadm3, file="channels/biosphere/forest-projs-adm3.RData")

shp.adm3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

byadm3.map <- byadm3 %>% left_join(shp.adm3.polydata, by=c('county'='NAME_3'))

byadm32 <- make.maps.loop("channels/biosphere/maps", "jules", byadm3.map, shp.adm3, function(subres) {
    subres %>% group_by(PID) %>% summarize(mu=mean(damage, na.rm=T), ci5=quantile(damage, .05, na.rm=T), ci95=quantile(damage, .95, na.rm=T))
}, "Expected\nvalue\nincrease (£ million)", "95% CI\nvalue\nincrease (£ million)", 0, 25000, locol='white', hicol=muted('green'))

## Compute global as total - local
if (do.group == 'local') {
    projs.uk.local <- projs.uk
    load("channels/biosphere/forest-projs.RData")
    projs.uk.total <- projs %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage * 1000 * 0.7798 / 1e6, na.rm=T)) # mgbp

    all(paste(projs.uk.local$scenario, projs.uk.local$run_id, projs.uk.local$period) == paste(projs.uk.total$scenario, projs.uk.total$run_id, projs.uk.total$period))

    projs.uk.globe <- projs.uk.total
    projs.uk.globe$damage <- projs.uk.total$damage - projs.uk.local$damage

    print(xtable(make.table(projs.uk.globe, 'damage'), digits=1), include.rownames=F)

    projs.uk.globe$percent <- -100 * projs.uk.globe$damage * 1e6 / gdp.2015.gbp
    print(xtable(make.table(projs.uk.globe, 'percent'), digits=3), include.rownames=F)
}
