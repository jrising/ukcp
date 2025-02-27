setwd("~/Open Modeling Group Dropbox/UK Economic Risks/channels/agriculture")

library(ncdf4)

do.scenario <- 1
## 1: w/o TP, no irrig
## 3: w/ TP, no irrig

nc <- nc_open("Boulton/Temp_35K_A1B_CO2.nc") # Standard HadGEM3/HadRM3-PPE-UK ECS
lats <- nc$dim$lat_mean$vals
lons <- nc$dim$lon_mean$vals
year <- nc$dim$year$vals
temp <- ncvar_get(nc, 'Temp')

df.grid <- read.csv("Smith/data/Ag_model_grid_2km.v2.csv")
df.arable <- read.csv(paste0("Smith/data/Ag_model_sc", do.scenario, ".csv")) # w/ TP, no irrig.
## df.base <- read.csv("Ritchie et al ERL/ECO_AG_baseline.csv") # Sometimes very large discreptancy. Drop it!

## mean(df.base$arable_ha_6190)
## colMeans(df.arable, na.rm=T)

all(df.grid$new2kid == df.arable$new2kid)
## all(df.grid$new2kid == df.base$new2kid)

all(which(df.arable$arable_ha_2089 == 0) == which(df.arable$arable_ha_2000 == 0))

##test.log <- log(df.arable / df.base$arable_ha_6190)
test.log <- log(df.arable / df.arable$arable_ha_2000)
apply(test.log, 2, function(col) quantile(col, na.rm=T))

## df.base[which.max(df.base$arable_ha_6190 - df.arable$arable_ha_2000),]
## df.arable[which.max(df.base$arable_ha_6190 - df.arable$arable_ha_2000),]

## Aggregate to match final pixels
library(raster)
rr <- raster("../../climate/worldclim/uk-present/uk_bio.nc", band=1)

coords <- coordinates(rr)
coords.lon <- unique(coords[, 1])
coords.lat <- unique(coords[, 2])

df.grid$lonii <- NA
df.grid$latii <- NA
for (ii in 1:nrow(df.grid)) {
    if (ii %% 100 == 0)
        print(ii / nrow(df.grid))
    df.grid$lonii[ii] <- which.min(abs(df.grid$lon[ii] - coords.lon))
    df.grid$latii[ii] <- which.min(abs(df.grid$lat[ii] - coords.lat))
}

df.arable$lonii <- df.grid$lonii
df.arable$latii <- df.grid$latii
## df.base$lonii <- df.grid$lonii
## df.base$latii <- df.grid$latii

library(dplyr)
df.grid2 <- df.grid %>% group_by(lonii, latii) %>% summarize(Area_ha=sum(Area_ha), lon=mean(lon), lat=mean(lat))
## df.base2 <- df.base %>% group_by(lonii, latii) %>% summarize(arable_ha_6190=sum(arable_ha_6190))

df.grid2$templonii <- NA
df.grid2$templatii <- NA
for (ii in 1:nrow(df.grid2)) {
    df.grid2$templonii[ii] <- which.min(abs(df.grid2$lon[ii] - lons))
    df.grid2$templatii[ii] <- which.min(abs(df.grid2$lat[ii] - lats))
}

results <- list()
for (ii in 1:nrow(df.grid2)) {
    print(ii)
    temps <- temp[df.grid2$templonii[ii], df.grid2$templatii[ii], ] # 1998 - 2098
    if (all(is.nan(temps)))
        next

    basetemp <- mean(temps[1:11]) - 273.15
    ## basearable <- df.base2$arable_ha_6190[ii]

    arables <- as.numeric(colSums(df.arable[df.arable$lonii == df.grid2$lonii[ii] & df.arable$latii == df.grid2$latii[ii], c(-1, -ncol(df.arable), -ncol(df.arable)+1)])) # 2000 - 2089
    ## values <- data.frame(year=c(2003, 2000:2089),
    ##                      loss=c(0, log(arables / basearable)),
    ##                      cdiff=c(0, temps[3:(length(arables) + 2)] - 273.15 - basetemp))
    values <- data.frame(year=c(2003, 2000:2089),
                         loss=c(0, log(arables / mean(arables[1:11]))),
                         cdiff=c(0, temps[3:(length(arables) + 2)] - 273.15 - basetemp))
    ## plot(values$cdiff, values$loss)

    values$cdiff2 <- values$cdiff^2

    coeff <- matrix(NA, 0, 3)
    for (mm in 1:1000) {
        gaps <- runif(10)
        rows <- c(1, floor(nrow(values) * cumsum(gaps) / sum(gaps))[-10] + 1)
        mod <- lm(loss ~ cdiff + cdiff2, data=values[rows,])
        if (mod$coeff[2] < 0 && mod$coeff[3] < 0) {
            mod <- lm(loss ~ 0 + cdiff2, data=values[rows,]) # impose constraint that starts gradually
            mod$coeff <- c(0, 0, mod$coeff)
        }
        coeff <- rbind(coeff, mod$coeff)
    }
    results[[ii]] <- coeff
}

if (do.scenario != 3) {
    save(results, file=paste0("arable-coeffs-", do.scenario, ".RData"))
} else {
    save(results, file="arable-coeffs.RData")
}

## Take average curve from each grid cell

pdf <- data.frame()
for (ii in 1:nrow(df.grid2)) {
    if (ii > length(results) || is.null(results[[ii]]))
        next

    beta1 <- mean(results[[ii]][, 2])
    beta2 <- mean(results[[ii]][, 3])

    TT <- 0:6
    impact <- exp(beta1 * TT + beta2 * TT^2)

    base <- mean(as.numeric(colSums(df.arable[df.arable$lonii == df.grid2$lonii[ii] & df.arable$latii == df.grid2$latii[ii], 2:12])))

    pdf <- rbind(pdf, data.frame(lon=df.grid2$lon[ii], lat=df.grid2$lat[ii], TT, impact, base, area=df.grid2$Area_ha[ii]))
}

library(ggplot2)

pdf$frac <- pmin(1, pdf$impact * pdf$base / pdf$area)

ggplot(pdf) +
    geom_line(data=pdf %>% group_by(lat, lon) %>% summarize(x=c(lon, lon, lon + 1/6), y=c(lat + 1/6, lat, lat)),
              aes(x, y, group=paste(lat, lon)), size=.1) +
    geom_line(aes(lon+TT / 36, lat+frac / 6, group=paste(lat, lon)), colour='brown') +
    theme_bw() + xlab(NULL) + ylab(NULL)
