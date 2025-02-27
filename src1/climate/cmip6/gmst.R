setwd("~/Dropbox/UK Economic Risks/climate/cmip6")
datapath <- "~/Dropbox/UK Economic Risks Data"

do.periods <- F

library(ncdf4)
library(abind)

lmst <- read.csv("../worldclim/lmst.csv")

results <- data.frame()
for (scenario in c('historical', 'ssp126', 'ssp370')) {
    if (scenario == 'historical')
        models <- unique(lmst$model[lmst$model != 'baseline'])
    else
        models <- unique(lmst$model[lmst$scenario == scenario])
    for (model in models) {
        print(c(scenario, model))
        filenames <- Sys.glob(file.path(datapath, "climate/cmip6", paste0("ts_Amon_", model, "_", scenario, "_*.nc")))
        if (length(filenames) == 1) {
            nc <- nc_open(filenames)
            time <- ncvar_get(nc, "time")
            tas <- ncvar_get(nc, "ts")
        } else if (length(filenames) == 2) {
            nc <- nc_open(filenames[1])
            time <- ncvar_get(nc, "time")
            tas <- ncvar_get(nc, "ts")
            nc <- nc_open(filenames[2])
            time <- c(time, ncvar_get(nc, "time"))
            tas <- abind(tas, ncvar_get(nc, "ts"), along=3)
        } else {
            print("Not found.")
            next
        }
        nc$dim$time$calendar
        nc$dim$time$units

        if (nc$dim$time$calendar %in% c("365_day", "noleap")) {
            if (nc$dim$time$units %in% c("days since 2015-01-01", "days since 2015-01-01 00:00:00"))
                year <- time / 365 + 2015
            else if (nc$dim$time$units %in% c("days since 1850-01-01", "days since 1850-1-1", "days since 1850-01-01 0:0:0.0", "days since 1850-01-01 00:00:00"))
                year <- time / 365 + 1850
            else {
                print("Unknown year.")
                next
            }
        } else if (nc$dim$time$calendar %in% c("gregorian", "proleptic_gregorian")) {
            if (nc$dim$time$units %in% c("days since 2015-01-01", "days since 2015-01-01 00:00:00"))
                year <- as.numeric(substring(as.Date("2015-01-01") + time, 1, 4))
            else if (nc$dim$time$units %in% c("days since 1850-01-01", "days since 1850-1-1", "days since 1850-01-01 0:0:0.0", "days since 1850-01-01 00:00:00"))
                year <- as.numeric(substring(as.Date("1850-01-01") + time, 1, 4))
            else {
                print("Unknown year.")
                next
            }
        } else {
            print("Unknown calendar.")
            next
        }

        if ('lat_bnds' %in% names(nc$var)) {
            gridlat <- ncvar_get(nc, 'lat_bnds')
            gridlon <- ncvar_get(nc, 'lon_bnds')
        } else {
            midlat <- ncvar_get(nc, 'lat')
            midlon <- ncvar_get(nc, 'lon')
            gridlat <- rbind(c(-90, (midlat[-1] + midlat[-length(midlat)]) / 2),
                             c((midlat[-1] + midlat[-length(midlat)]) / 2, 90))
            gridlon <- rbind(c(-90, (midlon[-1] + midlon[-length(midlon)]) / 2),
                             c((midlon[-1] + midlon[-length(midlon)]) / 2, 90))
        }
        gridsize <- tas[, , 1] * 0
        for (ii in 1:dim(gridsize)[1]) # lon
            for (jj in 1:dim(gridsize)[2]) # lat
                gridsize[ii, jj] <- .5 * (cos(gridlat[1, jj] * pi / 180) + cos(gridlat[2, jj] * pi / 180)) * diff(gridlon[, ii]) * diff(gridlat[, jj])

        if (do.periods) {
            if (scenario == 'historical') {
                periods.lo <- c(1850, 1850, 1970, 1995)
                periods.hi <- c(1880, 2000, 2000, 2015)
            } else {
                periods.lo <- c(2021, 2041, 2081)
                periods.hi <- c(2040, 2060, 2100)
            }

            for (ii in 1:length(periods.lo)) {
                subtas <- tas[, , year >= periods.lo[ii] & year < periods.hi[ii] + 1]
                gsat <- weighted.mean(subtas, array(gridsize, c(dim(gridsize), dim(subtas)[3])), na.rm=T) - 273.15
                results <- rbind(results, data.frame(period=paste0(periods.lo[ii], '-', periods.hi[ii]),
                                                     model, scenario, gsat))
            }
            print(tail(results))
        } else {
            for (yy in min(floor(year)):max(floor(year))) {
                if (sum(year >= yy & year < yy + 1) != 12)
                    next
                subtas <- tas[, , year >= yy & year < yy + 1]
                gsat <- weighted.mean(subtas, array(gridsize, c(dim(gridsize), dim(subtas)[3])), na.rm=T) - 273.15
                results <- rbind(results, data.frame(year=yy, model, scenario, gsat))
            }
        }
    }
}

if (do.periods) {
    write.csv(results, "gmst.csv", row.names=F)
} else {
    write.csv(results, "gmst-annual.csv", row.names=F)
}

