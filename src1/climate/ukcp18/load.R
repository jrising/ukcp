library(ncdf4)

nc <- nc_open("climate/ukcp18/tas_rcp85_land-gcm_uk_region_07_ann_189912-209911.nc")
tas <- ncvar_get(nc, "tas")
clim.regions <- gsub(" +$", "", ncvar_get(nc, "geo_region"))
year <- ncvar_get(nc, "year")
nc_close(nc)

preind <- rowMeans(tas[, 1:30])
yr2001.2010 <- rowMeans(tas[, year %in% 2001:2010])
sc2C <- rowMeans(tas[, year %in% (2031 - 5):(2031 + 5)])
sc4C <- rowMeans(tas[, year %in% (2064 - 5):(2064 + 5)])

source("lib/project.R")

## Uncorrected temperatures
get.ukcp18.temps <- function(pattern, scenario, filedur) {
    if (filedur == '1970-2000')
        cachekey <- paste("ukcp18", filedur)
    else
        cachekey <- paste("ukcp18", pattern, scenario, filedur)
    if (cachekey %in% names(filecache))
        return(filecache[[cachekey]])

    if (filedur == '1970-2000')
        df <- read.csv(file.path("climate/ukcp18/worldclim/uk-present", "uk_bio.csv"))
    else
        df <- read.csv(file.path("climate/ukcp18/worldclim/uk-future", paste0("uk_bioc_", pattern, "_", scenario, "_", filedur, ".csv")))

    result <- list()
    for (ii in 1:nrow(df))
        result[[df$geo_region[ii]]] <- df$variable0[ii]
    filecache[[cachekey]] <<- result

    result
}

ukcp18.pattern.cdiff <- function(pattern, scenario, year) {
    shares <- get.shares(year)

    cdiff.global.0 <- gmst$gsat[gmst$scenario == 'historical' & gmst$model == pattern & gmst$period == '1970-2000']
    cdiff.0 <- get.ukcp18.temps(pattern, scenario, '1970-2000')

    ## Construct regional temperatures
    temps <- NULL
    for (filedur in names(shares)) {
        thistemps <- get.ukcp18.temps(pattern, scenario, filedur)
        ## Subtract GMST for this model-period
        if (filedur == '1970-2000') {
            cdiff.global <- gmst$gsat[gmst$scenario == 'historical' & gmst$model == pattern & gmst$period == filedur]
        } else
            cdiff.global <- gmst$gsat[gmst$scenario == scenario & gmst$model == pattern & gmst$period == filedur]

        ## Adjust to pattern warmings
        for (region in names(thistemps))
            thistemps[[region]] <- (thistemps[[region]] - cdiff.0[[region]]) - (cdiff.global - cdiff.global.0)

        if (is.null(temps))
            temps <- lapply(thistemps, function(x) x * shares[[filedur]])
        else {
            for (region in names(thistemps))
                temps[[region]] <- temps[[region]] + thistemps[[region]] * shares[[filedur]]
        }
    }

    temps
}

ukcp18.project <- function(allla, transform.cdiff=function(x) x) {
    results <- data.frame()
    for (scenario in unique(alldraws$scenario)) {
        for (run_id in unique(alldraws$run_id[alldraws$scenario == scenario])) {
            print(c(scenario, run_id))

            warming <- get.warming.eoc(scenario, run_id)
            ## pattern <- get.pattern(scenario, warming)
            pattern <- common.patterns$pattern[common.patterns$run_id == run_id]
            warming.1900.1929 <- mean(alldraws$value[alldraws$scenario == scenario & alldraws$run_id == run_id & alldraws$year >= 1900 & alldraws$year < 1930])

            draw <- sample.int(dim(allla[[names(allla)[1]]]$coeff)[1], 1)

            for (period in names(periods)) {
                subres <- data.frame()
                for (year in periods[[period]][1]:periods[[period]][2]) {
                    warming.now <- alldraws$value[alldraws$scenario == scenario & alldraws$run_id == run_id & alldraws$year == year]

                    cdiffs <- ukcp18.pattern.cdiff(pattern, scenario, year)
                    cdiffs <- transform.cdiff(cdiffs)
                    for (region in names(cdiffs)) {
                        if (!(region %in% names(allla)))
                            next
                        fullcdiff <- (warming.now - warming.1900.1929) + cdiffs[[region]]

                        coeffs <- allla[[region]]$coeff[draw, ]
                        damage <- coeffs[2] * fullcdiff + coeffs[3] * fullcdiff^2
                        subres <- rbind(subres, data.frame(year, region, damage))
                    }
                }

                resrow <- subres %>% group_by(region) %>% summarize(damage=mean(damage))
                results <- rbind(results, cbind(scenario=scenario, run_id=run_id, pattern=pattern, period=period, resrow))
            }
        }
    }

    results
}
