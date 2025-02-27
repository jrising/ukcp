setwd("~/Dropbox/UK Economic Risks")

library(dplyr)
library(raster)

fit.gendists <- read.csv("climate/worldclim/gendists.csv")
alldraws <- read.csv("climate/gsat/ssps_26_70_posttp.csv")
common.patterns <- read.csv("climate/worldclim/common-patterns.csv")

gmst <- read.csv("climate/cmip6/gmst.csv")
gmst.base <- gmst[gmst$period == "1995-2015",]
gmst.2100 <- gmst[gmst$period == "2081-2100",]

gmst2 <- gmst.2100 %>% left_join(gmst.base, by='model', suffix=c('', '.base'))
gmst2$warming <- gmst2$gsat - gmst2$gsat.base + 0.85

periods <- list('2011-2030'=c(2011, 2030), '2041-2060'=c(2041, 2060), '2081-2100'=c(2081, 2100))

## EOC warming
get.warming.eoc <- function(scenario, run_id) {
    draw <- alldraws[alldraws$scenario == scenario & alldraws$run_id == run_id,]
    warming.base <- mean(draw$value[draw$year >= 1995 & draw$year < 2015])
    warming.eoc <- mean(draw$value[draw$year >= 2081 & draw$year < 2100])

    warming.eoc - warming.base + 0.85
}

get.pattern <- function(scenario, warming) {
    ## Find one of the patterns
    probs <- dnorm(warming, gmst2$warming[gmst2$scenario == scenario], fit.gendists$tau[fit.gendists$scenario == scenario])
    pattern <- sample(1:length(probs), 1, prob=probs)

    gmst2$model[pattern]
}

get.shares <- function(year) {
    centers <- list('1970-2000'=1985, '2021-2040'=2030, '2041-2060'=2050, '2081-2100'=2090)
    if (year <= centers[['1970-2000']])
        return(list('1970-2000'=1))
    if (year >= centers[['2081-2100']])
        return(list('2081-2100'=1))

    latestbefore <- '1970-2000'
    earliestafter <- '2081-2100'
    result <- list()
    for (period in names(centers)) {
        if (centers[[period]] == year) {
            result[[period]] <- 1
            return(result)
        }

        if (centers[[period]] < year && centers[[period]] > centers[[latestbefore]])
            latestbefore <- period
        if (centers[[period]] > year && centers[[period]] < centers[[earliestafter]])
            earliestafter <- period
    }

    shareafter <- (year - centers[[latestbefore]]) / (centers[[earliestafter]] - centers[[latestbefore]])
    result[[latestbefore]] <- 1 - shareafter
    result[[earliestafter]] <- shareafter
    return(result)
}

filecache <- list()
get.grid.temps <- function(pattern, scenario, filedur) {
    if (filedur == '1970-2000')
        cachekey <- filedur
    else
        cachekey <- paste(pattern, scenario, filedur)
    if (cachekey %in% names(filecache))
        return(filecache[[cachekey]])

    if (filedur == '1970-2000') {
        rr <- raster("climate/worldclim/uk-present/uk_bio.nc", band=1)
    } else {
        rr <- raster(file.path("climate/worldclim/uk-future", paste0("uk_bioc_", pattern, "_", scenario, "_", filedur, ".nc")), band=1)
    }
    df <- as.data.frame(rr, xy=T)

    filecache[[cachekey]] <<- df

    df
}

grid.pattern.cdiff <- function(pattern, scenario, year) {
    shares <- get.shares(year)

    cdiff.global.0 <- gmst$gsat[gmst$scenario == 'historical' & gmst$model == pattern & gmst$period == '1970-2000']
    cdiff.0 <- get.grid.temps(pattern, scenario, '1970-2000')

    ## Construct grided average
    temps <- NULL
    for (filedur in names(shares)) {
        thistemps <- get.grid.temps(pattern, scenario, filedur)
        ## Subtract GMST for this model-period
        if (filedur == '1970-2000') {
            cdiff.global <- gmst$gsat[gmst$scenario == 'historical' & gmst$model == pattern & gmst$period == filedur]
        } else
            cdiff.global <- gmst$gsat[gmst$scenario == scenario & gmst$model == pattern & gmst$period == filedur]

        ## Adjust to pattern warmings
        thistemps$variable <- (thistemps$variable - cdiff.0$variable) - (cdiff.global - cdiff.global.0)
        thistemps$variable <- thistemps$variable * shares[[filedur]]

        if (is.null(temps))
            temps <- thistemps
        else
            temps$variable <- temps$variable + thistemps$variable
    }

    temps
}

maybe.zeroneg <- function(cdiffs, values, zeroneg=-Inf, zeroneg.range=NA) {
    if (zeroneg > -Inf) {
        rows1 <- values < 0 & cdiffs > zeroneg & cdiffs < zeroneg + zeroneg.range
        values[rows1] <- values[rows1] * (zeroneg.range - (T - zeroneg)) / zeroneg.range
        rows2 <- cdiffs >= zeroneg + zeroneg.range
        values[rows2] <- 0
    }
    values
}

grid.project <- function(la, zero.y0=NA, zero.y1=NA, per.scenario=Inf, aggregate.cdiff=function(x) x, gridpatts=common.patterns, grid2region=NULL, zeroneg=-Inf, zeroneg.range=NA, run_id.limit=Inf) {
    results <- data.frame()
    maxrid <- min(run_id.limit, max(alldraws$run_id))
    for (scenario in unique(alldraws$scenario)) {
        for (run_id in 0:maxrid) {
            if (run_id == per.scenario)
                break
            print(c(scenario, run_id))

            warming <- get.warming.eoc(scenario, run_id)
            if (is.null(gridpatts))
                pattern <- get.pattern(scenario, warming)
            else {
                if ('scenario' %in% names(gridpatts))
                    pattern <- gridpatts$pattern[gridpatts$scenario == scenario & gridpatts$run_id == run_id]
                else
                    pattern <- gridpatts$pattern[gridpatts$run_id == run_id]
            }

            if (!is.na(zero.y0))
                warming.0 <- mean(alldraws$value[alldraws$scenario == scenario & alldraws$run_id == run_id & alldraws$year >= zero.y0 & alldraws$year < zero.y1])
            else
                warming.0 <- 0

            subres <- data.frame()
            for (period in names(periods)) {
                for (year in periods[[period]][1]:periods[[period]][2]) {
                    warming.now <- alldraws$value[alldraws$scenario == scenario & alldraws$run_id == run_id & alldraws$year == year]
                    cdiffs <- grid.pattern.cdiff(pattern, scenario, year)
                    cdiffs <- aggregate.cdiff(cdiffs)
                    cdiffs <- cdiffs[!is.na(cdiffs$variable), ]

                    fullcdiff <- (warming.now - warming.0) + cdiffs$variable

                    if (!is.null(grid2region)) {
                        damage <- rep(NA, length(fullcdiff))
                        for (region in names(la)) { # if not in la, left as NA
                            rows <- grid2region == region
                            draw <- sample.int(dim(la[[region]]$coeff)[1], 1)
                            coeffs <- la[[region]]$coeff[draw, ]
                            regcdiff <- fullcdiff[rows]
                            if (length(coeffs) == 3)
                                regdamage <- coeffs[2] * regcdiff + coeffs[3] * regcdiff^2
                            else if (length(coeffs) == 2)
                                regdamage <- coeffs[2] * regcdiff
                            else
                                print("Unknown length of coeffs.")
                            damage[rows] <- regdamage
                        }
                    } else {
                        draw <- sample.int(dim(la$coeff)[1], 1)
                        coeffs <- la$coeff[draw, ]
                        damage <- coeffs[2] * fullcdiff + coeffs[3] * fullcdiff^2
                    }

                    damage <- maybe.zeroneg(fullcdiff, damage, zeroneg, zeroneg.range)
                    subres <- rbind(subres, data.frame(period, year, x=cdiffs$x, y=cdiffs$y, cdiff=fullcdiff, damage))
                }
            }

            resrow <- subres %>% group_by(period, x, y) %>% summarize(cdiff=mean(cdiff), damage=mean(damage))
            results <- rbind(results, cbind(scenario=scenario, run_id=run_id, pattern=pattern, resrow))
        }
    }

    results
}

get.grid2region <- function(shapepath) {
    library(PBSmapping)

    shp <- importShapefile(shapepath)
    polydata <- attr(shp, 'PolyData')

    get.grid2region.shape(shp, polydata$geo_region)
}

get.grid2region.shape <- function(shp, geonames) {
    source("lib/aggregate.R")
    source("lib/distance.R")

    validrrdf <- rrdf[!is.na(rrdf$variable),]
    events <- data.frame(EID=1:nrow(validrrdf), X=validrrdf$x, Y=validrrdf$y)
    events <- as.EventData(events, projection=1)
    found <- findPolys(events, shp)

    grid2region <- rep(NA, nrow(validrrdf))
    grid2region[found$EID] <- as.character(geonames[found$PID])

    centroids <- calcCentroid(shp, rollup=1)
    for (ii in which(is.na(grid2region))) {
        dists <- gcd.slc(validrrdf$x[ii], validrrdf$y[ii], centroids$X, centroids$Y)
        grid2region[ii] <-  as.character(geonames[which.min(dists)])
    }

    grid2region
}
