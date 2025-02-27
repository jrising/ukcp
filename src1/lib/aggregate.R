 setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(raster)
library(PBSmapping)
library(dplyr)

rr <- raster("climate/worldclim/uk-present/uk_bio.nc", band=1)
rrdf <- as.data.frame(rr, xy=T)

if (F) {
    ## Check math
    county.weightmap <- read.csv("regions/gadm36_GBR_shp/weightmap-agg3.csv")
    ## county.weightmap.weight <- read.csv("regions/gadm36_GBR_shp/weightmap-weights.csv")

    indexes <- matrix(NA, dim(rr)[1], dim(rr)[2])
    for (ii in 1:nrow(county.weightmap)) {
        print(ii)
        coords <- strsplit(county.weightmap$coords[ii], "),")[[1]]
        pixids <- strsplit(county.weightmap$pix_idxs[ii], ",")[[1]]
        for (jj in 1:length(coords)) {
            yx <- as.numeric(gsub("[^-0-9.]", "", strsplit(coords[jj], ",")[[1]]))
            col <- colFromX(rr, yx[2])
            row <- rowFromY(rr, yx[1])
            index <- as.numeric(gsub("[^0-9]", "", pixids[jj])[[1]])
            indexes[row, col] <- index

            ## Big changes from S to N, small changes from W to E
            row <- rrdf[nrow(rrdf) - floor(index / dim(rr)[2]) * dim(rr)[2] - (dim(rr)[2] - index %% dim(rr)[2] - 1),]
            stopifnot(abs(row$x - yx[2]) + abs(row$y - yx[1]) < .01)
        }
    }
}

## Step 1. Translate to county regions

get.standard.agginfo <- function(admlev) {
    weightmap <- read.csv(paste0("regions/gadm36_GBR_shp/weightmap-agg", admlev, ".csv"))
    ## weightmap.weight <- read.csv("regions/gadm36_GBR_shp/weightmap-weights.csv")

    shp <- importShapefile(paste0("regions/gadm36_GBR_shp/gadm36_GBR_", admlev, ".shp"))
    polydata <- attr(shp, 'PolyData')

    agginfo <- get.agginfo(polydata, weightmap, paste0('NAME_', admlev))
    agginfo
}

get.agginfo <- function(polydata, weightmap, namecol) {
    agginfo <- list()
    for (ii in 1:nrow(polydata)) {
        pixids <- strsplit(weightmap$pix_idxs[ii], ",")[[1]]
        indexes <- as.numeric(gsub("[^0-9]", "", pixids))
        rows <- nrow(rrdf) - floor(indexes / dim(rr)[2]) * dim(rr)[2] - (dim(rr)[2] - indexes %% dim(rr)[2] - 1)
        values <- gsub("[^-0-9e.]", "", strsplit(weightmap$rel_area[ii], ",")[[1]])
        agginfo[[ii]] <- list(rows=rows, weights=as.numeric(values), name=as.character(polydata[ii, namecol]))
    }

    agginfo
}

agginfo3 <- get.standard.agginfo(3)

## Aggregate values to an administrative level
##   order can be a data.frame(x, y), or 'worldclim' (same order) or 'worldclim-nona' (same order minus nas)
county.average <- function(values, agginfo=agginfo3, order='worldclim', round.digits=NULL) {
    if (class(order) == 'data.frame') {
        order$values <- values
        if (is.null(round.digits)) {
            values <- (rrdf %>% left_join(order))$values
        } else {
            rrdf$x.r <- round(rrdf$x, round.digits)
            rrdf$y.r <- round(rrdf$y, round.digits)
            order$x.r <- round(order$x, round.digits)
            order$y.r <- round(order$y, round.digits)
            values <- (rrdf %>% left_join(order, by=c('x.r', 'y.r')))$values
        }
    } else if (order == 'worldclim-nona') {
        allvals <- rep(NA, nrow(rrdf))
        allvals[!is.na(rrdf$variable)] <- values
        values <- allvals
    }
    results <- c()
    for (ii in 1:length(agginfo)) {
        valids <- !is.na(values[agginfo[[ii]]$rows])
        weights <- agginfo[[ii]]$weights[valids] / sum(agginfo[[ii]]$weights[valids])
        results <- c(results, sum(values[agginfo[[ii]]$rows[valids]] * weights))
    }
    results
}

aggregate.gridded.county <- function(gridded, op, run_id.limit=Inf, col='damage', round.digits=NULL) {
    stopifnot(op %in% c('sum', 'mean'))

    if (op == 'sum')
        pops <- read.csv("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.csv")

    byadm3 <- data.frame()
    maxrid <- min(run_id.limit, max(gridded$run_id))
    for (scn in unique(gridded$scenario)) {
        for (rid in 0:maxrid) {
            for (per in unique(gridded$period)) {
                print(c(scn, rid, per))

                subgrid <- subset(gridded, scenario == scn & run_id == rid & period == per)
                value <- county.average(subgrid[, col, drop=T], order=as.data.frame(subgrid[, c('x', 'y')]), round.digits=round.digits) # (sum_i p_i y_i) / sum_i p_i
                if (op == 'mean')
                    byadm3 <- rbind(byadm3, cbind(scenario=scn, run_id=rid, period=per, county=sapply(agginfo3, function(x) x$name), avg=value))
                else {
                    byadm3.this <- data.frame(county=sapply(agginfo3, function(x) x$name), avg=value)
                    byadm3.this2 <- byadm3.this %>% left_join(pops, by=c('county'='NAME_3'))
                    byadm3 <- rbind(byadm3, cbind(scenario=scn, run_id=rid, period=per, county=byadm3.this2$county, tot=byadm3.this2$avg * byadm3.this2$popsum / sum(byadm3.this2$popsum)))
                }
            }
        }
    }

    byadm3
}

find.region <- function(name, agginfo=agginfo3) {
    for (ii in 1:length(agginfo)) {
        if (agginfo[[ii]]$name == name) {
            print(ii)
            print(agginfo[[ii]])
            break
        }
    }
}

get.gridded.pop <- function() {
    library(PBSmapping)
    library(raster)

    shp <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_0.shp")
    temps <- raster("climate/worldclim/uk-present/uk_bio.nc", band=1)

    pops <- raster("socioeconomics/LandScan Global 2019/lspop2019/w001001.adf")
    pops2 <- aggregate(crop(pops, bbox(temps)), fact=20, fun=sum)

    pops2.df <- as.data.frame(pops2, xy=T)
    pops2.df$EID <- 1:nrow(pops2.df)
    names(pops2.df)[1:2] <- c('X', 'Y')
    events <- as.EventData(pops2.df, projection=1)

    found <- findPolys(events, shp, maxRows=nrow(events))
    pops2.df$is.uk <- F
    pops2.df$is.uk[found$EID] <- T

    pops2.df$X.2 <- round(pops2.df$X, 2)
    pops2.df$Y.2 <- round(pops2.df$Y, 2)

    pops2.df
}

force.match <- function(gridded, regional, gridweight, grid2region=NULL, col='weight') {
    if (!is.null(grid2region)) {
        print("Multiple regions not implemented yet.")
        return()
    }

    totweight <- sum(gridweight[gridweight$is.uk, col, drop=T], na.rm=T)

    gridded$total <- NA
    for (scn in unique(regional$scenario)) {
        for (rid in unique(regional$run_id[regional$scenario == scn])) {
            for (per in unique(regional$period)) {
                print(c(scn, rid, per))
                ## Ensure that sum(gridded$total) = regional$total
                rows <- which(gridded$scenario == scn & gridded$run_id == rid & gridded$period == per)
                subgrid <- gridded[rows,]
                uklevel <- subset(regional, scenario == scn & run_id == rid & period == per)
                uktotal <- totweight * uklevel$damage

                subgrid2 <- subgrid %>% left_join(gridweight, by=c('x.2'='X.2', 'y.2'='Y.2'))


                gridtotal <- subgrid2[subgrid2$is.uk, col, drop=T] * subgrid2$damage[subgrid2$is.uk]
                gridded$total[rows[subgrid2$is.uk]] <- force.match.vector(gridtotal, uktotal, subgrid2[subgrid2$is.uk, col, drop=T])
            }
        }
    }

    gridded
}

force.match.vector <- function(many, one, weight=NULL) {
    if (is.na(one))
        return(many * NA)
    if (is.null(weight))
        weight <- rep(1, length(many))

    summed <- sum(weight * many)

    if (sign(summed) != sign(one)) {
        extratotal <- one - summed
        many + weight * extratotal / sum(weight)
    } else {
        ratio <- one / summed
        many * ratio
    }
}

shp.adm3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")

reg2adm3 <- function(shp.reg, col='geo_region') {
    polydata.adm3 <- attr(shp.adm3, 'PolyData')

    polydata.reg <- attr(shp.reg, 'PolyData')

    ## First run with PID rollup, so don't get edge cases
    centroids.PID <- calcCentroid(shp.adm3, rollup=1)
    centroids.PID$EID <- 1:nrow(centroids.PID)

    events <- as.EventData(centroids.PID, projection=1)

    found <- findPolys(events, shp.reg)

    library(dplyr)

    found$PID.adm3 <- centroids.PID$PID[found$EID]
    found2 <- found %>% group_by(PID.adm3) %>% summarize(PID=unique(PID))

    stopifnot(all(table(found2$PID.adm3) == 1))

    polydata.adm3$geo_region <- NA
    polydata.adm3$geo_region[found2$PID.adm3] <- as.character(polydata.reg[found2$PID, col])

    ## Now run with no rollup for missing regions
    centroids <- calcCentroid(shp.adm3, rollup=3)
    centroids$EID <- 1:nrow(centroids)

    events <- as.EventData(centroids, projection=1)
    found <- findPolys(events, shp.reg)

    found$PID.adm3 <- centroids$PID[found$EID]
    found$SID.adm3 <- centroids$SID[found$EID]
    found2 <- found %>% group_by(PID.adm3, SID.adm3) %>% summarize(PID=unique(PID))

    found2 <- found2[found2$PID.adm3 %in% polydata.adm3$PID[is.na(polydata.adm3$geo_region)],]

    areas <- calcArea(shp.adm3, rollup=3)

    found3 <- found2 %>% left_join(areas, by=c('PID.adm3'='PID', 'SID.adm3'='SID')) %>% group_by(PID.adm3) %>%
        summarize(PID=PID[which.max(area)])

    polydata.adm3$geo_region[found2$PID.adm3] <- as.character(polydata.reg[found2$PID, col])

    if (any(is.na(polydata.adm3$geo_region))) {
        source("lib/distance.R")
        centroids.reg <- calcCentroid(shp.reg, rollup=3)
        for (ii in which(is.na(polydata.adm3$geo_region))) {
            dists <- gcd.slc(centroids.PID$X[ii], centroids.PID$Y[ii], centroids.reg$X, centroids.reg$Y)
            jj <- centroids.reg$PID[which.min(dists)]
            polydata.adm3$geo_region[ii] <- as.character(polydata.reg[jj, col])
        }
    }

    polydata.adm3
}

full.force.match <- function(byadm3, byregion, polydata.adm3) { # both results need `damage` column
    byadm32 <- byadm3 %>% left_join(polydata.adm3[, c('PID', 'NAME_3', 'geo_region')], by=c('county'='NAME_3'))
    stopifnot(all(unique(byregion$region) %in% unique(byadm32$geo_region)))

    byadm33 <- byadm32 %>% left_join(byregion, by=c('scenario', 'run_id', 'period', 'geo_region'='region'), suffix=c('.adm3', '.reg'))

    byregsum <- byadm33 %>% group_by(scenario, run_id, period, geo_region) %>% summarize(damage.sum=sum(damage.adm3), damage.reg=damage.reg[1])
    mod <- lm(damage.reg ~ 0 + damage.sum, data=byregsum)

    byadm33$damage.adm3.scaled <- byadm33$damage.adm3 * mod$coeff

    byadm34 <- byadm33 %>% group_by(scenario, run_id, period, geo_region) %>% summarize(PID=PID, county=county, damage=force.match.vector(damage.adm3.scaled, damage.reg[1]))
    byadm34
}
