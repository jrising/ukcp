setwd("~/Open Modeling Group Dropbox/UK Economic Risks/channels/energy")

source("../../lib/damagefunc.R")

library(dplyr)
library(ncdf4)

do.method <- 2 # don't assume I understand maps_data.nc4

if (do.method == 1) {
    ## billions of 2019 dollars
    ## (according to https://github.com/ClimateImpactLab/energy-code-release-2020/blob/master/3_post_projection/1_visualise_impacts/plot_maps.R)
    df <- read.csv("energy_data_release_2021oct21/OUTPUT/projection_system_outputs/mapping_data/main_model-total_energy-SSP3-rcp85-high-fulladapt-price014-2099-map.csv")
    df <- df[grep("GBR", df$region),]
    df$seshare <- ((df$q95 - df$q50) / 1.644854) / df$damage

    nc <- nc_open("energy_data_release_2021oct21/DATA/intermediate_data/maps_data.nc4")
    damages <- ncvar_get(nc, 'damage_inc_share')
    years <- ncvar_get(nc, 'year')
    regions <- ncvar_get(nc, 'region')
    gdppc <- ncvar_get(nc, 'gdppc')
    damages <- damages[grep("GBR", regions),,,]
    gdppc <- gdppc[,,grep("GBR", regions)]
    regions <- regions[grep("GBR", regions)]

    ## > ncvar_get(nc, 'ssp')
    ## [1] "SSP3"
    ## > ncvar_get(nc, 'rcp')
    ## [1] "rcp45" "rcp85"
    ## > ncvar_get(nc, 'model')
    ## [1] "IIASA GDP"       "OECD Env-Growth"

    library(reshape2)

    damages <- melt(damages, varnames=c('region', 'year', 'model', 'rcp'), value.name='damages_incshare')
    damages$region <- regions[damages$region]
    damages$year <- years[damages$year]
    damages$model <- c("IIASA GDP", "OECD Env-Growth")[damages$model]
    damages$rcp <- c("rcp45", "rcp85")[damages$rcp]

    gdppc <- melt(gdppc, varnames=c('year', 'model', 'region'), value.name='gdppc')
    gdppc$region <- regions[gdppc$region]
    gdppc$year <- years[gdppc$year]
    gdppc$model <- c("IIASA GDP", "OECD Env-Growth")[gdppc$model]

    damages2 <- damages %>% left_join(gdppc)

    ## Group into 20 year chunks
    tas1 <- read.csv("~/Dropbox/NextGen IAMs/Mortality/data/covariates/tas/rcp45-SSP3.csv")
    tas1 <- tas1[grep("GBR", tas1$region),]
    tas1$rcp <- "rcp45"
    tas2 <- read.csv("~/Dropbox/NextGen IAMs/Mortality/data/covariates/tas/rcp85-SSP3.csv")
    tas2 <- tas2[grep("GBR", tas2$region),]
    tas2$rcp <- "rcp85"

    tas <- rbind(tas1, tas2)
    tas2 <- tas %>% left_join(subset(tas, year == '1980-1999'), by=c('region', 'rcp'), suffix=c('', '.base'))

    damages2$yearspan <- paste(floor(damages2$year / 20) * 20, floor(damages2$year / 20) * 20 + 19, sep='-')
    damages3 <- damages2 %>% group_by(region, model, rcp, yearspan) %>% summarize(damages_incshare=mean(damages_incshare))

    allinfo <- damages3 %>% left_join(tas2, by=c('region', 'rcp', 'yearspan'='year')) %>% left_join(df[, c('region', 'seshare')], by='region')

    saved <- fit.damages.byregion(allinfo$region, (allinfo$mean - allinfo$mean.base),
                                  allinfo$damages_incshare,
                                  (allinfo$damages_incshare * allinfo$seshare))

    save(saved, file="rodeetal.RData")

    ## Construct average over regions
    ddf <- saved$ddf %>% group_by(T) %>% summarize(mu=mean(mu), ci25=mean(ci25), ci75=mean(ci75))
    pdf <- saved$pdf %>% group_by(T) %>% summarize(mu=mean(mu), ses=mean(ses))

    ggplot(ddf, aes(T, mu)) +
        geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
        geom_point(data=pdf) +
        scale_x_continuous("Difference in temperature from 1980-1999 (C)", expand=c(0, 0)) +
        theme_bw() + ylab("Change in energy expenditures (share of income)")
}

if (do.method == 2) {
    ## df1 <- read.csv("energy_data_release_2021oct21/OUTPUT/projection_system_outputs/mapping_data/main_model-electricity-SSP3-rcp85-high-fulladapt-impact_pc-2090-map.csv")
    ## df1 <- df1[grep("GBR", df1$region),]
    ## df2 <- read.csv("energy_data_release_2021oct21/OUTPUT/projection_system_outputs/mapping_data/main_model-other_energy-SSP3-rcp85-high-fulladapt-impact_pc-2090-map.csv")
    ## df2 <- df2[grep("GBR", df2$region),]

    ## billions of 2019 dollars
    ## (according to https://github.com/ClimateImpactLab/energy-code-release-2020/blob/master/3_post_projection/1_visualise_impacts/plot_maps.R)
    df <- read.csv("energy_data_release_2021oct21/OUTPUT/projection_system_outputs/mapping_data/main_model-total_energy-SSP3-rcp85-high-fulladapt-price014-2099-map.csv")
    df <- df[grep("GBR", df$region),]
    df$seshare <- ((df$q95 - df$q50) / 1.644854) / df$damage

    tas <- read.csv("~/Dropbox/NextGen IAMs/Mortality/data/covariates/tas/rcp85-SSP3.csv")
    tas <- tas[grep("GBR", tas$region),]
    tas.zero <- subset(tas, year == '1980-1999')
    tas.base <- subset(tas, year == '2000-2019')
    tas.late <- subset(tas, year == '2080-2099')

    ## df <- (tas.zero %>% left_join(tas.base, by='region', suffix=c('', '.base')) %>% left_join(tas.late, by='region', suffix=c('.zero', '.late'))) %>% left_join((df1 %>% left_join(df2, by='region', suffix=c('.elec', '.oeng'))), by='region')
    df2 <- (tas.zero %>% left_join(tas.base, by='region', suffix=c('', '.base')) %>% left_join(tas.late, by='region', suffix=c('.zero', '.late'))) %>% left_join(df, by='region')

    source("../../lib/damagefunc.R")

    saved <- fit.damages.byregion(rep(df2$region, 2),
                                  c(df2$mean.base - df2$mean.zero, df2$mean.late - df2$mean.zero),
                                  c(rep(0, nrow(df2)), df2$damage),
                                  c(rep(0, nrow(df2)), (df2$q95 - df2$q50) / 1.64))

    save(saved, file="rodeetal2.RData")

    library(ggplot2)

    ddf <- saved$ddf
    ddf$region <- sapply(ddf$region, function(ss) substring(ss, 1, 11))
    ddf$mu <- ddf$mu * 1000
    ddf$ci25 <- ddf$ci25 * 1000
    ddf$ci75 <- ddf$ci75 * 1000

    ggplot(ddf, aes(T, mu)) +
        facet_wrap(~ region) +
        geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
        #geom_point(data=saved$pdf) +
        scale_x_continuous("Difference in temperature from 1981-2000 (C)", expand=c(0, 0)) +
        theme_bw() + theme(strip.text.x=element_text(size=6), axis.text.y=element_text(size=5)) +
        ylab("Change in energy expenditures (million 2019 USD)")
}

## Project

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/project.R")

if (do.method == 2) {
    load("channels/energy/rodeetal2.RData")

    library(PBSmapping)
    shp <- importShapefile("~/Dropbox/For DJ/world-combo-201710/agglomerated-world-new.shp")
    polydata <- attr(shp, 'PolyData')

    shp <- subset(shp, PID %in% polydata$PID[polydata$ISO == 'GBR'])

    grid2region  <- get.grid2region.shape(shp, polydata$hierid)

    validrrdf <- rrdf[!is.na(rrdf$variable),]
    validrrdf$region <- NA
    validrrdf$region[1:length(grid2region)] <- grid2region

    library(ggplot2)
    ggplot(validrrdf, aes(x, y, fill=region)) +
        geom_raster() + guides(fill="none")

    results <- grid.project(saved$allla, 1980, 1999, grid2region=grid2region)

    save(results, file="rodeetal2-project.RData")

    ## First average to the CIL-regions, so don't over-count
    grid2region <- c(grid2region, rep(NA, nrow(validrrdf) - length(grid2region)))
    results$region <- rep(grid2region, nrow(results) / length(grid2region))

    results2 <- results %>% group_by(scenario, run_id, pattern, period, region) %>% summarize(cdiff=mean(cdiff), damage=mean(damage, na.rm=T))

    save(results, results2, file="channels/energy/rodeetal2-project.RData")
    ## load("channels/energy/rodeetal2-project.RData")

    ## Combine to UK level
    results2.uk <- results2 %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage, na.rm=T))
    ## https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG?locations=US
    results2.uk$damage.mgbp <- 1000 * (1 + 1.401 / 100) * 0.7798 * results2.uk$damage

    source("lib/report.R")

    tbl <- make.table(results2.uk, 'damage.mgbp')
    print(xtable(tbl, digits=1), include.rownames=F)

    source("lib/constants.R")

    results2.uk$percent <- 100 * results2.uk$damage.mgbp * 1e6 / gdp.2020.gbp

    tbl <- make.table(results2.uk, 'percent')
    print(xtable(tbl, digits=3), include.rownames=F)

    results2.map <- results2 %>% left_join(polydata, by=c('region'='hierid'))
    results2.map$damage.mgbp <- 1000 * (1 + 1.401 / 100) * 0.7798 * results2.map$damage

    make.maps.loop("channels/energy/maps", "rode2", results2.map, shp, function(subres) {
        subres %>% group_by(PID) %>% summarize(mu=-mean(damage.mgbp, na.rm=T), ci5=-quantile(damage.mgbp, .05, na.rm=T),
                                               ci95=-quantile(damage.mgbp, .95, na.rm=T))
    }, "Expected\nreduction\nin costs (£ million)", "95% CI\nreduction\nin costs (£ million)", 0, 1.5, locol='white', hicol=muted('blue'))

    ## Convert to ADM3
}
