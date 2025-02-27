library(ggplot2)

source("lib/project.R")

do.cmip6 <- T

if (do.cmip6) {
    get.country.temps <- function(pattern, scenario, filedur) {
        if (filedur == '1970-2000')
            cachekey <- paste("country", filedur)
        else
            cachekey <- paste("country", pattern, scenario, filedur)
        if (cachekey %in% names(filecache))
            return(filecache[[cachekey]])

        if (filedur == '1970-2000') {
            df <- read.csv("climate/worldclim/adm0/wc2.1_10m_bio.csv")
        } else {
            df <- read.csv(file.path("climate/worldclim/adm0", paste0("wc2.1_10m_bioc_", pattern, "_", scenario, "_", filedur, ".csv")))
        }
        df2 <- data.frame(ADM0=df$ADM0_A3, tas=df$band_data)
        df2$tas[df2$tas == 0] <- NA

        filecache[[cachekey]] <<- df2

        df2
    }

    get.country.year.temps <- function(warming.now, pattern, scenario, year) {
        shares <- get.shares(year)

        if (is.null(pattern))
            pattern <- 'MIROC6'

        cdiff.global.0 <- gmst$gsat[gmst$scenario == 'historical' & gmst$model == pattern & gmst$period == '1970-2000']
        cdiff.0 <- get.country.temps(pattern, scenario, '1970-2000')

        ## Construct grided average
        temps <- NULL
        for (filedur in names(shares)) {
            thistemps <- get.country.temps(pattern, scenario, filedur)
            ## Subtract GMST for this model-period
            if (filedur == '1970-2000') {
                cdiff.global <- gmst$gsat[gmst$scenario == 'historical' & gmst$model == pattern & gmst$period == filedur]
            } else
                cdiff.global <- gmst$gsat[gmst$scenario == scenario & gmst$model == pattern & gmst$period == filedur]

            ## Adjust to pattern warmings
            thistemps$tas <- (thistemps$tas - cdiff.0$tas) - (cdiff.global - cdiff.global.0)
            thistemps$tas <- thistemps$tas * shares[[filedur]]

            if (is.null(temps))
                temps <- thistemps
            else
                temps$tas <- temps$tas + thistemps$tas
        }

        fullcdiff <- warming.now + temps$tas
        fulltemp <- cdiff.0$tas + fullcdiff

        temps$tas <- fulltemp
        temps
    }

    year1 <- 1970
} else {
    cmip5.global <- read.csv("climate/cmip5/FAIR-gmst.csv")

    cmip5.rcp45 <- read.csv("climate/cmip5/rcp45.csv")
    cmip5.rcp45 <- cmip5.rcp45[nchar(cmip5.rcp45$region) == 3,]
    cmip5.rcp85 <- read.csv("climate/cmip5/rcp85.csv")
    cmip5.rcp85 <- cmip5.rcp85[nchar(cmip5.rcp85$region) == 3,]

    cmip5.rcp45.base <- cmip5.rcp45 %>% filter(year >= 1995 & year < 2015) %>% group_by(region) %>% summarize(base=mean(mean))
    cmip5.rcp85.base <- cmip5.rcp85 %>% filter(year >= 1995 & year < 2015) %>% group_by(region) %>% summarize(base=mean(mean))

    cmip5.rcp45x <- cmip5.rcp45 %>% left_join(cmip5.rcp45.base) %>% arrange(region)
    cmip5.rcp85x <- cmip5.rcp85 %>% left_join(cmip5.rcp85.base) %>% arrange(region)

    cmip5.global.base <- cmip5.global %>% filter(year >= 1995 & year < 2015) %>% summarize(rcp45.base=mean(rcp45), rcp85.base=mean(rcp85))
    cmip5.globalx <- cbind(cmip5.global, cmip5.global.base)

    year1 <- min(cmip5.rcp45$year)

    ## Ignore pattern, swap RCPs
    get.country.year.temps <- function(warming.now, pattern, scenario, year) {
        if (year > 2099)
            year <- 2099
        if (scenario == 'ssp126') {
            values <- cmip5.rcp45x[cmip5.rcp45x$year == year,]
            values.global <- cmip5.globalx[cmip5.globalx$year == year, c('rcp45', 'rcp45.base')]
        } else if (scenario == 'ssp370') {
            values <- cmip5.rcp85x[cmip5.rcp85x$year == year,]
            values.global <- cmip5.globalx[cmip5.globalx$year == year, c('rcp85', 'rcp85.base')]
        }
        names(values.global) <- c('gmst', 'base')

        ## Pattern is T_it - T_gt
        ## But then need to add on a number that's consistent with T_gt
        ## warming was 0.85 in baseline period,
        data.frame(ADM0=values$region, tas=(values$mean - values.global$gmst) + values.global$base + (warming.now - 0.85))
    }
}

country.project.single <- function(setup, simulate) {
    onealldraws <- alldraws %>% group_by(scenario, year) %>% summarize(value=mean(value))

    allres <- data.frame()
    for (scenario in unique(onealldraws$scenario)) {
        print(c(scenario))

        setupinfo <- setup(scenario, NULL)

        results <- data.frame()
        for (year in year1:2100) {
            warming.now <- onealldraws$value[onealldraws$scenario == scenario & onealldraws$year == year]

            temps <- get.country.year.temps(warming.now, NULL, scenario, year)

            resrow <- simulate(setupinfo, year, temps$ADM0, temps$tas)
            results <- rbind(results, cbind(year=year, resrow))
        }

        results0 <- results %>% filter(year <= 2000) %>% group_by(ADM0) %>% summarize(impact0=mean(impact))
        results2 <- results %>% left_join(results0)
        results2$dimpact <- results2$impact - results2$impact0

        allres <- rbind(allres, data.frame(scenario=scenario, results2))
    }

    allres
}

country.project <- function(setup, simulate, nrun=Inf) {
    allres <- list()
    for (scenario in unique(alldraws$scenario)) {
        scnres <- data.frame()
        for (run_id in unique(alldraws$run_id[alldraws$scenario == scenario])) {
            print(c(scenario, run_id))
            if (run_id >= nrun)
                break

            setupinfo <- setup(scenario, run_id)

            warming <- get.warming.eoc(scenario, run_id)
            pattern <- get.pattern(scenario, warming)

            results <- data.frame()
            for (year in year1:2100) {
                warming.now <- alldraws$value[alldraws$scenario == scenario & alldraws$run_id == run_id & alldraws$year == year]

                temps <- get.country.year.temps(warming.now, pattern, scenario, year)

                resrow <- simulate(setupinfo, year, temps$ADM0, temps$tas)
                results <- rbind(results, cbind(year=year, resrow))
            }

            results0 <- results %>% filter(year <= 2000) %>% group_by(ADM0) %>% summarize(impact0=mean(impact))
            results2 <- results %>% left_join(results0)
            results2$dimpact <- results2$impact - results2$impact0

            scnres <- rbind(scnres, data.frame(run_id=run_id, results2[, c('year', 'ADM0', 'dimpact')]))
        }

        allres[[scenario]] <- scnres
    }

    allres
}

byscen <- function(allres, callback) {
    allres2 <- list()
    for (scn in names(allres))
        allres2[[scn]] <- callback(allres[[scn]])
    allres2
}

library(cowplot)
library(grid)
library(scales)
library(PBSmapping)
global.adm0 <- importShapefile("regions/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
global.adm0.polydata <- attr(global.adm0, 'PolyData')

make.map.proj <- function(outdir, prefix, results, scale.title="change in\nGDP p.c. (%)", loval=NULL, hival=NULL) {
    for (scn in c('ssp126', 'ssp370')) {
        for (per in c(2020, 2050, 2090)) {
            for (measure in c('mean', 'ci05', 'ci95')) {
                subres <- subset(results[[scn]], year > per - 10 & year <= per + 10)

                if (measure == 'mean') {
                    subres2 <- subres %>% group_by(year, ADM0) %>% summarize(fracloss=1 - exp(mean(impact, na.rm=T)))
                    scale.title.full <- paste0("Expected\n", scale.title)
                } else if (measure == 'ci05') {
                    subres2 <- subres %>% group_by(year, ADM0) %>% summarize(fracloss=1 - exp(quantile(impact, .05, na.rm=T)))
                    scale.title.full <- paste0("5% q.\n", scale.title)
                } else if (measure == 'ci95') {
                    subres2 <- subres %>% group_by(year, ADM0) %>% summarize(fracloss=1 - exp(quantile(impact, .95, na.rm=T)))
                    scale.title.full <- paste0("1-in-20\n", scale.title)
                } else
                    print("Error")

                subres3 <- subres2 %>% group_by(ADM0) %>% summarize(fracloss=mean(fracloss, na.rm=T))

                make.map.inner(outdir, paste(prefix, scn, per, measure, sep='-'), subres3, scale.title.full, loval, hival)
            }
        }
    }
}

make.map.post <- function(outdir, prefix, results, scale.title="change in\nGDP p.c. (%)  ", loval=NULL, hival=NULL) {
    results$fracloss <- 1 - exp(results$impact)
    for (scn in unique(results$scenario)) {
        for (per in unique(results$period)) {
            for (measure in c('mean', 'ci05', 'ci95')) {
                subres <- subset(results, scenario == scn & period == per)

                if (measure == 'mean') {
                    subres2 <- subres %>% group_by(ADM0) %>% summarize(fracloss=mean(fracloss))
                    scale.title.full <- paste0("Expected\n", scale.title)
                } else if (measure == 'ci05') {
                    subres2 <- subres %>% group_by(ADM0) %>% summarize(fracloss=quantile(fracloss, .05, na.rm=T))
                    scale.title.full <- paste0("5% q.\n", scale.title)
                } else if (measure == 'ci95') {
                    subres2 <- subres %>% group_by(ADM0) %>% summarize(fracloss=quantile(fracloss, .95, na.rm=T))
                    scale.title.full <- paste0("1-in-20\n", scale.title)
                } else
                    print("Error")

                subres3 <- subres2 %>% group_by(ADM0) %>% summarize(fracloss=mean(fracloss, na.rm=T))

                make.map.inner(outdir, paste(prefix, scn, per, measure, sep='-'), subres3, scale.title.full, loval, hival)

            }
        }
    }
}

make.map.inner <- function(outdir, prefix, subres3, scale.title.full, loval, hival) {
    subres4 <- subres3 %>% left_join(global.adm0.polydata[, c('PID', 'ADM0_A3')], by=c('ADM0'='ADM0_A3'))
    shp2 <- global.adm0 %>% left_join(subres4[, c('PID', 'fracloss')])

    if (is.null(loval)) {
        gp <- ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
            coord_map(ylim=c(-55, 78), projection="mollweide") + scale_x_continuous(expand=c(0, 0)) + geom_polygon(aes(fill=fracloss), colour='#808080', size=.1) +
            scale_fill_gradient2(scale.title.full, limits=c(loval, hival), low=muted("blue"),
                                 high=muted("red"), oob=squish, labels=scales::percent) +
            theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="bottom",
                               axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.border=element_blank()) + xlab(NULL) + ylab(NULL)

        ggsave(file.path(outdir, paste0(prefix, '.pdf')), gp, width=7, height=3.7)
    } else {
        gp <- ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
            coord_map(ylim=c(-55, 78), projection="mollweide") + scale_x_continuous(expand=c(0, 0)) + geom_polygon(aes(fill=fracloss), colour='#808080', size=.1) +
            scale_fill_gradient2(scale.title.full, limits=c(loval, hival), low=muted("blue"),
                                 high=muted("red"), oob=squish, labels=scales::percent) +
            theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.position="bottom",
                               axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.border=element_blank()) + xlab(NULL) + ylab(NULL)

        ggsave(file.path(outdir, paste0(prefix, '.pdf')), gp + theme(legend.position="none"), width=7, height=3.2)

        legend <- get_legend(gp)
        pdf(file.path(outdir, paste0(prefix, '-legend.pdf')), width=3, height=1)
        grid.newpage()
        grid.draw(legend)
        dev.off()
    }
}
