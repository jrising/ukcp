setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(dplyr)

do.redo.gendists <- F
do.uk.only <- F
do.redo.pattassigns <- F

gmst <- read.csv("climate/cmip6/gmst.csv")

gmst.base <- gmst[gmst$period == "1995-2015",]
gmst.2100 <- gmst[gmst$period == "2081-2100",]

gmst2 <- gmst.2100 %>% left_join(gmst.base, by='model', suffix=c('', '.base'))
gmst2$warming <- gmst2$gsat - gmst2$gsat.base + 0.85

if (do.redo.gendists) {
    library(raster)
    library(abind)

    results <- data.frame()
    for (scenario in c('ssp126', 'ssp370')) {
        options <- gmst2[gmst2$scenario == scenario,]

        ## 1. Calculate average correlation between GCM temperature and anomalies, across grid-cells
        allvals <- NULL
        draws <- matrix(NA, 1e5, nrow(options))
        for (ii in 1:nrow(options)) {
            if (do.uk.only) {
                rr <- raster(paste0("climate/worldclim/uk-future/uk_bioc_", options$model[ii], "_", scenario, "_2081-2100.nc"))
                dd <- as.matrix(rr)
            } else {
                system(paste0("unzip \"", file.path(path.expand(datapath), paste0("climate/worldclim/future/wc2.1_10m_bioc_", options$model[ii], "_", scenario, "_2081-2100.zip")), "\" -d ~/tmp"))
                for (tiffile in list.files("~/tmp/share", recursive=T)) {
                    rr <- raster(paste0("~/tmp/share/", tiffile), band=1)
                }
                dd <- as.matrix(rr)
                system("rm -r ~/tmp/share")
            }

            if (is.null(allvals))
                allvals <- dd
            else
                allvals <- abind(allvals, dd, along=3)
            draws[, ii] <- rnorm(1e5)
        }
        cors <- apply(allvals, 1:2, function(vv) cor(options$warming, vv - options$gsat.base))
        weights <- as.matrix(area(rr))
        target <- weighted.mean(abs(cors), weights, na.rm=T)

        ## Now use draws to do optimization
        res <- optimize(function(tau) {
            gcors <- sapply(1:1e5, function(ii) cor(options$warming, draws[ii,] * tau + options$warming))
        (target - mean(gcors))^2
        }, c(0, 1))

        results <- rbind(results, data.frame(scenario, tau=res$minimum))
    }

    write.csv(results, "climate/worldclim/gendists.csv", row.names=F)
}

source("lib/project.R")

if (do.redo.pattassigns) {
    all(alldraws$run_id[alldraws$scenario == 'ssp126'] == alldraws$run_id[alldraws$scenario == 'ssp370'])

    patterns <- data.frame()
    for (run_id in unique(alldraws$run_id)) {
        print(run_id)

        ## Repeat until same pattern
        while (T) {
            warming <- get.warming.eoc('ssp126', run_id)
            pattern1 <- get.pattern('ssp126', warming)

            warming <- get.warming.eoc('ssp370', run_id)
            pattern2 <- get.pattern('ssp370', warming)

            if (pattern1 == pattern2) {
                pattern <- pattern1
                break
            }
        }

        patterns <- rbind(patterns, data.frame(run_id, pattern))
    }

    write.csv(patterns, "climate/worldclim/common-patterns.csv", row.names=F)
}

## Plot all run_ids

if (F) {
    ## If want to choose a different pattern for each scenario
    pdf <- data.frame()
    for (scenario in unique(alldraws$scenario)) {
        for (run_id in unique(alldraws$run_id[alldraws$scenario == scenario])) {
            print(c(scenario, run_id))
            warming <- get.warming.eoc(scenario, run_id)
            pattern <- get.pattern(scenario, warming)
            pdf <- rbind(pdf, data.frame(scenario, run_id, warming, pattern))
        }
    }

    ## If want to use the common patterns
    pdf <- data.frame()
    for (scenario in unique(alldraws$scenario)) {
        for (run_id in unique(alldraws$run_id[alldraws$scenario == scenario])) {
            print(c(scenario, run_id))
            warming <- get.warming.eoc(scenario, run_id)
            pdf <- rbind(pdf, data.frame(scenario, run_id, warming))
        }
    }

    pdf <- pdf %>% left_join(read.csv("climate/worldclim/common-patterns.csv"))

    library(ggplot2)

    pdf$scenario.label <- "SSP1-2.6"
    pdf$scenario.label[pdf$scenario == 'ssp370'] <- "SSP3-7.0"
    pdf$`GCM Pattern` <- pdf$pattern
    gmst2$scenario.label <- "SSP1-2.6"
    gmst2$scenario.label[gmst2$scenario == 'ssp370'] <- "SSP3-7.0"
    gmst2$`GCM Pattern` <- gmst2$pattern

    ggplot(pdf, aes(warming, fill=`GCM Pattern`, colour=`GCM Pattern`)) +
        facet_wrap(~ scenario.label, ncol=1, scale="free_y") +
        geom_histogram(binwidth=.1, alpha=.8, size=0) + geom_vline(data=gmst2, aes(xintercept=warming, colour=model), size=1) +
        scale_x_continuous("Warming since pre-industrial (C)", expand=c(0, 0)) + scale_y_continuous("Monte Carlo simulations", expand=c(0, 0)) +
        theme_bw()
    ggsave("climate/worldclim/gendists.pdf", width=6.5, height=5)

    ## quantile(alldraws$value[alldraws$scenario == 'ssp370' & alldraws$year == 2100], c(.1, .9))
    ## sort(gmst2$warming[gmst2$scenario == 'ssp370'])

    ## ggplot(subset(alldraws, year == 2100), aes(value)) +
    ##     facet_wrap(~ scenario, ncol=1, scale="free_y") +
    ##     geom_histogram()
}
