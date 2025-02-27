library(xtable)
library(cowplot)
library(grid)
library(ggplot2)
library(scales)

make.table <- function(results, col) {
    tbl <- data.frame()
    for (per in unique(results$period)) {
        values <- data.frame(period=per)
        for (scn in c('ssp370', 'ssp126')) {
            subres <- subset(results, period == per & scenario == scn)
            values[, paste(scn, 'q05')] <- quantile(subres[, col, drop=T], .05)
            values[, paste(scn, 'mean')] <- mean(subres[, col, drop=T])
            values[, paste(scn, 'q95')] <- quantile(subres[, col, drop=T], .95)
        }

        damagediff <- subset(results, period == per & scenario == 'ssp370')[, col, drop=T] - subset(results, period == per & scenario == 'ssp126')[, col, drop=T]
        values[, 'diff q05'] <- quantile(damagediff, .05)
        values[, 'diff mean'] <- mean(damagediff)
        values[, 'diff q95'] <- quantile(damagediff, .95)

        tbl <- rbind(tbl, values)
    }

    tbl
}

library(PBSmapping)
shp.national.level <- importShapefile("~/Dropbox/UK Economic Risks/regions/nuts2-2013/nuts2-2013.shp")

make.maps <- function(outdir, prefix, shp2, scale.title.mu, scale.title.ci95, loval, hival, gradient=2, labels=NULL, small.prefix=prefix, locol=NULL, hicol=NULL) {
    gp.mu <- ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
        coord_map(xlim=c(-8, 2), ylim=c(50, 60.5)) + geom_polygon(aes(fill=mu)) +
        geom_polygon(data=shp.national.level, size=.1, fill="#00000000", colour="#808080") +
        theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                           axis.text.y=element_blank(), axis.ticks.y=element_blank()) + xlab(NULL) + ylab(NULL)

    gp.ci95 <- ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
        coord_map(xlim=c(-8, 2), ylim=c(50, 60.5)) + geom_polygon(aes(fill=ci95)) +
        geom_polygon(data=shp.national.level, size=.1, fill="#00000000", colour="#808080") +
        theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                           axis.text.y=element_blank(), axis.ticks.y=element_blank()) + xlab(NULL) + ylab(NULL)

    scale.mu.params <- list(name=scale.title.mu, oob=squish)
    scale.ci95.params <- list(name=scale.title.ci95, oob=squish)
    if (!is.null(loval)) {
        scale.mu.params[['limits']] <- c(loval, hival)
        scale.ci95.params[['limits']] <- c(loval, hival)
    }
    if (!is.null(labels)) {
        scale.mu.params[['labels']] <- percent
        scale.ci95.params[['labels']] <- percent
    }
    if (gradient == 2) {
        scale.mu.params[['low']] <- ifelse(is.null(locol), muted("blue"), locol)
        scale.mu.params[['high']] <- ifelse(is.null(hicol), muted("red"), hicol)
        scale.ci95.params[['low']] <- ifelse(is.null(locol), muted("blue"), locol)
        scale.ci95.params[['high']] <- ifelse(is.null(hicol), muted("red"), hicol)
        gp.mu <- gp.mu + do.call(scale_fill_gradient2, scale.mu.params)
        gp.ci95 <- gp.ci95 + do.call(scale_fill_gradient2, scale.ci95.params)
    } else {
        scale.mu.params[['low']] <- ifelse(is.null(locol), muted("yellow"), locol)
        scale.mu.params[['high']] <- ifelse(is.null(hicol), muted("red"), hicol)
        scale.ci95.params[['low']] <- ifelse(is.null(locol), muted("yellow"), locol)
        scale.ci95.params[['high']] <- ifelse(is.null(hicol), muted("red"), hicol)
        gp.mu <- gp.mu + do.call(scale_fill_gradient, scale.mu.params)
        gp.ci95 <- gp.ci95 + do.call(scale_fill_gradient, scale.ci95.params)
    }

    ## if (gradient == 2) {
    ##     if (!is.null(loval)) {
    ##         gp.mu <- gp.mu + scale_fill_gradient2(scale.title.mu, limits=c(loval, hival), low=muted("blue"),
    ##                                               high=muted("red"), oob=squish)
    ##         gp.ci95 <- gp.ci95 + scale_fill_gradient2(scale.title.ci95, limits=c(loval, hival), low=muted("blue"),
    ##                                                   high=muted("red"), oob=squish)
    ##     } else {
    ##         gp.mu <- gp.mu + scale_fill_gradient2(scale.title.mu, low=muted("blue"),
    ##                                               high=muted("red"), oob=squish)
    ##         gp.ci95 <- gp.ci95 + scale_fill_gradient2(scale.title.ci95, low=muted("blue"),
    ##                                                   high=muted("red"), oob=squish)
    ##     }
    ## } else if (gradient == 1) {
    ##     if (!is.null(loval)) {
    ##         gp.mu <- gp.mu + scale_fill_gradient(scale.title.mu, limits=c(loval, hival), low="yellow",
    ##                                              high=muted("red"), oob=squish)
    ##         gp.ci95 <- gp.ci95 + scale_fill_gradient(scale.title.ci95, limits=c(loval, hival), low="yellow",
    ##                                                  high=muted("red"), oob=squish)
    ##     } else {
    ##         gp.mu <- gp.mu + scale_fill_gradient(scale.title.mu, low="yellow",
    ##                                              high=muted("red"), oob=squish)
    ##         gp.ci95 <- gp.ci95 + scale_fill_gradient(scale.title.ci95, low="yellow",
    ##                                                  high=muted("red"), oob=squish)
    ##     }
    ## }

    if (!is.null(loval)) {
        ggsave(file.path(outdir, paste(prefix, 'mean.pdf', sep='-')), gp.mu + theme(legend.position="none"), width=3.9, height=7)
        ggsave(file.path(outdir, paste(prefix, 'ci95.pdf', sep='-')), gp.ci95 + theme(legend.position="none"),
               width=3.9, height=7)

        ## Plot legends
        legend <- get_legend(gp.mu)
        pdf(file.path(outdir, paste0(small.prefix, '-mean-legend.pdf')), width=1, height=2.5)
        grid.newpage()
        grid.draw(legend)
        dev.off()

        legend <- get_legend(gp.ci95)
        pdf(file.path(outdir, paste0(small.prefix, '-ci95-legend.pdf')), width=1, height=2.5)
        grid.newpage()
        grid.draw(legend)
        dev.off()
    } else {
        ggsave(file.path(outdir, paste(prefix, 'mean.pdf', sep='-')), gp.mu, width=4.5, height=7)
        ggsave(file.path(outdir, paste(prefix, 'ci95.pdf', sep='-')), gp.ci95, width=4.5, height=7)
    }
}

make.maps.loop <- function(outdir, prefix, results, shp, getstats, scale.title.mu, scale.title.ci95, loval, hival, gradient=2, labels=NULL, hicol=NULL, locol=NULL) {
    bypid <- data.frame()
    for (scn in unique(results$scenario)) {
        for (per in unique(results$period)) {
            print(c(scn, per))

            pidstats <- getstats(subset(results, scenario == scn & period == per))
            shp2 <- shp %>% left_join(pidstats)

            make.maps(outdir, paste(prefix, scn, per, sep='-'), shp2, scale.title.mu, scale.title.ci95, loval, hival, gradient=gradient, labels=labels, small.prefix=prefix, hicol=hicol, locol=locol)

            bypid <- rbind(bypid, cbind(scenario=scn, period=per, pidstats))
        }
    }

    bypid
}
