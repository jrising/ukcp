setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(PBSmapping)
library(ggplot2)
library(scales)
library(raster)

shp0 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_0.shp")
shp1 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_1.shp")
shp3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")

rr <- raster("climate/worldclim/uk-present/uk_bio.nc")
lonlines <- data.frame(x=unique(c(coordinates(rr)[, 1] - 0.1666667 / 2, coordinates(rr)[, 1] + 0.1666667 / 2)))
latlines <- data.frame(y=unique(c(coordinates(rr)[, 2] - 0.1666667 / 2, coordinates(rr)[, 2] + 0.1666667 / 2)))

gp <- ggplot(shp3, aes(X, Y, group=paste(PID, SID))) +
    coord_map(xlim=c(-8, 2), ylim=c(50, 60.5)) + geom_polygon(colour="#ffffff", size=.2) +
    geom_polygon(data=shp0, size=.4, fill="#00000000", colour='#808080') +
    geom_polygon(data=shp1, aes(fill=factor(PID)), alpha=.5) +
    geom_hline(data=latlines, aes(yintercept=y), colour="#000000", size=.2, alpha=.15) +
    geom_vline(data=lonlines, aes(xintercept=x), colour="#000000", size=.2, alpha=.15) +
    theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                       axis.text.y=element_blank(), axis.ticks.y=element_blank()) + xlab(NULL) + ylab(NULL) +
    theme(legend.position="none")
ggsave("regions/figures/adm3.pdf", gp, width=3.9, height=7)

shp3x <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.shp")
polydata <- attr(shp3x, 'PolyData')

area.ratio <- 476.4 / 0.047535493 # based on area of Borough of Bedford (wikipedia)
median(polydata$area * area.ratio)
quantile(polydata$area * area.ratio)

0.1666667 * 0.1666667 * 111 * 65 # based on 54 N
