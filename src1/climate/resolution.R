## HadGEM3 (MetOfce—Hadley Centre (MOHC), Roberts et al. (2019))
## Long. res. [°]: 0.35
## Lat. res. [°]: 0.23

setwd("~/projects/ukcp")

library(PBSmapping)
library(raster)
library(ggplot2)

shp0 <- importShapefile("gadm36_GBR_shp/gadm36_GBR_0.shp")
shp2 <- importShapefile("gadm36_GBR_shp/gadm36_GBR_2.shp")

rr <- raster("uk-bio_1.nc")
rrdf <- as.data.frame(rr, xy=T)

## lonlines <- data.frame(x=c(seq(0, 3, by=.35), seq(0, -11, by=-.35)[-1]))
## latlines <- data.frame(y=seq(0, 65, by=.23)[seq(0, 62, by=.23) > 49])

lonlines <- data.frame(x=c(seq(0, 3, by=10 / 60), seq(0, -11, by=-10 / 60)[-1]))
latlines <- data.frame(y=seq(0, 65, by=10 / 60)[seq(0, 62, by=10 / 60) > 49])

ggplot() +
    geom_raster(data=rrdf, aes(x, y, fill=wc2.1_30s_bio_1)) +
    geom_polygon(data=shp2, aes(X, Y, group=paste(PID, SID)), colour='#000000', fill="00000000", size=.1) +
geom_polygon(data=shp0, aes(X, Y, group=paste(PID, SID)), colour='#000000', fill="00000000") +
geom_hline(data=latlines, aes(yintercept=y), colour="#FFFFFF", size=.2) + geom_vline(data=lonlines, aes(xintercept=x), colour="#FFFFFF", size=.2) +
    theme_bw() + xlab(NULL) + ylab(NULL) + coord_fixed() +
    scale_x_continuous(limits=c(-10.716305, 2.159672), expand=c(0, 0)) + scale_y_continuous(limits=c(49.731803, 61.021214), expand=c(0, 0)) +
    scale_fill_gradientn(name="Annual Mean\nTemp. (C)", colours=rev(rainbow(4)))

## Bioclim data from https://www.worldclim.org/data/worldclim21.html
