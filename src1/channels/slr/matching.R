setwd("~/projects/ukcp/slr")

library(PBSmapping)

segments <- importShapefile("diva_published/data/gis/cls.shp")
polydata <- attr(segments, "PolyData")
gbrsegs <- subset(segments, PID %in% polydata$PID[polydata$countryid == "GBR"])

admins <- importShapefile("../gadm36_GBR_shp/gadm36_GBR_3.shp")
adminfo <- attr(admins, "PolyData")

source("~/projects/research-common/R/distance.R")

gbrsegs$adminPID <- NA
for (ii in 1:nrow(gbrsegs)) {
    print(ii / nrow(gbrsegs))
    dists <- gcd.slc(gbrsegs$X[ii], gbrsegs$Y[ii], admins$X, admins$Y)
    gbrsegs$adminPID[ii] <- admins$PID[which.min(dists)]
}

matching <- data.frame()
for (adminPID in unique(gbrsegs$adminPID)) {
    shares <- table(gbrsegs$PID[gbrsegs$adminPID == adminPID])
    matching <- rbind(matching, data.frame(Country=adminfo$NAME_1[adminPID], ADM2=adminfo$NAME_2[adminPID], ADM3=adminfo$NAME_3[adminPID],
                                           segment=polydata$locationid[as.numeric(names(shares))], share=as.numeric(shares / sum(shares))))
}

write.csv(matching, "matching.csv", row.names=F)
