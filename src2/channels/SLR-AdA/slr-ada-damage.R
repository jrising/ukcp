library(dplyr)
library(ncdf4)
library(raster)

setwd("~/Open Modeling Group Dropbox/COACCH/climate")

source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/damagefunc.R")

df <- read.csv("../COACCH_ICES-CMCC_data/impact-ses.csv")
ncr <- nc_open("~/Open Modeling Group Dropbox/COACCH/climate/cmip5/HadGEM2-ES/ts-HadGEM2-rcp-aggregated.nc")
nch <- nc_open("~/Open Modeling Group Dropbox/COACCH/climate/cmip5/HadGEM2-ES/ts-HadGEM2-hist-aggregated.nc")
tash <- ncvar_get(nch, "tas")
timeh <- ncvar_get(nch, "time")

tasr <- ncvar_get(ncr, "tas")
timer <- ncvar_get(ncr, "time")

nuts <-ncvar_get(ncr, "NUTS_ID")
ukrrs <- grep("^UK..$", nuts)

ddf <- data.frame() # damage functions
pdf <- data.frame() # points
allla <- list() # region -> la

for (rr in ukrrs){
  cdiffs <-c()
  basestart = (1860 - 1859)*360 - 330
  baseend = (1880 - 1859)*360 - 330
  basekel<- mean(tash[timeh >= basestart & timeh < baseend,rr])
  
  for (year in seq(2015, 2070, by=5)) {
    timestart = (year -2.5 - 2005)*360 - 330
    timeend = (year +2.5 - 2005)*360 - 330
    kel<- mean(tasr[timer >= timestart & timer < timeend,rr])
    cdiffs <- c(cdiffs, kel-basekel)
  }
  
  #  for (item in unique(df$impact)) {
  subdf <- df[df$region == nuts[rr] & df$impact == "Sea_level_rise_AdA",]
  if (nrow(subdf)==0){
    print(paste("Skipping", nuts[rr], "Sea_level_rise_AdA"))
    next 
  }
  print(paste("Processing", nuts[rr], "Sea_level_rise_AdA"))
  la <- fit.damages(cdiffs, subdf$mu, subdf$se)
  
  #  }
  #For plotting
  pdf <- rbind(pdf, data.frame(region=nuts[rr], T=cdiffs, mu=subdf$mu - mean(la$coeff[, 1]), subdf$se))
  
  rrddf <- data.frame(region=nuts[rr], T=seq(0, 6, length.out=100))
  rrddf$mu <- sapply(rrddf$T, function(TT) mean(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2))
  rrddf$ci25 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .25))
  rrddf$ci75 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .75))
  ddf <- rbind(ddf, rrddf)
  
  allla[[nuts[rr]]] <- la
  
}

library(ggplot2)

gp <- ggplot(ddf, aes(T, -mu)) +
  facet_wrap(~ region, scales="free_y", ncol=4) +
  geom_line() + geom_ribbon(aes(ymin=-ci25, ymax=-ci75), alpha=.5) +
  geom_point(data=pdf) +
  scale_x_continuous("Difference in temperature from 1869 (C)", expand=c(0, 0)) +
  theme_bw() + ylab("Damages to sea level rise_AdA (£ million)")
ggsave("../channels/SLR/SLR-ada-coacch-df.pdf", width=9.5, height=9.5)
