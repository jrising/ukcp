library(dplyr)
library(ncdf4)
library(raster)

setwd("~/Open Modeling Group Dropbox/COACCH/climate")

source("~/Open Modeling Group Dropbox/UK Economic Risks/lib/damagefunc.R")

df <- read.csv("../COACCH_ICES-CMCC_data/impact-ses.csv")

ncr <-nc_open("cmip5/IPSL-CM5A-MR/historical/tas_Amon_IPSL-CM5A-MR_historical_r3i1p1_185001-200512-aggregated.nc")

lb.gcm.rcp <- read.csv("cmip5/Combine-GCM/energy-gcm-rcp.csv") #rcp85 
lb.gcm.hist <-read.csv("cmip5/Combine-GCM/energy-gcm-hist.csv") #historical 

timer <- lb.gcm.rcp$yyyymm
tsavg.r <- lb.gcm.rcp$tsavg
region.r <- lb.gcm.rcp$NUTS_ID
date.r <- as.Date(paste0(timer, "-15"))

timeh <- lb.gcm.hist$yyyymm
tsavg.h <- lb.gcm.hist$tsavg
region.h <- lb.gcm.hist$NUTS_ID

nuts <-ncvar_get(ncr, "NUTS_ID")
ukrrs <- grep("^UK..$", nuts)

ddf <- data.frame() # damage functions
pdf <- data.frame() # points
allla <- list() # region -> la

for (rr in ukrrs){
  cdiffs <-c()
  basekel<- mean(tsavg.h[region.h == nuts[rr]])
  
  for (year in seq(2015, 2070, by=5)) {
    timestart = as.Date(paste0(year, "-07-01"))-360*2.5
    timeend = as.Date(paste0(year, "-07-01"))+360*2.5
    kel<- mean(tsavg.r[region.r == nuts[rr] & timestart <= date.r & timeend > date.r] )
    cdiffs <- c(cdiffs, kel-basekel)
  }
  
  #  for (item in unique(df$impact)) {
  subdf <- df[df$region == nuts[rr] & df$impact == "Labour_Productivity",]
  if (nrow(subdf)==0){
    print(paste("Skipping", nuts[rr], "Labour_Productivity"))
    next 
  }
  print(paste("Processing", nuts[rr], "Labour_Productivity"))
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
  theme_bw() + ylab("Damages to labor productivity (£ million)")
ggsave("../channels/Labor/labor-coacch-df.pdf", width=9.5, height=9.5)

save(allla, file="../channels/Labor/labor-la.RData")


