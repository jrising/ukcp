library(ncdf4)
setwd("~/Open Modeling Group Dropbox/COACCH/")

nc <- nc_open("climate/cmip5/EC-Earth/rcp85/temp_EC-Earth_rcp85_r1i1p1.nc")
time <- ncvar_get(nc, "time")

## Option 0: Hours per year
hoursperyear <- time[13] - time[1] + 6
timestart.0 <- (2006 - 2006) * hoursperyear
timeend.0 <- (2026 - 2006) * hoursperyear

## Option 1: Calculate the number of hours
timestart.1 <- 0
timeend.1 <- (24 * 365.25) * 20

## Option 2: Index the right number of steps
timestart.2 <- time[1]
timeend.2 <- time[12 * 20 + 1]

## Option 3: Label each time

labels <- c()
for (year in 2006:2100)
    for (month in 1:12)
        labels <- c(labels, paste(year, month))

timestart.3 <- time[labels == "2006 1"]
timeend.3 <- time[labels == "2026 1"]

## Option 4: Convert to Dates
dates <- as.Date("2006-01-16") + time / 24

timestart.4 <- time[dates == "2006-01-16"]
timeend.4 <- time[dates == "2026-01-16"]

## Option 5: Calculate hour difference using Dates

daydiff <- as.Date("2026-01-16") - as.Date("2006-01-16")
as.numeric(daydiff) * 24

timestart.5 <- 24 * as.numeric(as.Date("2006-01-16") - as.Date("2006-01-16"))
timeend.5 <- 24 * as.numeric(as.Date("2026-01-16") - as.Date("2006-01-16"))


c(timestart.0, timestart.1, timestart.2, timestart.3, timestart.4, timestart.5)
c(timeend.0, timeend.1, timeend.2, timeend.3, timeend.4, timeend.5)
