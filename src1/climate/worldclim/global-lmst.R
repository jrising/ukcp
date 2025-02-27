setwd("~/projects/ukcp/worldclim")

library(stringr)
library(raster)

results <- data.frame()

## Present

filename <- "present/wc2.1_10m_bio.zip"
system(paste("unzip", filename))

tiffile <- gsub("present/", "", gsub(".zip", paste0("_1.tif"), filename))

rr <- raster(tiffile)
lmst <- cellStats(rr, 'mean')

system("rm *.tif")

results <- data.frame(period='1970-2000', model='baseline', scenario='baseline', lmst)

## Future

for (filename in list.files("future", ".+bioc.+zip")) {
    system(paste0("unzip future/", filename))

    for (tiffile in list.files("share", recursive=T)) {
        rr <- raster(paste0("share/", tiffile), band=1)

        lmst <- cellStats(rr, 'mean')

        parts <- str_split(tiffile, "_|\\.")[[1]]

        results <- rbind(results, data.frame(period=parts[length(parts)-1], model=parts[length(parts)-3], scenario=parts[length(parts)-2], lmst))
    }

    system("rm -r share")
}

write.csv(results, "lmst.csv", row.names=F)

pdf <- rbind(data.frame(period='1970-2000', model=results$model[-1], scenario=results$scenario[-1], lmst=results$lmst[1]),
             results[-1, ])
pdf$midyear <- (sapply(str_split(pdf$period, "-"), function(xx) as.numeric(xx[1]) - 1) + sapply(str_split(pdf$period, "-"), function(xx) as.numeric(xx[2]))) / 2

library(ggplot2)

ggplot(pdf, aes(midyear, lmst - results$lmst[1], colour=scenario, group=paste(model, scenario))) +
    geom_line() + theme_bw()


