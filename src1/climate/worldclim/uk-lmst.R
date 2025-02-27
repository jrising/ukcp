setwd("~/projects/ukcp/worldclim")

library(stringr)
library(raster)

results <- data.frame()

## Present

rr <- raster("uk-present/uk_bio.nc", band=1)
lmst <- cellStats(rr, 'mean')

results <- data.frame(period='1970-2000', model='baseline', scenario='baseline', lmst)

## Future

for (filename in list.files("uk-future", ".+bioc.+nc")) {
    rr <- raster(paste0("uk-future/", filename), band=1)

    lmst <- cellStats(rr, 'mean')

    parts <- str_split(filename, "_|\\.")[[1]]

    results <- rbind(results, data.frame(period=parts[length(parts)-1], model=parts[length(parts)-3], scenario=parts[length(parts)-2], lmst))
}

write.csv(results, "uk-lmst.csv", row.names=F)
