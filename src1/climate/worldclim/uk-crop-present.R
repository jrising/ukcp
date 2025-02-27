setwd("~/projects/ukcp/worldclim/present")

library(stringr)
library(raster)

for (filename in list.files(".", ".zip")) {
    system(paste("unzip", filename))

    rrs <- NULL
    for (band in 1:19) {
        tiffile <- gsub(".zip", paste0("_", band, ".tif"), filename)
        if (!file.exists(tiffile))
            tiffile <- gsub(".zip", paste0("_0", band, ".tif"), filename)

        rr <- tryCatch({
            raster(tiffile)
        }, error=function(e) {
            NULL
        })

        if (is.null(rr))
            break

        rr2 <- crop(rr, extent(-10.716305, 2.159672, 49.731803, 61.021214))

        if (is.null(rrs))
            rrs <- rr2
        else
            rrs <- addLayer(rrs, rr2)
    }

    outfile <- gsub(".zip", ".nc", gsub("wc2.1_10m", "uk", filename))
    writeRaster(rrs, outfile, "CDF")

    system("rm *.tif")
}
