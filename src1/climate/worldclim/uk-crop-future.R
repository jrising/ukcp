setwd("~/projects/ukcp/worldclim/future")

library(stringr)
library(raster)

for (filename in list.files(".", ".zip")) {
    system(paste("unzip", filename))

    for (tiffile in list.files("share", recursive=T)) {
        rrs <- NULL
        for (band in 1:19) {
            rr <- tryCatch({
                raster(paste0("share/", tiffile), band=band)
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

        parts <- str_split(tiffile, "/")[[1]]
        outfile <- gsub(".tif", ".nc", gsub("wc2.1_10m", "uk", parts[length(parts)]))

        writeRaster(rrs, outfile, "CDF")
    }

    system("rm -r share")
}
