library(ncdf4)
library(abind)
library(reshape2)

basedir <- '/shares/gcp/outputs/growth/impacts-sandpuppy'

outputs <- data.frame(cluster=c(), region=c(), year=c(), loss=c())
for (rcp in c('rcp45', 'rcp85')) {

allclsloss <- NULL
for (batch in 1:38) {
    alltotloss <- NULL
    for (subpath in list.files(path=file.path(basedir, paste0("batch", batch)),
                               pattern="burkeetal.nc4", recursive=T)) {
        if (length(grep(rcp, subpath)) == 0)
	    next
	    
        print(file.path(batch, subpath))

        nc <- nc_open(file.path(basedir, paste0("batch", batch), subpath))
        regions <- ncvar_get(nc, 'regions')
        rebased <- ncvar_get(nc, 'rebased')
        year <- ncvar_get(nc, 'year')

        totloss <- 0 * rebased
        for (rr in 1:length(regions)) {
            totloss[rr, ] <- cumsum(rebased[rr, ])
	    totloss[rr, ] <- totloss[rr, ] - totloss[rr, 25] # relative to 2005
	}

        totloss <- totloss[, 1:119] # drop 120 if available
        dim(totloss) <- c(1, dim(totloss))

        if (is.null(alltotloss))
            alltotloss <- totloss
        else
            alltotloss <- abind(alltotloss, totloss, along=1)

        nc_close(nc)
    }

    forclust <- alltotloss[, , dim(alltotloss)[3]] # grab last value
    clusters <- hclust(dist(forclust))
    clusterCut <- cutree(clusters, 19)

    for (cc in unique(clusterCut)) {
        values <- apply(alltotloss[clusterCut == cc, , , drop=F], c(2, 3), mean)
        dim(values) <- c(1, dim(values))
        if (is.null(allclsloss))
            allclsloss <- values
        else
            allclsloss <- abind(allclsloss, values, along=1)
    }
}

print(dim(allclsloss))

forclust <- allclsloss[, , dim(allclsloss)[3]] # grab last value
clusters <- hclust(dist(forclust))
clusterCut <- cutree(clusters, 19)

for (cc in unique(clusterCut)) {
    values <- apply(allclsloss[clusterCut == cc, , , drop=F], c(2, 3), mean)
    rows <- melt(values)
    names(rows) <- c('region', 'year', 'loss')
    rows$region <- regions[rows$region]
    rows$year <- year[rows$year]
    outputs <- rbind(outputs, cbind(rcp, cluster=cc, rows))
}
}

write.csv(outputs, "burkeetal.csv", row.names=F)


