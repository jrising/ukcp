setwd("~/Dropbox/COACCH")
load("synthesis/alldamage-adm3-dblcnt.RData")

df <- alldamage.adm3.dblcnt
## Factor analysis
## Each row (county, period, scenario, MC) is a linear sum of sum group of factors

cols <- paste0('fracgdp.', c('dght', 'marine', 'slr', 'mort', 'labor', 'milk', 'lamb', 'bios', 'bios2', 'forest', 'ener', 'esup', 'crop', 'algal', 'rivr'))
## Categorize each county into 10 groups
groups <- data.frame(county=unique(df$county))
groups$set <- sample(10, nrow(groups), replace=T)

library(reshape2)
runids <- sample(1:max(df$run_id), 100) # always drop run_id 0

df2 <- melt(df[df$run_id %in% runids & (df$period != '2011-2030' | df$scenario == 'ssp370'), c('period', 'scenario', 'run_id', 'county', cols)], c('period', 'scenario', 'run_id', 'county'))
df3 <- dcast(df2, county ~ ..., fun.aggregate=mean, value.var='value')

## cl <- kmeans(df3[, -1], 10)

library(mclust)

results <- data.frame()
for (kk in 1:30) {
    print(kk)

    cl.all <- kmeans(df3[, -1], kk)
    ss.all <- cl.all$tot.withinss

    ss.out <- 0
    aris <- c()
    for (gg in 1:10) {
        cl <- kmeans(df3[!(df3$county %in% groups$county[groups$set == gg]), -1], kk)

        mat.out <- as.matrix(df3[df3$county %in% groups$county[groups$set == gg], -1])
        best.dists <- rep(Inf, nrow(mat.out))
        best.clusters <- rep(NA, nrow(mat.out))
        for (cc in 1:kk) {
            dists <- sqrt(rowSums((mat.out - cl$centers[cc,])^2))
            best.clusters[best.dists > dists] <- cc
            best.dists <- pmin(best.dists, dists)
        }

        ss.out <- ss.out + sum(best.dists^2)
        ari <- adjustedRandIndex(cl.all$cluster[df3$county %in% groups$county[groups$set == gg]],
                                 best.clusters)
        aris <- c(aris, ari)
    }

    results <- rbind(results, data.frame(kk, ss.all, ss.out, ari=mean(aris)))
}

results2 <- melt(results, 'kk')

ggplot(subset(results2, variable != 'ari'), aes(kk, value, colour=variable)) +
    geom_line() + theme_bw() +
    xlab("Number of clusters") + ylab("Sum of squared error")


## Call it 9 clusters
cl <- kmeans(df3[, -1], 9)

source("../UK Economic Risks/lib/report.R", chdir=T)
shp.adm3 <- importShapefile("../UK Economic Risks/regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

library(dplyr)
pdf <- data.frame(county=df3$county, cluster=cl$cluster) %>% left_join(shp.adm3.polydata[, c('PID', 'NAME_3')], by=c('county'='NAME_3'))

shp2 <- shp.adm3 %>% left_join(pdf)

gp <- ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    coord_map(xlim=c(-8, 2), ylim=c(50, 60.5)) + geom_polygon(aes(fill=factor(cluster))) +
    geom_polygon(data=shp.national.level, size=.1, fill="#00000000", colour="#808080") +
    theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                       axis.text.y=element_blank(), axis.ticks.y=element_blank()) + xlab(NULL) + ylab(NULL)
ggsave('synthesis/clusters.pdf', gp, width=4.5, height=7)


cldf <- melt(cl$centers)


if (F) {

fdf <- df[, cols]

library(psych)

results <- data.frame()
for (kk in 1:10) {
    print(kk)

    facts <- factanal(fdf, factors=kk)

    scores <- factor.scores(fdf, facts)
    epsilon.all <- fdf - scores$scores %*% t(facts$loadings)

    sse.out <- 0
    for (ss in 1:10) {
        fdf.trn <- fdf[df$county %in% groups$county[groups$set != ss],]
        facts <- factanal(fdf.trn, factors=kk)

        fdf.out <- fdf[df$county %in% groups$county[groups$set == ss],]
        scores <- factor.scores(fdf.out, facts)
        epsilon.out <- fdf.out - scores$scores %*% t(facts$loadings)
        
        sse.out <- sse.out + sum(as.matrix(epsilon.out)^2)
    }

    results <- rbind(results, data.frame(kk, sse.all=sum(as.matrix(epsilon.all)), sse.out))
}

ggplot(results, aes(kk)) +
    geom_point(aes(y=sse.all, colour='Training')) +
    geom_point(aes(y=sse.out, colour='Test'))

}
