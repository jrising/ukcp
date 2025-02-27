setwd("C:/Users/ritik/Open Modeling Group Dropbox/UK Economic Risks")
## setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/damagefunc.R")

## Use data from CCRA report

df <- read.csv("channels/flooding/FloodDamages.csv")

ddf <- data.frame() # damage functions
pdf <- data.frame() # points
allla <- list() # region -> la

for (rr in 1:nrow(df)) {
  print(df$Region[rr])
  if (is.na(df$BaseDamage[rr]))
    next

  cdiff <- c(df$X2012[rr], df$X2C207099[rr], df$X4C207099[rr]) - df$X2012[rr]
  damages <- c(0, c(df$X2NCDamages[rr], df$X4NCDamages[rr]) - df$BaseDamage[rr])
  damages.ses <- abs(damages) * ((10.9 - 3.2) / 10.9) # From Appendix G of CCRA report: actual damages within 1 SD of projected

  la <- fit.damages(cdiff, damages, damages.ses)

  ## Record for plotting
  pdf <- rbind(pdf, data.frame(region=df$Region[rr], T=cdiff, mu=damages - mean(la$coeff[, 1]), damages.ses))

  rrddf <- data.frame(region=df$Region[rr], T=seq(0, 6, length.out=100))
  rrddf$mu <- sapply(rrddf$T, function(TT) mean(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2))
  rrddf$ci25 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .25))
  rrddf$ci75 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .75))
  ddf <- rbind(ddf, rrddf)

  allla[[df$Region[rr]]] <- la
}

library(ggplot2)

gp <- ggplot(ddf, aes(T, mu)) +
    facet_wrap(~ region, scales="free_y", ncol=4) +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_point(data=pdf) +
    scale_x_continuous("Difference in temperature from 2012 (C)", expand=c(0, 0)) +
    theme_bw() + ylab("Change in costs due to flooding (£ million)")
ggsave("channels/flooding/flooding-dfs.pdf", width=9.5, height=9.5)

save(allla, file="channels/flooding/flooding-las.RData")
## load("channels/flooding/flooding-las.RData")

source("lib/project.R")

correct.names <- function(polydata, col) {
    ## Correct poly names
    polydata[, col] <- gsub(" Wales", "", polydata[, col])
    polydata[, col] <- gsub(" Ireland", "", polydata[, col])
    polydata[polydata[, col] == "Devon and Cornall", col] <- "Devon & Cornwall"
    polydata[polydata[, col] == "Greater Manchester", col] <- "Gtr Mancs & Ches"
    polydata[polydata[, col] == "Northumberland Durham", col] <- "Northumberland Durham & Tees"
    polydata[polydata[, col] == "Cumbria and Lanchashire", col] <- "Cumbria and Lancashire"
    polydata[polydata[, col] == "Kent and South London", col] <- "Kent & South London"
    polydata[polydata[, col] == "Hertfordshire and North London", col] <- "Herts & North London"
    polydata[polydata[, col] == "Lincolnshire and Northamptonshire", col] <- "Lincs & Northants"
    polydata[polydata[, col] == "Shropshire Herefordshire Worcestershire and Gloucestershire", col] <- "Shrops Heref Worcs & Glos"
    polydata[polydata[, col] == "Derbyshire Nottinghamshire and Leicestershire", col] <- "Derbys Notts and Leics"
    polydata[polydata[, col] == "Cambridgeshire and Bedfordshire", col] <- "Cambs and Bedfordshire"
    polydata[polydata[, col] == "Staffordshire Warwickshire", col] <- "Staffs Warks & West Mids"
    polydata[polydata[, col] == "Essex Norfolk and Suffolk Essex", col] <- "Essex Norfolk & Suffolk"
    polydata[polydata[, col] == "Findhorn, Nairn and Speyside", col] <- "Findhorn, Nairn & Speyside"
    polydata[polydata[, col] == "Ayshire", col] <- "Ayrshire"
    polydata[polydata[, col] == "Tay Estuary and Montrose Basin", col] <- "Tay Estuary & Montrose Basin"
    polydata[polydata[, col] == "Clyde and Loch Lomond", col] <- "Clyde & Loch Lomond"
    polydata[polydata[, col] == "Solent and South Downs", col] <- "Solent & South Downs"
    polydata[polydata[, col] == "Scottish Borders", col] <- "Tweed"
    polydata[polydata[, col] == "Dumfries and Galloway", col] <- "Solway"

    stopifnot(length(polydata[!(polydata[, col] %in% names(allla)), col]) == 0)
    stopifnot(names(allla)[!(names(allla) %in% polydata[, col])] == 0)

    polydata
}

library(PBSmapping)
shp <- importShapefile("regions/ukcp09/ukcp09-merged01.shp")
polydata <- attr(shp, 'PolyData')
polydata <- correct.names(polydata, 'Name')

grid2region <- get.grid2region.shape(shp, polydata$Name)

results <- grid.project(allla, 2012, 2013, grid2region=grid2region)

save(results, file="channels/flooding/flooding-project.RData")
## load("channels/flooding/flooding-project.RData")

## At reginonal level
aggregate.cdiff <- function(cdiffs) {
    cbind(cdiffs[!is.na(cdiffs$variable),], region=grid2region) %>%
        group_by(region) %>% summarize(x=mean(x), y=mean(y),
                                       variable=mean(variable))
    ##stopifnot(all(subres$region == sort(unique(grid2region))))
    ##subres
}

# Determine x, y for each region
regionxy <- aggregate.cdiff(grid.pattern.cdiff('MIROC6', 'ssp126', 2020))
regionxy$x02 <- round(regionxy$x, 2)
regionxy$y02 <- round(regionxy$y, 2)
regionxy <- regionxy[, c('region', 'x02', 'y02')]

results.region <- grid.project(allla, 2012, 2013, grid2region=sort(unique(grid2region)),
                               aggregate.cdiff=aggregate.cdiff)
results.region <- results.region %>% mutate(x02=round(x, 2), y02=round(y, 2)) %>% left_join(regionxy) %>% dplyr::select(-c(x02, y02))

save(results, results.region, file="channels/flooding/flooding-project.RData")

source("lib/report.R", chdir=T)
results.uk <- results.region %>% group_by(scenario, run_id, period) %>% summarize(damage=sum(damage))

source("lib/constants.R")
results.uk$percent <- 100 * results.uk$damage * 1.24570131 / gdp.2012.gbp

tbl <- make.table(results.uk, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

## Aggregate gridded to ADM3

source("lib/aggregate.R")
byadm3 <- aggregate.gridded.county(results, 'sum')

save(results, results.region, byadm3, file="channels/flooding/flooding-project.RData")

## Force-match ADM3 to regional

shp.reg <- importShapefile("regions/ukcp09/ukcp09-merged01.shp")
polydata.adm3 <- reg2adm3(shp.reg, 'Name') # adds geo_region
polydata.adm3 <- correct.names(polydata.adm3, 'geo_region')

byadm3$damage <- as.numeric(byadm3$tot)
byadm3$run_id <- as.numeric(byadm3$run_id)
byadm3.match <- full.force.match(byadm3, results.region, polydata.adm3)

save(byadm3.match, file="channels/flooding/flooding-final.RData")


