setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(ggplot2)
library(cowplot)
library(grid)

source("lib/project.R")

df <- read.csv("climate/worldclim/uk-lmst.csv")

library(PBSmapping)
shp.national.level <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_1.shp")

for (gcm in unique(df$model[-1])) {
    for (per in c('2041-2060', '2081-2100')) {
        temps <- get.grid.temps(gcm, 'ssp370', per)

        gp <- ggplot(temps, aes(x, y)) +
            coord_map(xlim=c(-8, 2), ylim=c(50, 60.5)) + geom_tile(aes(fill=pmax(6, pmin(14, variable)))) +
            geom_polygon(data=shp.national.level, aes(X, Y, group=paste(PID, SID)), size=1, fill="#00000000", colour="#386cb0") +
            theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                               axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
            xlab(NULL) + ylab(NULL) +
            scale_fill_gradient("Annual\nAverage\nTemp. (C)", low='#ffffb2', high='#bd0026', na.value='#FFFFFF', limits=c(6, 14))
        ggsave(file.path("climate/worldclim/figures", paste0(paste(gcm, 'ssp370', per, sep='-'), '.pdf')),
               gp + theme(legend.position="none"), width=3.9, height=7)
    }
}

## Plot legends
legend <- get_legend(gp)
pdf(file.path("climate/worldclim/figures", 'legend.pdf'), width=1, height=2)
grid.newpage()
grid.draw(legend)
dev.off()

## Get temperature range

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/project.R")

events <- get.grid.temps(NA, 'historical', '1970-2000')
names(events)[1:2] <- c('X', 'Y')
events$EID <- 1:nrow(events)

library(PBSmapping)
shp0 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_0.shp")

found <- findPolys(as.EventData(events), shp0)

results <- data.frame()
maxrid <- max(alldraws$run_id)
for (scn in unique(alldraws$scenario)) {
    for (rid in 0:maxrid) {
        print(c(scn, rid))
        warming <- get.warming.eoc(scn, rid)
        pattern <- common.patterns$pattern[common.patterns$run_id == rid]

        subdraws <- subset(alldraws, scenario == scn & run_id == rid)
        subres <- data.frame()
        for (year in 2000:2100) {
            warming.now <- subdraws$value[subdraws$year == year]

            cdiffs <- grid.pattern.cdiff(pattern, scn, year)
            fullcdiff <- warming.now + mean(cdiffs$variable[found$EID], na.rm=T)
            subres <- rbind(subres, data.frame(year, cdiff=fullcdiff, warming=warming.now))
        }

        results <- rbind(results, cbind(scenario=scn, run_id=rid, pattern=pattern, subres))
    }
}

results.base <- subset(results, year >= 1995 & year < 2015) %>% group_by(scenario, run_id) %>% summarize(mu0=mean(cdiff), mu.globe0=mean(warming))

results2 <- results %>% left_join(results.base, by=c('scenario', 'run_id')) %>% group_by(year, scenario) %>%
    summarize(mu=mean(cdiff - mu0), ci5=quantile(cdiff - mu0, .05), ci95=quantile(cdiff - mu0, .95),
              mu.globe=mean(warming), ci5.globe=quantile(warming, .05), ci95.globe=quantile(warming, .95))

library(ggplot2)

ggplot(results2, aes(year, mu)) +
    geom_line(aes(colour=scenario)) + geom_ribbon(aes(ymin=ci5, ymax=ci95, fill=scenario), alpha=.5) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) +
    ylab("Warming in the UK, relative to 1995 - 2005")
ggsave("uk-byyear.pdf", width=6.5, height=4)

subset(results2, year == 2100)

## ggplot(results2, aes(year, mu.globe)) +
##     geom_line(aes(colour=scenario)) + geom_ribbon(aes(ymin=ci5.globe, ymax=ci95.globe, fill=scenario), alpha=.5)
