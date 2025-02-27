setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/constants.R")

library(readxl)

## Load GDHI per head of population at current basic prices
df <- read_excel("socioeconomics/regionalgrossdisposablehouseholdincomeallitlregions.xls", sheet=4, skip=1)
df$NUTS_ID <- gsub('TL', 'UK', df$`ITL code`)

## Figure out scaling factor between this and GDP p.c.
scale <- gdp.2015.gbp / (pop.2015 * df$`2015`[1])

library(PBSmapping)

shp.nuts3 <- importShapefile("regions/ref-nuts-2016-10m.shp/NUTS_RG_10M_2016_4326_LEVL_3.shp/NUTS_RG_10M_2016_4326_LEVL_3.shp")
nuts3info <- attr(shp.nuts3, 'PolyData')

shp.adm3 <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3-withattr.shp")
adm3info <- attr(shp.adm3, 'PolyData')

cent.adm3 <- calcCentroid(shp.adm3, rollup=3)
cent.adm3$EID <- 1:nrow(cent.adm3)

events <- as.EventData(cent.adm3, projection=1)
found <- findPolys(events, shp.nuts3)

library(dplyr)

adm3info2 <- adm3info %>% left_join(cent.adm3, by=c('PID')) %>% left_join(found, by=c('EID'), suffix=c('', '.nuts'))
adm3info2$NUTS_ID <- nuts3info$NUTS_ID[adm3info2$PID.nuts]
adm3info2$NUTS_NAME <- nuts3info$NUTS_NAME[adm3info2$PID.nuts]

adm3info3 <- adm3info2 %>% left_join(df[, c('NUTS_ID', 'Region name', '2015')], by=c('NUTS_ID'))
adm3info4 <- adm3info3 %>% group_by(PID) %>% summarize(county=NAME_3[1], pop=popsum[1], GDHI=mean(`2015`, na.rm=T))

adm3info4$GDHI[is.na(adm3info4$GDHI)] <- df$`2015`[1]
adm3info4$GDPpc <- round(adm3info4$GDHI * scale)

write.csv(adm3info4, "socioeconomics/adm3inc.csv", row.names=F)

shp.adm32 <- shp.adm3 %>% left_join(adm3info4)

shp.national.level <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_1.shp")

library(ggplot2)
gp <- ggplot(shp.adm32, aes(X, Y, group=paste(PID, SID), fill=GDHI)) +
    coord_map(xlim=c(-8, 2), ylim=c(50, 60.5)) +
    geom_polygon() +
    geom_polygon(data=shp.national.level, size=.1, fill="#00000000", colour="#808080") +
    theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                       axis.text.y=element_blank(), axis.ticks.y=element_blank()) + xlab(NULL) + ylab(NULL) +
    scale_fill_continuous("GDHI per head (£)", trans='log10')
ggsave("socioeconomics/gdhi.pdf", gp, width=5.5, height=7)
