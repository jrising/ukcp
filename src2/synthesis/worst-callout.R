setwd("~/Dropbox/COACCH")
load("synthesis/alldamage-adm3-dblcnt.RData")

alldamage2 <- alldamage.adm3.dblcnt

source("../UK Economic Risks/lib/report.R")
library(dplyr)

shp.adm3 <- importShapefile("../UK Economic Risks/regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

alldamage3 <- alldamage2 %>% left_join(shp.adm3.polydata[, c('PID', 'NAME_3')], by=c('county'='NAME_3'))

alldamage3$sectormax <- pmax(alldamage3$fracgdp.slr, alldamage3$fracgdp.mort,
                             alldamage3$fracgdp.labor, alldamage3$fracgdp.crop, alldamage3$fracgdp.dght,
                             alldamage3$fracgdp.esup + alldamage3$fracgdp.ener, alldamage3$fracgdp.rivr,
                             alldamage3$fracgdp.milk + alldamage3$fracgdp.lamb + alldamage3$fracgdp.algal + alldamage3$fracgdp.marine,
                             alldamage3$fracgdp.bios + alldamage3$fracgdp.forest, alldamage3$missing)
alldamage3$bigsector <- ifelse(alldamage3$fracgdp.slr == alldamage3$sectormax, 'Coastal impacts',
                        ifelse(alldamage3$fracgdp.milk + alldamage3$fracgdp.lamb + alldamage3$fracgdp.algal + alldamage3$fracgdp.marine == alldamage3$sectormax, 'Livestock & fisheries',
                        ifelse(alldamage3$fracgdp.mort == alldamage3$sectormax, 'Health',
                        ifelse(alldamage3$fracgdp.labor == alldamage3$sectormax, 'Labour productivity',
                        ifelse(alldamage3$fracgdp.crop == alldamage3$sectormax, 'Agriculture',
                        ifelse(alldamage3$fracgdp.dght == alldamage3$sectormax, 'Droughts',
                        ifelse(alldamage3$fracgdp.esup + alldamage3$fracgdp.ener == alldamage3$sectormax, 'Energy supply & demand',
                        ifelse(alldamage3$fracgdp.rivr == alldamage3$sectormax, 'River floods',
                        ifelse(alldamage3$fracgdp.bios + alldamage3$fracgdp.forest == alldamage3$sectormax, 'Ecosystems',
                        ifelse(alldamage3$missing == alldamage3$sectormax, "Missing non-catastrophic", 'unknown'))))))))))

library(pracma)
channel.order <- c('Droughts', 'River floods', 'Agriculture', 'Livestock & fisheries',
                   'Ecosystems', 'Energy supply & demand', 'Labour productivity', 'Health',
                   'Coastal impacts', 'Trade effects', 'Missing non-catastrophic', 'Catastrophic risk')
alldamage3$bigsector <- factor(alldamage3$bigsector, channel.order)
alldamage3$secnum <- as.numeric(alldamage3$bigsector)

pdf <- alldamage3 %>% group_by(scenario, period, PID) %>% summarize(secnum=Mode(secnum))
pdf$bigsector <- channel.order[pdf$secnum]

table(pdf$bigsector) # Make sure we didn't add missing in the map

finder <- subset(alldamage3, period == '2081-2100' & scenario == 'ssp370') %>% group_by(county) %>% summarize(drought=mean(bigsector == 'Droughts'), ecosys=mean(bigsector == 'Ecosystems'))
finder[which.max(finder$drought),]
finder[which.max(finder$ecosys),]

allpdf <- data.frame()
for (cty in c('London',
              'Birmingham', # Labour Productivity
              #'Leeds', # Labour Productivity
              'Glasgow', # Energy supply & demand
              'Manchester', # Energy supply & demand
              'Canterbury', # Coastal
              'Fenland', # Agriculture
              #'Mid Suffolk', # Agriculture
              'Banbridge', # Livestock & fisheries
              'West Lothian' # Coastal
              )) {
    print(cty)
    pdf <- subset(alldamage3, county == cty & period == '2081-2100' & scenario == 'ssp370') %>% group_by(bigsector) %>% summarize(count=length(bigsector))
    ggplot(pdf, aes(x="", y=count, fill=bigsector)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() +
        scale_fill_manual("High risk channel:", breaks=rev(channel.order),
                          values=rev(c('#b15928', '#a6cee3', '#33a02c', '#ffff99',
                                       '#b2df8a', '#e31a1c', '#fdbf6f', '#fb9a99',
                                       '#1f78b4', '#ff7f00', '#808080', '#cab2d6'))) +
        theme(legend.position="none") 
    ggsave(paste0("synthesis/maps/worst-callout-", cty, ".pdf"), width=5, height=5)

    allpdf <- rbind(allpdf, pdf)
}

allpdf %>% group_by(bigsector) %>% summarize(count=sum(count))
