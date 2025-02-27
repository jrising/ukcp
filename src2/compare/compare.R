setwd("~/Dropbox/COACCH/compare")

library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)

df1 <- read.csv("results.csv")

## Add Nordhaus missing
df1.tot <- rbind(df1[df1$Channel != "Missing" & df1$Contribute == '', -which(names(df1) == 'Contribute')],
                 subset(df1, Contribute != '') %>% group_by(Channel, Years) %>%
                   summarize(Method=ifelse(all(Method == Method[1]), Method[1], ifelse(Channel[1] %in%
                             c('Ecosystems', 'Fisheries'), 'Process-based', 'Aggregate')),
                             SSP370.q05=sum(SSP370.q05, na.rm=T), SSP370.mean=sum(SSP370.mean, na.rm=T),
                             SSP370.q95=sum(SSP370.q95, na.rm=T), SSP126.q05=sum(SSP126.q05, na.rm=T),
                             SSP126.mean=sum(SSP126.mean, na.rm=T), SSP126.q95=sum(SSP126.q95, na.rm=T),
                             diff.q05=sum(diff.q05, na.rm=T), diff.mean=sum(diff.mean, na.rm=T),
                             diff.q95=sum(diff.q95, na.rm=T), Source='Aggregate'))

df1.mctot <- data.frame()
for (ii in 1:1000) {
    print(ii)
    df1.mc <- data.frame()
    for (chan in unique(df1.tot$Channel)) {
        if (chan == 'Missing')
            next
        chosen <- subset(df1.tot, Channel == chan & Source == sample(unique(df1.tot$Source[df1.tot$Channel == chan]), 1))
        if (chan == 'Droughts') {
            ## Double-count in droughts is 47% in 1981-2010 (1995) and 38% in 2100
            doublecount <- 47 + (38 - 47) * (c(2020, 2050, 2090) - 1995) / (2100 - 1995)
            chosen[1, -(1:4)] <- chosen[1, -(1:4)] * (1 - doublecount[1] / 100)
            chosen[2, -(1:4)] <- chosen[1, -(1:4)] * (1 - doublecount[2] / 100)
            chosen[3, -(1:4)] <- chosen[1, -(1:4)] * (1 - doublecount[3] / 100)
        }

        df1.mc <- rbind(df1.mc, chosen)
    }
    df1.mctot.one <- df1.mc %>% group_by(Years) %>% summarize(SSP370.q05=sum(SSP370.q05, na.rm=T),
                                                              SSP370.mean=sum(SSP370.mean, na.rm=T),
                                                              SSP370.q95=sum(SSP370.q95, na.rm=T),
                                                              SSP126.q05=sum(SSP126.q05, na.rm=T),
                                                              SSP126.mean=sum(SSP126.mean, na.rm=T),
                                                              SSP126.q95=sum(SSP126.q95, na.rm=T),
                                                              diff.q05=sum(diff.q05, na.rm=T),
                                                              diff.mean=sum(diff.mean, na.rm=T),
                                                              diff.q95=sum(diff.q95, na.rm=T))
    df1.mctot <- rbind(df1.mctot, df1.mctot.one)
}

miss.nordhaus <- df1.mctot %>% group_by(Years) %>%
  summarize(SSP370.q05=.25*median(SSP370.q05), SSP370.mean=.25*mean(SSP370.mean),
            SSP370.q95=.25*median(SSP370.q95), SSP126.q05=.25*median(SSP126.q05),
            SSP126.mean=.25*mean(SSP126.mean), SSP126.q95=.25*median(SSP126.q95),
            diff.q05=.25*median(diff.q05), diff.mean=.25*mean(diff.mean), diff.q95=.25*median(diff.q95))
write.csv(miss.nordhaus, "missing-nordhaus.csv", row.names=F)

df1.full <- rbind(df1,
                  cbind(miss.nordhaus, Channel="Missing", Source="Nordhaus (2013)",
                        Contribute="", Method="Expert elicitation"))

df2 <- read_excel("../CCRA3/UK All Risks.xlsx", skip=1)
df2 <- subset(df2, !is.na(Code) & !(Code %in% c('H1a', 'H1b', 'H3a', 'NE9'))) # keep H1 and H2 since have bounds

df2 <- subset(df2, (Code %in% c('NE6a', 'I1', 'B2', 'H3b', 'H4', 'I3', 'I11', 'N10', 'NE17', 'B3', 'H10a', 'I8', 'H6a', 'H6b',
                                'I6', 'I9', 'I10', # COACCH only does wind and hydro, but the CCRA categories overlap these.
                                'NE6b', 'N11', 'B1', 'H3a', 'I2', 'I4', 'B5', 'H11', 'NE5', 'NE18',
                                'NE14', 'NE15',
                                'H1', 'H2', 'ID8', 'B6', 'I5', 'I12'))) # changed H1a to H1, H2
df2 <- df2[!(substring(df2$Code, 1, 1) %in% c('A', 'C')), c(2:6, 12:29)]
df2 <- df2 %>% mutate_at(14:21, as.double)
df2$Independents[df2$Independents %in% c("Biodiversity", "Landscape", "Forestry")] <- "Ecosystems"
df2$Independents[df2$Independents %in% c("Inland Fisheries", "Marine Fisheries")] <- "Fisheries"
df2$Independents[is.na(df2$Independents)] <- "Mortality" # actually all health

sort(unique(df1.full$Channel))
sort(unique(df2$Independents))

df3.lo <- melt(df2[, c(1:9, 22)], names(df2)[c(1:5, 22)], value.name='loval')
df3.lo$loval <- as.numeric(df3.lo$loval)
df3.lo$variable2 <- substring(df3.lo$variable, 1, nchar(as.character(df3.lo$variable)) - 2)
df3.hi <- melt(df2[, c(1:5, 10:13, 22)], names(df2)[c(1:5, 22)], value.name='hival')
df3.hi$hival <- as.numeric(df3.hi$hival)
df3.hi$variable2 <- substring(df3.hi$variable, 1, nchar(as.character(df3.hi$variable)) - 2)

df3 <- df3.lo %>% left_join(df3.hi, by=c(names(df2)[c(1:5, 22)], 'variable2'))

growrate <- mean(diff(log(c(1314.245, 1329.186, 1350.140, 1382.128, 1418.235, 1455.828, 1495.583))))
gdp.2015.gbp <- 2089276e6 # GBP, from https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2
gdp.proj <- gdp.2015.gbp * exp(growrate * (c(2020, 2050, 2080) - 2015))

df3$Years <- ifelse(df3$variable2 == '2020s...', '2011-2030', ifelse(df3$variable2 == '2050s...', '2041-2060', '2081-2100'))
df4 <- df3 %>% left_join(data.frame(gdp.proj, Years=c('2011-2030', '2041-2060', '2081-2100')))

## ## Sum across indepedents
## df5 <- df4 %>% group_by(Independents, variable2) %>% summarize(loval=sum(loval, na.rm=T), hival=sum(hival, na.rm=T))

df <- rbind(cbind(df1.full[, 1:5], Synthesis='This', Scenario='Baseline',
                  loval=df1.full$SSP370.q05, muval=df1.full$SSP370.mean, hival=df1.full$SSP370.q95),
            cbind(df1.full[df1.full$Years == '2081-2100', 1:5], Synthesis='This', Scenario='High mitigation',
                  loval=df1.full$SSP126.q05[df1.full$Years == '2081-2100'],
                  muval=df1.full$SSP126.mean[df1.full$Years == '2081-2100'],
                  hival=df1.full$SSP126.q95[df1.full$Years == '2081-2100']),
            data.frame(Channel=df4$Independents, Source='CCRA3', Contribute=df4$Risk...3,
                       Method='Expert elicitation', Years=df4$Years, Synthesis='CCRA3',
                       Scenario=ifelse(df4$variable2 == "2080s, 2°C...", 'High mitigation', 'Baseline'),
                       loval=100 * df4$loval / df4$gdp.proj, muval=NA, hival=100 * df4$hival / df4$gdp.proj))

save(df, file = "/Users/ritikakhurana/Open Modeling Group Dropbox/COACCH/compare/df.RData")

## Version 1: Combine to channel level

aggs <- cbind(subset(df, Contribute != '') %>% group_by(Channel, Years, Synthesis, Scenario) %>%
                summarize(Method=ifelse(all(Method == Method[1]), Method[1], ifelse(Channel[1] %in%
                c('Ecosystems', 'Fisheries'), 'Process-based', 'Aggregate')), loval=sum(loval, na.rm=T),
                  muval=ifelse(all(is.na(muval)), NA, sum(muval, na.rm=T)), hival=sum(hival, na.rm=T),
                    Source=ifelse(Source[1] == 'CCRA3', 'CCRA3 Aggregate', 'Our Aggregate')))

pdf <- rbind(aggs[, -which(names(aggs) == 'Synthesis')], df[df$Contribute == '',
                     -which(names(df) %in% c('Synthesis', 'Contribute'))])
pdf$Source <- factor(pdf$Source, levels=c("Combo", unique(pdf$Source)[!(unique(pdf$Source) %in%
                 c("Combo", "CCRA3 Aggregate"))], "CCRA3 Aggregate"))
pdf$Channel <- factor(pdf$Channel, levels=rev(sort(unique(pdf$Channel))))

require(scales)
pseudolog_trans = function() trans_new("pseudolog", function(x) sign(x) * log(1 + abs(x)), function(x) sign(x) * (exp(abs(x)) - 1))

pdf$Source.symbol <- ifelse(pdf$Source == "CCRA3 Aggregate", "†",
                     ifelse(pdf$Source == "Bayesian Combo", "*",
                     ifelse(pdf$Source == "Our Aggregate", '**', '')))

## Drop CCRA3 results
ggplot(subset(pdf, Channel != 'Transport' & Source != "CCRA3 Aggregate"), aes(Channel, group=Source)) +
    ## facet_wrap(~ Years + Scenario, nrow=1) +
    facet_grid(. ~ Years + Scenario, scales="free_x", space="free_x") +
    coord_flip() +
    geom_hline(yintercept=0) +
    geom_errorbar(aes(ymin=loval, ymax=hival, colour=Method), position=position_dodge(width=1)) +
    geom_vline(xintercept=(1:13)+.5) +
    ## geom_text(data=subset(pdf, Channel != 'Transport' & Years == '2011-2030' & Scenario == 'Baseline'), aes(y=15, label=Source), size=1.2, hjust=1, position=position_dodge(width=.75)) +
    geom_text(data=subset(pdf, Channel != 'Transport' & Years == '2081-2100' & Scenario == 'Baseline' & Source != "CCRA3 Aggregate"), aes(y=hival * 1.05 + 0.1, label=Source.symbol),
              size=3.5, hjust=0, position=position_dodge(width=1)) +
    scale_y_continuous("Economic damages (% GDP)", trans="pseudolog", breaks=c(-10, -5, -2, -1, 0, 1, 2, 5, 10)) +
    scale_colour_manual("Method:", values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e')) +
    xlab(NULL) + theme_bw() + theme(legend.position="bottom") +
    guides(color=guide_legend(nrow=2))
ggsave("compare-aggregate.pdf", width=10, height=7)

## Other changes in Illustrator:
## Bolded innovative sectors (Catastrophic, Missing, Trade Spillovers).
## Change top to Baseline (SSP 3-7.0) with line under 3 cols and High Mitigation (SSP 1-2.6)

