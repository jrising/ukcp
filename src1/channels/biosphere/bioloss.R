setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

df <- read.csv("channels/biosphere/bioloss.csv")
df2 <- rbind(data.frame(dTg0=0, dTg1=0, loss0=0, loss1=1), df)

library(ggplot2)

ggplot(df2, aes(x=(dTg0 + dTg1)/2, y=(loss0 + loss1)/200)) +
    geom_smooth(se=F) +
    geom_smooth(method='lm', formula=y ~ 0 + x) +
    geom_point() + geom_linerange(aes(ymin=loss0/100, ymax=loss1/100)) +
    theme_bw() + scale_x_continuous(expand=c(0, 0)) +
    scale_y_continuous(labels=scales::percent) +
    coord_cartesian(ylim=c(0, 1)) + xlab("Global temperature change above pre-industrial") +
    ylab("Loss of species or habitat")
ggsave("channels/biosphere/bioloss.pdf", width=6.5, height=4)

source("lib/damagefunc.R")

la <- fit.damages((df2$dTg0 + df2$dTg1)/2, (df2$loss0 + df2$loss1)/2, (df2$loss1 - df2$loss0 + 0.5) / (2*1.96))

pdf2 <- data.frame(T=(df2$dTg0 + df2$dTg1)/2, mu=(df2$loss0 + df2$loss1)/2, loss0=df2$loss0, loss1=df2$loss1)
ggplot.damagefunc(la, (df2$dTg0 + df2$dTg1)/2, (df2$loss0 + df2$loss1)/2, (df2$loss1 - df2$loss0 + 0.5) / (2*1.96)) +
    coord_cartesian(ylim=c(0, 100)) + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) +
    geom_linerange(data=pdf2, aes(ymin=loss0, ymax=loss1)) + xlab("Global mean warming from pre-industrial (C)") +
    ylab("Percent habitat or species impacted")
ggsave("channels/biosphere/bioloss-df.pdf", width=6.5, height=4)

alldraws <- read.csv("climate/gsat/ssps_26_70_posttp.csv")

library(readxl)

wtps <- read_excel("channels/biosphere/Meta_data.xlsx")
wtps <- subset(wtps, Country == "United Kingdom" & `Published in peer-reviewed journal` == 1 & `Human_cause` == 1)
wtps$fracgdp <- wtps$`Mean WTP` / wtps$`GDP (GDP per capita in current (2017) US$ PPP in the data collection year) `

sum(wtps$fracgdp * wtps$Rel_weights) / sum(wtps$Rel_weights)

alldraws$bioloss <- NA
alldraws$damage <- NA
for (rid in unique(alldraws$run_id)) {
    print(rid)
    draw <- sample.int(dim(la$coeff)[1], 1)
    coeffs <- la$coeff[draw, ]
    rows <- which(alldraws$run_id == rid)

    alldraws$bioloss[rows] <- pmax(0, pmin(coeffs[2] * alldraws$value[rows] + coeffs[3] * alldraws$value[rows]^2, 100))

    ## Sample random WTP
    wtp <- sample(wtps$fracgdp, 1, prob=wtps$Rel_weights / sum(wtps$Rel_weights))
    alldraws$damage[rows] <- wtp * alldraws$bioloss[rows] / 100
}

alldraws$period <- NA
alldraws$period[alldraws$year >= 2011 & alldraws$year <= 2030] <- "2011-2030"
alldraws$period[alldraws$year >= 2041 & alldraws$year <= 2060] <- "2041-2060"
alldraws$period[alldraws$year >= 2081 & alldraws$year <= 2100] <- "2081-2100"

library(dplyr)

results <- alldraws %>% group_by(scenario, period, run_id) %>% summarize(bioloss=mean(bioloss), damage=mean(damage))

results %>% group_by(scenario, period) %>% summarize(bioloss=mean(bioloss))

source("lib/report.R")

results$percent <- results$damage * 100
tbl <- make.table(results, 'percent')
print(xtable(tbl, digits=2), include.rownames=F)

save(results, file="channels/biosphere/bioloss.RData")

wtps$ref <- paste(gsub("[_0-9]", "", wtps$Author), wtps$`Publication year`)
wtps$percent <- paste0(round(wtps$fracgdp * 100, 2), "%")

tbl <- wtps[, c('ref', 'Mean WTP', 'percent', 'Species abundance', 'Species richness', 'Habitat focused', 'Species focused', 'Is the outcome uncertain?', 'Human_cause', 'Rel_weights')]
names(tbl) <- c("Reference", "WTP (US$2017)", "\\% GDP", 'Sp. abundance', 'Sp. richness', 'On Habitat', 'On Species', 'Uncertainty', 'Human cause', 'Weight')
for (col in names(tbl)[4:9])
    tbl[, col] <- ifelse(tbl[, col] == 1, "Yes", "No")

print(xtable(tbl), include.rownames=F)
