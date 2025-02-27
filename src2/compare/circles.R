setwd("~/Dropbox/COACCH/compare")

library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)

do.halving <- F
do.scaling <- T

load("../CCRA3/ccradraws.RData") # allfracs, df.allfracs
df.allfracs$Code[df.allfracs$Code == "N10"] <- "NE10"
df.allfracs$Code[df.allfracs$Code == "N11"] <- "NE11"
df.allfracs$Code[df.allfracs$Code == "N12"] <- "NE12"
df.allfracs$Code[df.allfracs$Code == "N13"] <- "NE13"

unique(df.allfracs$Code[-grep("(B|H|I|ID|NE)\\d", df.allfracs$Code)])

df1 <- data.frame()
for (code in unique(df.allfracs$Code)) {
    row <- which(df.allfracs$Code == code & df.allfracs$cdiff == 4)
    df1 <- rbind(df1, data.frame(Code=code, mu=mean(allfracs[row,]), ci5=quantile(allfracs[row,], .05), ci95=quantile(allfracs[row,], .95)))
}
df2 <- df.allfracs %>% filter(cdiff == 4) %>% left_join(df1)

numvals <- as.numeric(!is.na(df2$loval)) + as.numeric(!is.na(df2$hival)) + as.numeric(!is.na(df2$muval)) + as.numeric(!is.na(df2$seval))
df2$quality <- c('Unknown', 'Unknown', 'Bounded', 'Bounded', 'Distributed')[numvals + 1]

df3 <- read.csv("results.csv")
df3 <- subset(df3, Years == '2081-2100')
df3$Code <- NA
df3$Code[df3$Channel == 'Agriculture' & df3$Source == 'Bayesian Combo'] <- 'NE6a'
df3$Code[df3$Channel == 'Ecosystems' & df3$Contribute == 'Biodiversity'] <- 'NE19'
df3$Code[df3$Channel == 'Ecosystems' & df3$Contribute == 'Amenities'] <- 'NE18'
df3$Code[df3$Channel == 'Ecosystems' & df3$Contribute == 'Carbon'] <- 'NE5'
df3$Code[df3$Channel == 'Ecosystems' & df3$Contribute == 'Forestry'] <- 'NE9b'
df3$Code[df3$Channel == 'Energy demand' & df3$Source == 'Bayesian Combo'] <- 'H6' # NEED TO DROP H6a, H6b
df3$Code[df3$Channel == 'Energy supply' & df3$Source == 'Bayesian Combo'] <- 'I6' # NEED TO DROP I9, halve I10
df3$Code[df3$Channel == 'Fisheries' & df3$Contribute == 'Algal Blooms'] <- 'NE12b' # NEED TO halve NE12 as NE12a
df3$Code[df3$Channel == 'Fisheries' & df3$Contribute == 'Marine'] <- 'NE14' # NEED TO DROP NE15
df3$Code[df3$Channel == 'Mortality' & df3$Source == 'Bayesian Combo'] <- 'H1a'
df3$Code[df3$Channel == 'Droughts'] <- 'B3' # NEED TO DROP H10a, I8
df3$Code[df3$Channel == 'Inland flooding' & df3$Source == 'Bayesian Combo'] <- 'B1' # NEED TO DROP H3a, I2
df3$Code[df3$Channel == 'Labour productivity' & df3$Source == 'Bayesian Combo'] <- 'B5'
df3$Code[df3$Channel == 'Livestock' & df3$Contribute == 'Sheep'] <- 'NE7b' # NEED TO halve NE7 as NE7a
df3$Code[df3$Channel == 'Livestock' & df3$Contribute == 'Milk'] <- 'NE7c'
df3$Code[df3$Channel == 'Coastal impacts' & df3$Source == 'Bayesian Combo'] <- 'B2' # NEED to drop H3b, H4, I3, halve NE17
df3$Code[df3$Channel == 'Trade effects' & df3$Source == 'Bayesian Combo'] <- 'ID8'
df3$Code[df3$Channel == 'Catastrophic'] <- 'ID10'

todrop <- c(df3$Code[!is.na(df3$Code)], 'H6a', 'H6b', 'I9', 'NE15', 'H10a', 'I8', 'H3a', 'I2', 'H3b', 'H4', 'I3')
tohalve <- c('I10', 'NE12', 'NE7', 'NE17')

df2.tocomb <- data.frame(Code=df2$Code[!(df2$Code %in% todrop)], Channel=NA, Contribute=NA,
                         mu=df2$mu[!(df2$Code %in% todrop)], ci95=df2$ci95[!(df2$Code %in% todrop)],
                         ci5=df2$ci5[!(df2$Code %in% todrop)], quality=df2$quality[!(df2$Code %in% todrop)])
if (do.halving) {
    df2.tocomb$mu[df2.tocomb$Code %in% tohalve] <- df2.tocomb$mu[df2.tocomb$Code %in% tohalve] / 2
    df2.tocomb$ci05[df2.tocomb$Code %in% tohalve] <- df2.tocomb$ci05[df2.tocomb$Code %in% tohalve] / 2
    df2.tocomb$ci95[df2.tocomb$Code %in% tohalve] <- df2.tocomb$ci95[df2.tocomb$Code %in% tohalve] / 2
    df2.tocomb$Code[df2.tocomb$Code %in% tohalve] <- paste0(df2.tocomb$Code[df2.tocomb$Code %in% tohalve], 'a')
}

if (do.scaling) {
    ## Calculate scaling factor

    df4 <- df3 %>% left_join(df2[, c('Code', 'mu')])
    df4$mu[df4$Code == 'H6'] <- sum(df2$mu[df2$Code %in% c('H6a', 'H6b')])
    df4$mu[df4$Code == 'I6'] <- sum(df2$mu[df2$Code %in% c('I6', 'I9')])
    df4$mu[df4$Code == 'NE14'] <- sum(df2$mu[df2$Code %in% c('NE14', 'NE15')])
    df4$mu[df4$Code == 'B3'] <- sum(df2$mu[df2$Code %in% c('B3', 'H10a', 'I8')])
    df4$mu[df4$Code == 'B1'] <- sum(df2$mu[df2$Code %in% c('B1', 'H3a', 'I2')])
    df4$mu[df4$Code == 'B2'] <- sum(df2$mu[df2$Code %in% c('B2', 'H3b', 'H4', 'I3')])

    df5 <- subset(df4, !is.na(mu))

    library(quantreg)
    coeffs <- c()
    for (ii in 1:4000) {
        mod <- rq(SSP370.mean ~ 0 + mu, data=df5[sample(nrow(df5), nrow(df5), replace=T),])
        coeffs <- c(coeffs, coef(mod))
    }

    ## Correct df2
    df2.scaled <- df2.tocomb
    for (ii in 1:nrow(df2.scaled)) {
        row <- which(df.allfracs$Code == df2.scaled$Code[ii] & df.allfracs$cdiff == 4)
        df2.scaled$mu[ii] <- mean(coeffs * allfracs[row,])
        df2.scaled$ci5[ii] <- quantile(coeffs * allfracs[row,], .05)
        df2.scaled$ci95[ii] <- quantile(coeffs * allfracs[row,], .95)
    }

    save(coeffs, file="ccra3-scaling-2100.RData")
}

df <- rbind(data.frame(Code=df3$Code[!is.na(df3$Code)], Channel=df3$Channel[!is.na(df3$Code)],
                       Contribute=df3$Contribute[!is.na(df3$Code)], mu=df3$SSP370.mean[!is.na(df3$Code)],
                       ci5=df3$SSP370.q05[!is.na(df3$Code)], ci95=df3$SSP370.q95[!is.na(df3$Code)],
                       quality=ifelse(df3$Source[!is.na(df3$Code)] == "Bayesian Combo", "Multiple", "Single")), df2.tocomb)
df <- df[order(abs(df$mu), decreasing=T),]

## Create line-graph
pdf <- df %>% left_join(df2[, c('Code', 'Risk', 'Double Counting')])
pdf$double <- !is.na(pdf$`Double Counting`) | (pdf$Code %in% tohalve)
pdf$Risk[pdf$Code == 'H6'] <- "Risks/Opportunities from changing household energy demand"
pdf$Risk[pdf$Code == 'NE19'] <- "Welfare loss from global biodiversity loss"
pdf$Risk[pdf$Code == 'NE12b'] <- "Risks to freshwater fisheries from algal blooms"
pdf$Risk[pdf$Code == 'NE7b'] <- "Risks to lamb production from pests"
pdf$Risk[pdf$Code == 'NE7c'] <- "Risks to milk production from heat stress"
pdf$Code[is.na(pdf$Risk)]

pdf$Risk[pdf$Risk == "H1+H2 Mortality-Only"] <- "Risks and opportunities to mortality from high temperatures"
pdf$Risk[pdf$Risk == "H1+H2 Morbidity-Only"] <- "Risks and opportunities to morbidity from high temperatures"
pdf$Risk[pdf$Risk == "NE9 Agriculture-Only"] <- "Opportunities for agricultural productivity from new/alternative species"
pdf$Risk[pdf$Risk == "NE9 Forestry-Only"] <- "Opportunities for forestry productivity from new/alternative species"

pdf$Risk <- factor(pdf$Risk, levels=rev(pdf$Risk[order(abs(pdf$mu), decreasing=T)]))

pdf$Risk.short <- c('Risk Cascades', 'Productivity Loss', 'Finance Sector Risk', 'Energy Demand Shift', 'Coastal Change Impact', 'Flooding Risk', 'Heat Mortality', 'Marine Climate Impact', 'Water Scarcity', 'Agricultural Impact', 'Biodiversity Loss', 'Landscape Character', 'Forestry Opportunity', 'Hydropower Risk', 'Algal Bloom Risk', 'Carbon Store Risk', 'Livestock Pest Risk', 'Milk Heat Stress', 'Trade Route Risk', 'Business Demand Opportunity', 'Trade Route Opportunity', 'Infrastructure Cascades', 'Financial Risk', 'Agricultural Opportunity', 'Heat Morbidity', 'Allergen Risk', 'Water Quality Risk', 'Terrestrial Pest Risk', 'Supply Chain Risk', 'Social Care Risk', 'Saltwater Intrusion', 'Service Disruption Risk', 'Species Colonization Opportunity', 'Cultural Heritage Risk', 'Governance Risk', 'Building Fabric Risk', 'Freshwater Ecosystem Risk', 'Soil Risk', 'Transport Infrastructure Risk', 'Food Export Opportunity', 'Conflict Risk', 'Digital Infrastructure Risk', 'Forestry Pest Risk', 'Terrestrial Climate Impact', 'Agricultural Pest Risk', 'Transport Climate Risk', 'Forestry Impact', 'Energy Grid Risk', 'Food Safety Risk', 'Freshwater Colonization Opportunity', 'Public Health Risk', 'Marine Pest Risk', 'Infrastructure Subsidence', 'Freshwater Pest Risk', 'Bridge/Pipeline Erosion Risk', 'Vector-borne Disease Risk', 'Coastal Ecosystem Risk', 'Food Security Risk', 'Human Mobility Risk', 'Offshore Infrastructure Risk', 'Air Pollution Risk')

pdf$label <- paste0(pdf$Risk.short, ' (', pdf$Code, ')')
pdf$label[pdf$Code == 'H1a'] <- "Heat Mortality (H1+H2 Mortality-only)"
pdf$label[pdf$Code == 'H1b'] <- "Heat Morbidity (H1+H2 Morbidity-only)"
pdf$label[pdf$Code == 'NE9a'] <- "Agricultural Opportunity (NE9 Agriculture-only)"
pdf$label[pdf$Code == 'NE9b'] <- "Forestry Opportunity (NE9 Forestry-only)"

pdf$mylabel <- NA
pdf$mylabel[!is.na(pdf$Channel)] <- pdf$Channel[!is.na(pdf$Channel)]
pdf$mylabel[!is.na(pdf$Channel) & pdf$Contribute != ''] <- paste0(pdf$Channel, ': ', pdf$Contribute)[!is.na(pdf$Channel) & pdf$Contribute != '']

pdf$label <- factor(pdf$label, levels=rev(pdf$label[order(abs(pdf$mu), decreasing=T)]))

pdf$scale <- pmax(abs(pdf$ci5), abs(pdf$ci95))
pdf$Category <- NA
pdf$Category[grep("B\\d", pdf$Code)] <- "Business and Industry"
pdf$Category[grep("I\\d", pdf$Code)] <- "Infrastructure"
pdf$Category[grep("ID\\d", pdf$Code)] <- "International"
pdf$Category[grep("NE\\d", pdf$Code)] <- "Natural Environment"
pdf$Category[grep("H\\d", pdf$Code)] <- "Health, Communities and\nthe Built Environment"
pdf$quality[pdf$quality %in% c('Bounded', 'Distributed')] <- "Indicative"
pdf$quality[pdf$quality == "Single"] <- "Modeled"
pdf$quality[pdf$quality == "Multiple"] <- "Meta-Analysis"

if (do.scaling) {
    require(scales)
    pseudolog_trans = function() trans_new("pseudolog", function(x) sign(x) * log(1 + abs(x)), function(x) sign(x) * (exp(abs(x)) - 1))

    pdf2 <- pdf %>% left_join(df2.scaled[, c('Code', 'mu', 'ci5', 'ci95')], by='Code', suffix=c('', '.scaled'))
    pdf2 <- pdf2[order(ifelse(is.na(pdf2$mu.scaled), pdf2$mu, pdf2$mu.scaled), decreasing=T),]
    pdf2$Risk <- factor(pdf2$Risk, levels=rev(pdf2$Risk[order(ifelse(is.na(pdf2$mu.scaled), pdf2$mu, pdf2$mu.scaled), decreasing=T)]))
    pdf2$label <- factor(pdf2$label, levels=rev(pdf2$label[order(ifelse(is.na(pdf2$mu.scaled), pdf2$mu, pdf2$mu.scaled), decreasing=T)]))
    pdf2$quality.unscaled <- pdf2$quality
    pdf2$quality.unscaled[pdf2$quality.unscaled %in% c("Indicative", "Unknown")] <- "Unscaled"

    ## log(1 + (exp(x) - 1))

    catcolors <- list("Business and Industry"='#d95f02',
                      "Infrastructure"='#e7298a',
                      "International"='#1b9e77',
                      "Natural Environment"='#66a61e',
                      "Health, Communities and\nthe Built Environment"='#7570b3')

    gp <- ggplot(pdf2, aes(label, colour=Category, linetype=quality.unscaled, linewidth=quality.unscaled, shape=quality.unscaled)) +
        coord_flip() +
        geom_linerange(aes(ymin=10 * ci5, ymax=10 * ci95)) +
        geom_linerange(aes(ymin=10 * ci5.scaled, ymax=10 * ci95.scaled, linetype=quality, linewidth=quality)) +
        geom_point(aes(y=10 * mu.scaled, shape=quality)) +
        geom_point(aes(y=10 * mu)) +
        geom_hline(yintercept=0) + theme_bw() +
        geom_hline(yintercept=200, colour='#00000000') + theme_bw() +
        geom_text(data=subset(pdf2, double), aes(y=200), label='*', colour='#000000') +
        geom_text(data=subset(pdf2, !is.na(mylabel)), aes(y=190, label=mylabel), hjust='right', colour='#000000', size=2) +
        scale_linetype_manual("Model quality", breaks=c('Unscaled', 'Unknown', 'Indicative', 'Modeled', 'Meta-Analysis'), values=c('blank', 'dotted', 'dashed', 'solid', 'solid')) +
        scale_linewidth_manual("Model quality", breaks=c('Unscaled', 'Unknown', 'Indicative', 'Modeled', 'Meta-Analysis'), values=.5 * c(1, 1, 1, 1, 2)) +
        scale_shape_manual("Model quality", breaks=c('Unscaled', 'Unknown', 'Indicative', 'Modeled', 'Meta-Analysis'), values=c(4, 1, 1, 16, 16)) +
        scale_colour_manual("CCRA Category", breaks=names(catcolors), values=as.character(catcolors)) +
        scale_y_continuous("Welfare-equivalent Damages (% GDP)", trans="pseudolog", breaks=10 * c(-2, -1, -.5, -.2, -.1, 0, .1, .2, .5, 1, 2, 5, 10), labels=c(-2, -1, -.5, -.2, -.1, 0, .1, .2, .5, 1, 2, 5, 10)) + xlab(NULL)
    ggsave("figure2.pdf", width=10, height=7)

} else {
    ## pdf <- rbind(pdf[1:35,], tibble(Code='X', mu=sum(pdf$mu[36:nrow(pdf)]), ci5=sum(pdf$ci5[36:nrow(pdf)]), ci95=sum(pdf$ci95[36:nrow(pdf)]),
    ##                                 quality='Distributed', Risk='Other risks and opportunities', `Double Counting`=NA, double=T, scale=sum(abs(pdf$scale[36:nrow(pdf)]))))

    pdf <- pdf[1:36,]
    pdf$panel <- rep(1:4, each=9)

    ggplot(pdf, aes(Risk, colour=quality)) +
        coord_flip() + facet_wrap(panel ~ ., scales='free', ncol=1) +
        geom_linerange(aes(ymin=ci5, ymax=ci95)) +
        geom_point(aes(y=mu)) +
        geom_hline(yintercept=0)
}

## Making the circles

make.wedge <- function(pid, x0, y0, theta0, radius, radians, resolution=10) {
    arclen <- radians * radius
    thetas <- seq(theta0, theta0 + radians, length.out=max(2, round(arclen * resolution)))

    data.frame(PID=pid, X=c(x0, x0 + radius * cos(thetas), x0),
               Y=c(y0, y0 + radius * sin(thetas), y0))
}

## ggplot(make.wedge(1, 0, 0, pi / 2, 1, pi / 3), aes(X, Y, group=PID)) +
##     geom_polygon()

xy0 <- data.frame(prefix=c('ID', 'B', 'H', 'NE', 'I'), y0=-c(0, 3, 5, 6.5, 8), x0=0,
                  labels=c("International", "Business and Industry", "Health, Communities and\n the Built Environment", "Natural Environment", "Infrastructure"))

forepolys <- data.frame()
backpolys <- data.frame()
labels <- data.frame()
for (prefix in c('B', 'H', 'I', 'ID', 'NE')) {
    rows <- which(grepl(paste0(prefix, "\\d"), df$Code) & df$mu > 0)
    mutot <- sum(df$mu[rows])
    radius <- sqrt(mutot / pi)
    theta0 <- 0
    for (row in rows) {
        radians <- 2*pi * df$mu[row] / mutot
        if (radians < .05 * 2*pi)
            break
        radius95 <- sqrt(df$ci95[row] / (radians / 2))
        forepolys <- rbind(forepolys, cbind(make.wedge(row, xy0$x0[xy0$prefix == prefix], xy0$y0[xy0$prefix == prefix], theta0, radius, radians, resolution=1000), panel='Major'))
        backpolys <- rbind(backpolys, cbind(make.wedge(max(c(0, backpolys$PID)) + 1, xy0$x0[xy0$prefix == prefix], xy0$y0[xy0$prefix == prefix], theta0, radius95, radians, resolution=1000), panel='Major'))
        if (radians * radius > .1 * 2*pi)
            labels <- rbind(labels, data.frame(PID=row, X=xy0$x0[xy0$prefix == prefix] + .9 * radius * cos(theta0 + radians / 2),
                                               Y=xy0$y0[xy0$prefix == prefix] + .9 * radius * sin(theta0 + radians / 2), panel='Major'))
        theta0 <- theta0 + radians
    }

    if (radians >= .05 * 2*pi)
        break
    rows <- rows[rows >= row]

    mutot <- sum(df$mu[rows])
    radius <- sqrt(mutot / pi)
    theta0 <- 0
    for (row in rows) {
        radians <- 2*pi * df$mu[row] / mutot
        radius95 <- sqrt(df$ci95[row] / (radians / 2))
        forepolys <- rbind(forepolys, cbind(make.wedge(row, xy0$x0[xy0$prefix == prefix], xy0$y0[xy0$prefix == prefix], theta0, radius * 20, radians, resolution=1000), panel='Minor'))
        backpolys <- rbind(backpolys, cbind(make.wedge(max(c(0, backpolys$PID)) + 1, xy0$x0[xy0$prefix == prefix], xy0$y0[xy0$prefix == prefix], theta0, radius95 * 20, radians, resolution=1000), panel='Minor'))
        if (radians * 20 * radius > .1 * 2*pi)
            labels <- rbind(labels, data.frame(PID=row, X=xy0$x0[xy0$prefix == prefix] + 20 * .9 * radius * cos(theta0 + radians / 2),
                                               Y=xy0$y0[xy0$prefix == prefix] + 20 * .9 * radius * sin(theta0 + radians / 2), panel='Minor'))
        theta0 <- theta0 + radians
    }
}
for (prefix in c('B', 'H', 'I', 'ID', 'NE')) {
    rows <- which(grepl(paste0(prefix, "\\d"), df$Code) & df$mu < 0)
    mutot <- -sum(df$mu[rows])
    radius <- sqrt(mutot / pi)
    if (radius > .05) {
        theta0 <- 0
        for (row in rows) {
            radians <- -2*pi * df$mu[row] / mutot
            if (radians < .05 * 2*pi)
                break
            radius95 <- sqrt(-df$ci5[row] / (radians / 2))
            forepolys <- rbind(forepolys, cbind(make.wedge(row, xy0$x0[xy0$prefix == prefix] + 4, xy0$y0[xy0$prefix == prefix], theta0, radius, radians, resolution=1000), panel='Major'))
            backpolys <- rbind(backpolys, cbind(make.wedge(max(c(0, backpolys$PID)) + 1, xy0$x0[xy0$prefix == prefix] + 4, xy0$y0[xy0$prefix == prefix], theta0, radius95, radians, resolution=1000), panel='Major'))
            if (radians * radius > .1 * 2*pi)
                labels <- rbind(labels, data.frame(PID=row, X=xy0$x0[xy0$prefix == prefix] + 4 + .9 * radius * cos(theta0 + radians / 2),
                                                   Y=xy0$y0[xy0$prefix == prefix] + .9 * radius * sin(theta0 + radians / 2), panel='Major'))
            theta0 <- theta0 + radians
        }

        if (radians >= .05 * 2*pi)
            break
        rows <- rows[rows >= row]
    }

    mutot <- -sum(df$mu[rows])
    radius <- sqrt(mutot / pi)
    theta0 <- 0
    for (row in rows) {
        radians <- -2*pi * df$mu[row] / mutot
        radius95 <- sqrt(-df$ci5[row] / (radians / 2))
        forepolys <- rbind(forepolys, cbind(make.wedge(row, xy0$x0[xy0$prefix == prefix] + 4, xy0$y0[xy0$prefix == prefix], theta0, radius * 20, radians, resolution=1000), panel='Minor'))
        backpolys <- rbind(backpolys, cbind(make.wedge(max(c(0, backpolys$PID)) + 1, xy0$x0[xy0$prefix == prefix] + 4, xy0$y0[xy0$prefix == prefix], theta0, radius95 * 20, radians, resolution=1000), panel='Minor'))
        if (radians * 20 * radius > .1 * 2*pi)
            labels <- rbind(labels, data.frame(PID=row, X=xy0$x0[xy0$prefix == prefix] + 4 + 20 * .9 * radius * cos(theta0 + radians / 2),
                                               Y=xy0$y0[xy0$prefix == prefix] + 20 * .9 * radius * sin(theta0 + radians / 2), panel='Minor'))
        theta0 <- theta0 + radians
    }
}

forepolys$quality <- df$quality[forepolys$PID]
forepolys$panel[forepolys$panel == 'Minor'] <- "Minor (Values × 20)"
backpolys$panel[backpolys$panel == 'Minor'] <- "Minor (Values × 20)"

labels$Code <- df$Code[labels$PID]
labels$panel[labels$panel == 'Minor'] <- "Minor (Values × 20)"

ggplot(forepolys, aes(X, Y, group=PID)) +
    coord_fixed() + facet_wrap(~ panel, ncol=2) +
    geom_polygon(data=backpolys, fill='#808080') +
    geom_polygon(aes(fill=quality), colour='#FFFFFF', lwd=.1) +
    geom_text(data=labels, aes(label=Code), size=3) +
    theme_bw() + scale_y_continuous("Risk category", breaks=xy0$y0, labels=xy0$labels) +
    scale_x_continuous(NULL, breaks=c(0, 4), labels=c("Risk", "Opportunity")) +
    scale_fill_discrete("Models quality:", breaks=c('Unknown', 'Bounded', 'Distributed', 'Single', 'Multiple')) +
    theme(panel.grid.minor = element_blank())
ggsave("circles.pdf", width=9, height=4)
