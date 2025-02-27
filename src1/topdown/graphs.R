setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

df.bea <- read.csv("topdown/burkeetal-final.csv")
df.djo <- read.csv("topdown/djo-final.csv")
df.kea <- read.csv("topdown/kahnetal-final.csv")

df <- rbind(cbind(source="Dell, Jones, Olken (2012)", df.djo),
            cbind(source="Burke et al. (2015)", df.bea),
            cbind(source="Kahn et al. (2019)", df.kea))
df$period.label <- paste(df$period - 9, "-", df$period + 10)
df$scenario.label <- "SSP1-2.6"
df$scenario.label[df$scenario == 'ssp370'] <- "SSP3-7.0"

library(dplyr)

df2 <- df %>% group_by(source, scenario.label, period.label) %>% summarize(group=c('UK-only', 'ROW on UK'), mu=c(alone[1], globe[1]))
df2.ci <- df %>% group_by(source, scenario.label, period.label) %>% summarize(ci5=alone[2] + globe[2], ci95=alone[3] + globe[3])

library(ggplot2)

ggplot(df2, aes(source)) +
    facet_wrap(~ scenario.label + period.label) +
    geom_hline(yintercept=0) +
    geom_col(aes(y=mu / 100, fill=group), position='stack') +
    geom_errorbar(data=df2.ci, aes(ymin=ci5 / 100, ymax=ci95 / 100), width=.25) +
    scale_y_continuous("Annual welfare-equivalent damages (% GDP)", labels=scales::percent) + xlab(NULL) +
    theme_bw() + scale_fill_discrete("Group:") + theme(axis.text.x=element_text(angle=45, hjust=1))

ggplot(df2, aes(source)) +
    facet_wrap(~ scenario.label + period.label) +
    coord_flip() +
    geom_hline(yintercept=0) +
    geom_col(aes(y=mu / 100, fill=group), position='stack') +
    geom_errorbar(data=df2.ci, aes(ymin=ci5 / 100, ymax=ci95 / 100), width=.25) +
    scale_y_continuous("Annual welfare-equivalent damages (% GDP)", labels=scales::percent) + xlab(NULL) +
    theme_bw() + scale_fill_discrete("Group:")

ggplot(df2, aes(period.label)) +
    facet_wrap(~ scenario.label + source, ncol=4, scales='free_y') +
    geom_hline(yintercept=0) +
    geom_col(aes(y=mu / 100, fill=group), position='stack') +
    geom_errorbar(data=df2.ci, aes(ymin=ci5 / 100, ymax=ci95 / 100), width=.25) +
    scale_y_continuous("Annual welfare-equivalent damages (% GDP)", labels=scales::percent) + xlab(NULL) +
    theme_bw() + scale_fill_discrete("Group:") + theme(axis.text.x=element_text(angle=45, hjust=1))

df3 <- rbind(df2, data.frame(source=unique(df2$source), scenario.label='', period.label='', group='UK-only', mu=0))
df3$full.label <- factor(paste(df3$scenario.label, df3$period.label), levels=c(paste("SSP1-2.6", unique(df2$period.label)), ' ', paste("SSP3-7.0", unique(df2$period.label))))

df3.ci <- rbind(df2.ci, data.frame(source=unique(df2$source), scenario.label='', period.label='', ci5=0, ci95=0))
df3.ci$full.label <- factor(paste(df3.ci$scenario.label, df3.ci$period.label), levels=c(paste("SSP1-2.6", unique(df2$period.label)), ' ', paste("SSP3-7.0", unique(df2$period.label))))

ggplot(df3, aes(full.label)) +
    facet_wrap(~ source, ncol=3, scales='free_y') +
    geom_hline(yintercept=0) +
    geom_col(aes(y=mu / 100, fill=group), position='stack') +
    geom_errorbar(data=df3.ci, aes(ymin=ci5 / 100, ymax=ci95 / 100), width=.25) +
    geom_vline(xintercept=' ') +
    scale_y_continuous("Annual welfare-equivalent damages (% GDP)", labels=scales::percent) + xlab(NULL) +
    theme_bw() + scale_fill_discrete("Group:") + theme(axis.text.x=element_text(angle=45, hjust=1))


