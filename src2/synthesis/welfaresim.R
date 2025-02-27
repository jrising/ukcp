library(dplyr)
library(reshape2)
library(ggplot2)

baseline <- c(32000, 40000)
eta <- 1.35

utility <- function(cc)
    (cc^(1 - eta) - 1) / (1 - eta)
invutil <- function(uu)
    (uu * (1 - eta) + 1)^(1 / (1 - eta))

no.welfare <- function(df) {
    df %>% summarize(cc=mean(cc))
}

avg.ineq <- function(df) {
    df %>% group_by(mm) %>% summarize(uu=mean(utility(cc))) %>%
        ungroup() %>% summarize(cc=mean(invutil(uu)))
}

ineq.avg <- function(df) {
    df %>% group_by(ii) %>% summarize(uu=utility(mean(cc))) %>%
        ungroup() %>% summarize(cc=invutil(mean(uu)))
}

all.welfare <- function(df) {
    df %>% summarize(cc=invutil(mean(utility(cc))))
}

df.baseline <- data.frame(ii=1:2, mm=1, cc=baseline)

no.welfare(df.baseline)
avg.ineq(df.baseline)
ineq.avg(df.baseline)
all.welfare(df.baseline)

get.table <- function(df) {
    cc.nw <- no.welfare(df)$cc - no.welfare(df.baseline)$cc
    cc.ai <- avg.ineq(df)$cc - avg.ineq(df.baseline)$cc
    cc.ia <- ineq.avg(df)$cc - ineq.avg(df.baseline)$cc
    cc.aw <- all.welfare(df)$cc - all.welfare(df.baseline)$cc

    data.frame(`No Welfare`=cc.nw, `Average of Inequality`=cc.ai, `Inequality of Average`=cc.ia, `All Welfare`=cc.aw)
}

results <-
    rbind(#cbind(scenario="No impacts", get.table(df.baseline)),
          cbind(scenario="Uniform level, no uncertainty", get.table(data.frame(ii=1:2, mm=1, cc=baseline - 15000))),
          cbind(scenario="Uniform fraction, no uncertainty", get.table(data.frame(ii=1:2, mm=1, cc=baseline * .6))),
          cbind(scenario="Pro-rich, no uncertainty", get.table(data.frame(ii=1:2, mm=1, cc=baseline * c(.4, 1)))),
          cbind(scenario="Pro-poor, no uncertainty", get.table(data.frame(ii=1:2, mm=1, cc=baseline * c(1, .4)))),
          cbind(scenario="Uniform level, with uncertainty", get.table(rbind(data.frame(ii=1:2, mm=1, cc=baseline - 10000),
                                                                            data.frame(ii=1:2, mm=2, cc=baseline - 20000)))),
          cbind(scenario="Uniform fraction, with uncertainty", get.table(rbind(data.frame(ii=1:2, mm=1, cc=baseline * .6),
                                                                               data.frame(ii=1:2, mm=2, cc=baseline * .2)))),
          cbind(scenario="Pro-rich, uncertainty increases", get.table(rbind(data.frame(ii=1:2, mm=1, cc=baseline * c(.6, 1)),
                                                                            data.frame(ii=1:2, mm=2, cc=baseline * c(.2, 1))))),
          cbind(scenario="Pro-rich, uncertainty decreases", get.table(rbind(data.frame(ii=1:2, mm=1, cc=baseline * c(.2, 1)),
                                                                            data.frame(ii=1:2, mm=2, cc=baseline * c(.6, .6))))),
          cbind(scenario="Pro-poor, uncertainty increases", get.table(rbind(data.frame(ii=1:2, mm=1, cc=baseline * c(1, .6)),
                                                                            data.frame(ii=1:2, mm=2, cc=baseline * c(1, .2))))),
          cbind(scenario="Pro-poor, uncertainty decreases", get.table(rbind(data.frame(ii=1:2, mm=1, cc=baseline * c(1, .2)),
                                                                            data.frame(ii=1:2, mm=2, cc=baseline * c(.6, .6))))))

results2 <- melt(results, 'scenario') %>% group_by(scenario) %>% mutate(rank=rank(value))

ggplot(results2, aes(scenario, value)) +
    coord_flip() +
    geom_col(aes(fill=variable), position='dodge') +
    geom_label(aes(y=0, label=rank), size=2, position=position_dodge2(width=.9)) +
    scale_fill_discrete("Welfare calculation:") +
    ylab("Difference from no-impact baseline") +
    theme_bw()

results3 <- melt(results, 'scenario') %>% group_by(scenario) %>% mutate(No.Welfare=value[variable == "No.Welfare"],
                                                                        All.Welfare=value[variable == "All.Welfare"]) %>%
    reframe(calc=c('Average.of.Inequality', 'Inequality.of.Average'),
            value=(c(value[variable == "Average.of.Inequality"],
                     value[variable == "Inequality.of.Average"]) - No.Welfare[1]) / (All.Welfare[1] - No.Welfare[1]))

ggplot(results3, aes(scenario, value)) +
    coord_flip() +
    geom_col(aes(fill=calc), position='dodge') +
    scale_fill_discrete("Welfare calculation:") +
    ylab("Proportional of difference with no-impact baseline, between no-welfare and all-welfare") +
    theme_bw()

