setwd("~/Dropbox/Tipping points in climate change economics/Calibration/damagefunc")

library(readxl)
library(splines)
library(quantreg)
library(ggplot2)
library(scales)

resdir <- "results-20201030"

header <- names(read_excel(file.path(resdir, "baseline.xlsx"), n_max=1))
df.85.tp <- read_excel(file.path(resdir, "withtp-rcp85-mktonly.xlsx"), skip=2, col_types='numeric', na="Error")
df.85.no <- read_excel(file.path(resdir, "notp-rcp85-mktonly.xlsx"), skip=2, col_types='numeric', na="Error")
df.45.tp <- read_excel(file.path(resdir, "withtp-rcp45-mktonly.xlsx"), skip=2, col_types='numeric', na="Error")
df.45.no <- read_excel(file.path(resdir, "notp-rcp45-mktonly.xlsx"), skip=2, col_types='numeric', na="Error")

header[2:10] # temps to 2100

obs <- data.frame(indep=c(unlist(df.45.no[,2:10]), unlist(df.85.no[,2:10])),
                  depen=c(unlist(df.45.tp[,2:10]), unlist(df.85.tp[,2:10])),
                  rcp=rep(c('RCP 4.5', 'RCP 8.5'), each=length(unlist(df.45.no[,2:10]))))

ggplot(obs, aes(indep, depen)) +
    geom_point(aes(colour=rcp), alpha=.25, size=.1) + geom_abline(slope=1, intercept=0, col=muted('red'), linetype='dashed') +
    theme_bw() + xlab("Temperature without tipping points") + ylab("Temperature with tipping points") +
    scale_colour_discrete(name=NULL) +
    scale_x_continuous(breaks=c(0, 2, 4, 6, 8), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 2, 4, 6, 8), expand=c(0, 0)) +
    guides(colour=guide_legend(override.aes=list(size=2, alpha=1))) +
    coord_cartesian(xlim=c(0, 8), ylim=c(0, 8))
ggsave("~/Open Modeling Group Dropbox/UK Economic Risks/climate/tippts/compare.png", height=4.5, width=6)

mcs <- read.csv("~/Open Modeling Group Dropbox/UK Economic Risks/climate/gsat/ssps_26_70_ukproject_allmembers.csv")

min(obs$indep[obs$indep > 0], na.rm=T) # 0.2478635
max(obs$indep[obs$indep < 8], na.rm=T) # 7.936453

matching <- data.frame()
for (indep in seq(0.35, 7.65, by=.1)) {
    rows <- which(obs$indep > indep - .05 & obs$indep < indep + .05)
    if (length(rows) < 10)
        print(indep)
    adds <- quantile((obs$depen - obs$indep)[rows], probs=((1:100) - 0.5) / 100, na.rm=T)
    matching <- rbind(matching, data.frame(indep, quantile=((1:100) - 0.5) / 100, adds))
    print(tail(matching))
}

write.csv(matching, "~/Open Modeling Group Dropbox/UK Economic Risks/climate/tippts/additions.csv", row.names=F)
