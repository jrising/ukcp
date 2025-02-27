setwd("~/Dropbox/COACCH/compare")

library(dplyr)
library(lfe)
library(ggplot2)

df1 <- read.csv("results.csv")
df1$Method[df1$Source == "Bayesian Combo"] <- "This Meta-Analysis"

df1.ssp370 <- df1[df1$Channel != "Missing", c(1:5, 6:8)]
names(df1.ssp370)[6:8] <- c('q05', 'mu', 'q95')
df1.ssp370$ssp <- 'SSP370'

df1.ssp126 <- df1[df1$Channel != "Missing", c(1:5, 9:11)]
names(df1.ssp126)[6:8] <- c('q05', 'mu', 'q95')
df1.ssp126$ssp <- 'SSP126'

df2 <- rbind(df1.ssp370, df1.ssp126)
df3 <- df2 %>% left_join(data.frame(ssp=rep(c('SSP126', 'SSP370'), each=3),
                                    Years=rep(c('2011-2030', '2041-2060', '2081-2100'), 2),
                                    cdiff=c(1, 2, 2, 1, 2, 4),
                                    ydiff=c(0, 30, 70, 0, 30, 70)), by=c('Years', 'ssp'))

df3$cdiff2 <- df3$cdiff^2
df3$cid <- paste(df3$Channel, df3$Contribute, df3$Source)

summary(felm(mu ~ cdiff + cdiff2 | cid, data=df3))
summary(felm(mu ~ cid : cdiff + cid : cdiff2 | cid, data=df3))
summary(felm(mu ~ cid : cdiff + cid : cdiff2 + ydiff : ssp | cid, data=df3))

## Find the best temperatures to use
soln <- optim(c(1, 2, 2, 1, 2, 4), function(cdiff) {
    df3 <- df2 %>% left_join(data.frame(ssp=rep(c('SSP126', 'SSP370'), each=3),
                                        Years=rep(c('2011-2030', '2041-2060', '2081-2100'), 2),
                                        cdiff=cdiff,
                                        ydiff=c(0, 30, 70, 0, 30, 70)), by=c('Years', 'ssp'))

    df3$cdiff2 <- df3$cdiff^2
    df3$cid <- paste(df3$Channel, df3$Contribute, df3$Source)

    mod <- felm(mu ~ cid : cdiff + cid : cdiff2 + ydiff : ssp | cid, data=df3)
    sqrt(mean(resid(mod)^2)) # RMSE
})

df3 <- df2 %>% left_join(data.frame(ssp=rep(c('SSP126', 'SSP370'), each=3),
                                    Years=rep(c('2011-2030', '2041-2060', '2081-2100'), 2),
                                    cdiff=soln$par,
                                    ydiff=c(0, 30, 70, 0, 30, 70)), by=c('Years', 'ssp'))

df3$cdiff2 <- df3$cdiff^2
df3$cid <- paste(df3$Channel, df3$Contribute, df3$Source)

mod <- felm(mu ~ cid : cdiff + cid : cdiff2 + ydiff : ssp | cid, data=df3)
summary(mod)

cres <- data.frame(coeff1=coef(mod)[1:(length(coef(mod)) / 2 - 1)],
                   coeff2=coef(mod)[((length(coef(mod)) / 2 - 1)+1):(length(coef(mod)) - 2)])
cres$scale1 <- abs(cres$coeff1 * 2 + cres$coeff2 * 4)
cres$scale2 <- abs(cres$coeff1 * 4 + cres$coeff2 * 16)
cres$cid <- sapply(rownames(cres), function(ss) substring(ss, 4, nchar(ss) - 6))

df4 <- df1 %>% group_by(Channel, Source, Contribute, Method) %>% summarize(cid=paste(Channel, Contribute, Source)[1])
df5 <- df4 %>% left_join(cres, by='cid')

summary(lm(log(scale1) ~ 0 + Method, data=df5))
summary(lm(log(scale2) ~ 0 + Method, data=df5))

df5$Method <- factor(df5$Method, levels=c("This Meta-Analysis", as.character(unique(df5$Method)[unique(df5$Method) != "This Meta-Analysis"])))
mod <- lm(log(scale1) ~ Method + Channel, data=df5)
summary(mod)

library(stargazer)
stargazer(mod, style='ajps')

df6 <- melt(df5[, c('Channel', 'Method', 'scale1', 'scale2')], c('Channel', 'Method'))
df6$TT <- 2
df6$TT[df6$variable == 'scale2'] <- 4

df6$Method <- factor(df6$Method, levels=c("This Meta-Analysis", as.character(unique(df6$Method)[unique(df6$Method) != "This Meta-Analysis"])))
mod <- lm(log(value) ~ Method * TT + Channel, data=df6)
summary(mod)

ggplot(df5, aes(Method, scale1)) +
    coord_flip() +
    geom_point(aes(colour=Channel)) +
    scale_y_log10("Scale of impact (absolute modeled effect at 2 C)") + theme_bw()

mod$coefficients[1] <- 0
exp(predict(mod, data.frame(Method=levels(df5$Method), Channel="Agriculture"), interval='confidence'))

