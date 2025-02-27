library(EnvStats)
library(ggplot2)

get.amoc <- function() {
    EL.2 <- rtri(1, 0.04, 0.46, 0.24 * 3 - (.04 + 0.46))
    EL.4 <- rtri(1, 0.17, 0.55, 0.39 * 3 - (.17 + 0.55))

    mode.2 <- EL.2 * 3 - 1
    if (mode.2 > 0 && mode.2 < 1) {
        L.2 <- rtri(1, 0, 1, EL.2 * 3 - 1)
    } else if (mode.2 < 0) {
        L.2 <- rtri(1, 0, 3 * EL.2 - .01, .01)
    } else if (mode.2 > 1)
        L.2 <- rtri(1, 3 * EL.2 - 1.99, 1, .99)

    mode.4 <- EL.4 * 3 - 1
    if (mode.4 > 0 && mode.4 < 1) {
        L.4 <- rtri(1, 0, 1, EL.4 * 3 - 1)
    } else if (mode.4 < 0) {
        L.4 <- rtri(1, 0, 3 * EL.4 - .01, .01)
    } else if (mode.4 > 1)
        L.4 <- rtri(1, 3 * EL.4 - 1.99, 1, .99)

    if (L.4 < L.2) {
        ## (T - d) b = -log(1 - L.4)
        dd <- 0
        bb <- -log(1 - (L.2 + L.4)/2) / (3 - dd) ## mean(c(-log(1 - L.2) / (2 - dd), -log(1 - L.4) / (4 - dd)))
    } else {
        ## exp(-b (T - d)) = 1 - L.T

        ## 4 b - b d = -log(1 - L.4)
        ## 2 b - b d = -log(1 - L.2)
        bb <- (-log(1 - L.4) + log(1 - L.2)) / 2
        dd <- (log(1 - L.4) + 4 * bb) / bb

        ## plot(TT, pmin(1, exp(-bb * (TT - dd))))

        if (dd < 0) {
            ## Instead use exp(-b T) = 1 - L.T
            dd <- 0
            bb <- -log(1 - (L.2 + L.4)/2) / (3 - dd) ## mean(c(-log(1 - L.2) / (2 - dd), -log(1 - L.4) / (4 - dd)))
        }
    }

    data.frame(L.2, L.4, bb, dd)
}

if (F) {
    TT <- seq(0, 6, length.out=100)

    pdf <- matrix(NA, 0, length(TT))
    results <- data.frame()
    for (ii in 1:1e3) {
        values <- get.amoc()

        results <- rbind(results, values)

        pdf <- rbind(pdf, pmin(1, exp(-values$bb * (TT - values$dd))))
    }

    pdf2 <- data.frame(TT, mu=colMeans(pdf),
                       ci05=apply(pdf, 2, function(xx) quantile(xx, .05)),
                       ci95=apply(pdf, 2, function(xx) quantile(xx, .95)))

    ggplot(pdf2, aes(TT, 1 - mu)) +
        geom_line() + geom_ribbon(aes(ymin=1 - ci05, ymax=1 - ci95), alpha=.5) +
        theme_bw() + xlab("Warming from pre-industrial (C)") + ylab("AMOC Weakening") +
        scale_y_continuous(labels=scales::percent)
                                        #geom_hline(yintercept=.24) + geom_hline(yintercept=.39)
}
