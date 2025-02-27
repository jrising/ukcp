## How to estimate standard errors

## Test case: If only high and low values

imp.low <- 0.034884121
imp.high <- 0.062307406

result <- optim(c((imp.low + imp.high) / 2, imp.high - imp.low), function(params) {
    mu <- params[1]
    sigma <- params[2]

    ((mu + qnorm(0.95, 0, 1) * sigma) - imp.high)^2 +
        ((mu + qnorm(0.05, 0, 1) * sigma) - imp.low)^2
})

## Case: If all 6 values

fit.normal <- function(impacts, levels) {
    ## impacts <- c(0.240288392, 0.253324598, 0.160878509, 0.106468417, 0.097408734, -0.022463165)
    ## levels <- c(1+1, 1+0, 1+-1, -1+1, -1+0, -1+-1)

    dq <- (.95 - 0.5) / 2

    optim(c(mean(impacts), sd(impacts)), function(params) {
        mu <- params[1]
        sigma <- abs(params[2])

        sum((qnorm(0.5 + dq * levels, mu, sigma) - impacts)^2)
    })
}

##Using data from COACCH dataset

df <- read.csv("~/Open Modeling Group Dropbox/COACCH/COACCH_ICES-CMCC_data/COACCH_data.csv")
subdf <- subset(df, Variable == "GDP"  & RCP == "RCP8.5") 
subUK <-subdf[grepl("^UK", subdf$Geo_ID),]  # <-- limit to UK..

saved<- data.frame()

for (item in unique(subUK$Item)) {
  for (region in unique(subUK$Geo_ID)) {
    for (year in seq(2015, 2070, by=5)) {
        values <- suppressWarnings(as.numeric(subUK[subUK$Item == item & subUK$Geo_ID == region, 
                                                   paste0('X', year)]))
        values <- values[!is.na(values)]
        if (length(unique(values))==1 || diff(range(values)) < 1e-8){
          result <-list(par=c(unique(values), 0))
          
        } else if (length(unique(values))==2){
          result <-fit.normal(sort(unique(values)), c(-1,1))
          
        } else if (length(values)<8){
          result <- fit.normal(values, c(1+1, 1+0, 1+-1, -1+1, -1+0, -1+-1))
            
        } else {
          result <- fit.normal(values, c(1+1, 1+0, 1+-1, -1+1, -1+0, -1+-1, 1+-1.1, -1+-1.1))
        }
        saved = rbind(saved,data.frame(impact=item, mu=result$par[1], se=result$par[2], region))
    }
  }
}
write.csv(saved, "impact-ses.csv")
