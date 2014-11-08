library(rstan)
library(dplyr)
library(ggplot2)

d <- (function () { source("R/model-data.R", local=T); environment()})()$d


m <- stan_model("R/model2s.stan")
s <- sampling(m, chains=1, iter=500, init=0,
              data=list(
                N=nrow(d), Nsite=nlevels(d$main.site), 
                count=d$count, tday=d$tday, site=as.integer(d$main.site), holiday=as.integer(d$holiday), 
                weekday=as.integer(d$weekday)
                ), 
              pars=c("ktemp", "phi")
           )