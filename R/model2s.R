library(rstan)
library(dplyr)
library(ggplot2)

d <- (function () { source("R/model-data.R", local=T); environment()})()$d


m <- stan_model("R/model2s.stan")
s <- sampling(m, chains=1, iter=500, seed=3, 
              data=list(
                N=nrow(d),  
                count=d$count, tday=d$tday),
              #pars=c("ktemp", "iphi", "intercept"),
              refresh=10)#, control=list(metric="diag_e", stepsize_jitter=0.3))
plot(s)
traceplot(s)
traceplot(s, inc_warmup=F)
