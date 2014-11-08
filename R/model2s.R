library(rstan)
library(dplyr)
library(ggplot2)

d <- (function () { source("R/model-data.R", local=T); environment()})()$d

# Weak penalization.
Xmain <- model.matrix(~ 0 + main.site + I(year/10) + I(year/10):main.site + 
                        holiday + weekday + july + near.july +
                        I(snow/50) + I(rrday/50) + I(rrday1/50) + I((tmax-tmin)/10) +
                        I(snow==0) + I(rrday==0) + I(rrday1==0) +
                        earth.phase, d) 

# Penalized interactions will be in a separate matrix.


m <- stan_model("R/model2s.stan")
s <- sampling(m, chains=1, iter=500, seed=3, 
              data=list(
                N=nrow(d), Nmain=ncol(Xmain), Xmain=Xmain,
                count=d$count, tday=d$tday),
              #pars=c("ktemp", "iphi", "intercept"),
              refresh=1, control=list(metric="diag_e", stepsize_jitter=0.3))
plot(s)
traceplot(s)
traceplot(s, c("iphi", "ktemp"), inc_warmup=F)
