# Script for running the GAM model for the bike count data

library("mgcv")

# Load data prepared in 'source/prepare_data.R'
d <- readRDS("model_data/data_prepared.rds")

m7 <- gam(count ~ #s(earth.phase, k=5) + 
            s(earth.phase, k=10) + # alt: s(yday, k=10) +
            s(tday, k=10) +  s(I(tmax-tmin), k=10) + # alt: tday + I(tmax-tmin) + ; alt2: s(tday, I(tmax-tmin), k=10)
            year + holiday + july + near.july +
            rrday + snow + I(pmax(0, dsnow)) + rrday1 + dtemp + 
            I((tday<0)*rrday) + 
            I((snow>0)*rrday) +
            I((tday<0)*rrday1) + 
            I((snow>0)*rrday1) +
            I(rrday!=0)*weekday + # AnyRain is more intuitive than NoRain
            I(snow!=0)*weekday + # AnySnow is more intuitive than NoSnow
            main.site +
            main.site + #s(main.site, bs="re") +
            year:(holiday + weekday + snow + rrday + tday + earth.phase) +
            earth.phase:(holiday + weekday + dtemp + I(tmax-tmin)) + # nää voi jättää poiskin
            main.site:(holiday + dtemp + I(tmax-tmin) + year) +
            s(main.site, earth.phase, bs="re") +
            s(main.site, weekday, bs="re") 
          # main.site:vars, mutta re ei onnistu kovin hyvin eli main.site pitää siistiä ensin
          , 
          family=nb(link="log"), optimizer="perf", data=d)   

summary(m7)
plot(m7)
plot(resid(m7), type="l") 
plot(qnorm((1:nrow(d))/nrow(d)), sort(resid(m7)), type="l"); abline(0, 1)
hist(resid(m7), n=1000)
anova(m7)

saveRDS(m7, "model_data/m7.rds")

