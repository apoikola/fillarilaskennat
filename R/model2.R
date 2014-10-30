library("dplyr")
library("ggplot2")
#library("lubridate")
#library("tidyr")
library("gridExtra")
library("reshape2")

bike.raw <- tbl_df(read.table(file=pipe("python processed/parse.py"), sep=" ", header=T))
saveRDS(bike.raw, "bike.raw.rds")
bike.raw <- readRDS("bike.raw.rds")

bike.raw <- bike.raw %>% mutate(main.site=substr(site, 1, 3), sc=paste(site, channel, sep=":")) 

# Check parity
bike.raw %>%
group_by(main.site, date, day, weekday, yday) %>% summarise(count=sum(count)) %>% filter(!is.na(count)) %>%
  filter(count<150) %>%  mutate(parity=count %%2) %>% group_by(main.site, parity) %>% summarise(n=n()) %>% 
  dcast(main.site ~ parity)

# Leave site, channel
bike.day <- bike.raw %>% group_by(main.site, sc, date, day, weekday, yday) %>% 
  summarise(count=sum(count)) %>% filter(!is.na(count)) %>% ungroup()

load("fmi_weather/FMI_weather_Helsinki_PROCESSED_2004-01-01_2011-08-31.RData")
weather.df <- weather.df %>% filter(name=="Helsinki Kaisaniemi") %>% tbl_df()
d <- tbl_df(merge(bike.day, weather.df, by="date"))
d$snow[is.na(d$snow)] <- 0
d$sc <- as.factor(d$sc)
d$main.site <- as.factor(d$main.site) # FIXME -> transform

# FIXME
# - sc is useless
# - holidays, incl 23-12 manually
# - year
# - pakkanen plus sade
# - juuso: kirjoitusalustasta skypecallin issuet, parserin mahd. ongelmat, plotti


library(mgcv)

m6 <- gam(count ~ s(yday, k=30, bs="cc") + 
            s(tday, k=10) + #year + 
            rrday + snow + 
            I((snow>0)*rrday) +
            I(rrday==0)*weekday +
            I(snow==0)*weekday +
            s(main.site, bs="re") 
          , 
          family=nb(link="log"), optimizer="perf", data=d)   

