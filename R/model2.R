library("dplyr")
library("ggplot2")
#library("lubridate")
#library("tidyr")
library("gridExtra")
library("reshape2")

bike.raw <- tbl_df(read.table(file=pipe("python processed/parse.py"), sep=" ", header=T))
saveRDS(bike.raw, "bike.raw.rds")
bike.raw <- readRDS("bike.raw.rds")

load("Finnish_holidays.RData")
holidays <- c(paste(2000:2020, "-12-23", sep=""), unique(as.character(holidays.df$Date)))

load("fmi_weather/FMI_weather_Helsinki_PROCESSED_2004-01-01_2011-08-31.RData")
weather.df <- weather.df %>% filter(name=="Helsinki Kaisaniemi") %>% tbl_df()

bike.raw <- bike.raw %>% mutate(main.site=substr(site, 1, 3), sc=paste(site, channel, sep=":")) 

if (F) { # Check parity
  bike.raw %>%
    group_by(main.site, date, day, weekday, yday) %>% summarise(count=sum(count)) %>% filter(!is.na(count)) %>%
    filter(count<150) %>%  mutate(parity=count %%2) %>% group_by(main.site, parity) %>% summarise(n=n()) %>% 
    dcast(main.site ~ parity) }
  
# Leave site, channel
bike.day <- bike.raw %>% group_by(file, main.site, date, day, weekday, yday) %>% 
  summarise(count=sum(count)) %>% filter(!is.na(count)) %>% ungroup()


d <- tbl_df(merge(bike.day, weather.df, by="date")) %>%
  mutate(snow=ifelse(is.na(snow), 0, snow), main.site=as.factor(main.site), year=as.numeric(substr(date, 1, 4))-2000) %>%
  mutate(holiday = date %in% holidays)


# FIXME
# - pakkanen plus sade
# - plotti


library(mgcv)

m6 <- gam(count ~ s(yday, k=30, bs="cc") + 
            s(tday, k=10) + year + holiday +
            rrday + snow + 
            I((snow>0)*rrday) +
            I(rrday==0)*weekday +
            I(snow==0)*weekday +
            s(main.site, bs="re") 
          , 
          family=nb(link="log"), optimizer="perf", data=d)   

plot(resid(m6), type="l") # V* mitä skeidaa tuolla datassa välillä
hist(resid(m6), n=1000)

# bayesilaisessa mallissa voisi olla 'laite paskana' -indikaattori nollille, joita tuolla on paljon.
# kesäaika?


dr <- d %>% mutate(res=resid(m6)) 
rbad <- dr %>% filter(res< -4 | res>4) %>% group_by(file) %>%select(file, res) %>% 
  summarise(n=n(), res=mean(res)) 
rbad %>% filter(n>3)
# Source: local data frame [11 x 3]
# 
# file  n       res
# 1  data/2004_05_toukokuu/12001908.204  5  6.531376
# 2  data/2004_07_heinakuu/11702320.204 29 -6.180333
# 3   data/2005_09_syyskuu/11703426.205  4  5.718892
# 4   data/2006_06_kesakuu/11202318.206  4 -5.651203
# 5   data/2006_10_lokakuu/10804001.206  5 -5.099787
# 6  data/2007_07_heinakuu/10202615.207  5 -5.690938
# 7  data/2008_04_huhtikuu/12101415.208 16  8.861227
# 8  data/2008_07_heinakuu/10202618.208  5 -5.854549
# 9  data/2008_07_heinakuu/11202611.208 10 -4.880154
# 10  data/2008_09_syyskuu/10803614.208 20 -5.709432
# 11 data/2009_07_heinakuu/10802503.209  5 -5.684332
message(paste(rbad$file, collapse=" ")) # for less

# filter by date, site
# nollacountit pois?
# 526 pois?

