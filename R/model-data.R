library("dplyr")
library("ggplot2")
#library("lubridate")
#library("tidyr")
library("reshape2")


bike.raw <- tbl_df(read.table(file=pipe("python processed/parse.py"), sep=" ", header=T))
saveRDS(bike.raw, "processed/bike_raw.rds")
bike.raw <- readRDS("processed/bike_raw.rds")

if (F) {
  # Mistähän nämä duplikaatit tulevat? (Sama data monessa failissa.)
  bike.raw %>% select(main.site, date, hour, channel) %>% group_by(main.site, date, hour, channel) %>% 
    summarise(n=n()) %>% filter(n>1)
  #Source: local data frame [40,176 x 5]
  bike.raw %>% select(file, main.site, date, hour, channel) %>% group_by(file, main.site, date, hour, channel) %>% 
    summarise(n=n()) %>% filter(n>1) %>% ungroup() %>% select(file) %>% distinct()
  #1   data/2004_08_elokuu/11702620.204
  #2   data/2005_08_elokuu/11503128.205
  #3 data/2008_04_huhtikuu/10201418.208
}


load("Finnish_holidays.RData")
holidays <- c(paste(2000:2020, "-12-23", sep=""), unique(as.character(holidays.df$Date)))

load("fmi_weather/FMI_weather_Helsinki_PROCESSED_2004-01-01_2011-08-31.RData")
weather.df <- weather.df %>% filter(name=="Helsinki Kaisaniemi") %>% tbl_df()

bike.raw <- bike.raw %>% mutate(main.site=substr(site, 1, 4)) 
# Fix main.site 5260 -> 1030 
bike.raw$main.site[bike.raw$main.site=="5260"] <- "1030"
bike.raw <- bike.raw %>% distinct(main.site, date, hour, channel)


if (F) { # Check parity
  bike.raw %>%
    group_by(main.site, date, day, weekday, yday) %>% summarise(count=sum(count)) %>% filter(!is.na(count)) %>%
    filter(count<150) %>%  mutate(parity=count %%2) %>% group_by(main.site, parity) %>% summarise(n=n()) %>% 
    dcast(main.site ~ parity)
}

# Leave site, channel
bike.day <- bike.raw %>% group_by(file, main.site, date, day, weekday, yday) %>% 
  summarise(count=sum(count)) %>% filter(!is.na(count)) %>% ungroup()


d <- tbl_df(merge(bike.day, weather.df, by="date")) %>%
  mutate(snow=ifelse(is.na(snow), 0, snow), main.site=as.factor(main.site), year=as.numeric(substr(date, 1, 4))-2000) %>%
  mutate(holiday = date %in% holidays)

d <- d %>% mutate(earth.phase=sin(-pi/2+2*pi*((yday+12)%%366)/365))
#with(d, plot(yday, earth.phase, type="l"))
lagged.weather <- d %>% select(day, rrday, snow, tday, tmin, tmax) %>% 
  transmute(day=day+1, rrday1=rrday, snow1=snow, tday1=tday, tmin1=tmin, tmax1=tmax) %>% distinct()
d <- inner_join(lagged.weather, d, by="day") %>% mutate(dsnow=snow1-snow, dtemp=tday1-tday)


if (F) {
  m6 <- gam(count ~ s(yday, k=30, bs="cc") + 
              s(tday, k=10) + year + holiday +
              rrday + snow + 
              I((snow>0)*rrday) +
              I(rrday==0)*weekday +
              I(snow==0)*weekday +
              s(main.site, bs="re")
            , 
            family=nb(link="log"), optimizer="perf", data=d)   
  
  plot(resid(m6), type="l") 
  plot(qnorm((1:nrow(d))/nrow(d)), sort(resid(m6)), type="l"); abline(0, 1)
  hist(resid(m6), n=1000)
  
  
  dr <- d %>% mutate(res=resid(m6)) 
  rbad <- dr %>% filter(res< -4 | res>4) %>% group_by(file) %>%select(file, res) %>% 
    summarise(n=n(), res=mean(res)) 
  bad.files <- as.character(rbad$file)
  saveRDS(bad.files, "processed/bad_files.rds")
  if (F) message(paste(rbad$file, collapse=" ")) # for less
}

d <- d %>% filter(!(file %in% readRDS("processed/bad_files.rds")))

# FIXME
# - residuaalien korrelaatioita pitäis katsoa
# - plotti
# - kesäaika?
# - 526 ja muut pienet sitet?
# - lisää interaktioita
# - heinäkuu(n tienoo)

# Fix sites: filter small sites, change 5260 -> 1030
sort(table(d$main.site))
# 1223 1218 1222 1202 1140 1020 1120 1080 1200 1210 1180 1160 1030 1070 1170 1150 
#    0   19   22   27   66  454  480  597  611  659  822  963 1185 1608 1610 1681
d <- droplevels(filter(d, !(main.site %in% c("1223", "1218", "1222", "1202", "1140"))))

## Add july and near july
july.days <- c(paste("07-0", 1:9, sep=""), paste("07", 10:31, sep="-"))
july.dates <- paste(rep(2000:2020, each=length(july.days)), july.days, sep="-")
near.july.days <- c(paste("06", 23:30, sep="-"), paste("08-0", 1:9, sep=""), paste("08-", 10:14, sep=""))
near.july.dates <- paste(rep(2000:2020, each=length(near.july.days)), near.july.days, sep="-")
june.days <- c(paste("06-0", 1:9, sep=""), paste("06", 10:30, sep="-"))
june.dates <- paste(rep(2000:2020, each=length(june.days)), june.days, sep="-")
aug.days <- c(paste("08-0", 1:9, sep=""), paste("08", 10:31, sep="-"))
aug.dates <- paste(rep(2000:2020, each=length(aug.days)), aug.days, sep="-")

d <- mutate(d, july=date %in% july.dates, near.july=date %in% near.july.dates) #june=date%in%june.dates, aug=date%in%aug.dates

