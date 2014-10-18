library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")

bike.raw <- tbl_df(read.csv("output/ kuu_20140927.csv"))
bike.raw$Date <- ymd(bike.raw$Date)

load("fmi_weather/FMI_weather_Helsinki_PROCESSED_2004-01-01_2011-08-31.RData")
weather.df <- tbl_df(weather.df) %>%
  mutate(date=ymd(date), Year = year(date), Day=yday(date))

d <- tbl_df(merge(
  bike.raw %>% filter(LocationID2 == "115"),
  weather.df %>% filter(name == "Helsinki Kaisaniemi"),
  by.x="Date", by.y="date", all=TRUE))

b.df <- bike.raw %>% filter(LocationID2 == "115") %>% group_by(Date) %>% summarise(weekday=WeekDay[[1]], count=sum(Value))
w.df <- weather.df %>% filter(name == "Helsinki Kaisaniemi") %>% mutate(Date=date) %>% select(-name, -date)

d <- inner_join(w.df, b.df, by="Date")

d <- filter(d, count<5000) # Is this ok? Negbin is not enough to handle extreme outliers, maybe

library(mgcv)

m1 <- gam(count ~ s(tday, k=10), family=nb(link="log"), optimizer="perf", data=d)
plot(m1)

m2 <- gam(count ~ s(tday, k=10) + as.factor(Year) + s(Day), family=nb(link="log"), optimizer="perf", data=d)                                                                           

m3 <- gam(count ~ 
            s(tday, k=10) + Year + s(Day, bs="cc", k=100) +
            I(snow==0) +  snow +
            I(rrday==0) + rrday +
            I((snow>0)*rrday) +
            weekday
          , 
          family=nb(link="log"), optimizer="perf", data=d)                                                                           
m3
summary(m3)
# rday ja snow on kokeiltu epälineaarisena
plot(m3, shade=T) # t lähes lineaarinen
hist(resid(m3), n=100)
plot(resid(m3), type="l") # Alussa paskaa?, muutama paha piikki vielä
plot(resid(m3)[d$Year==2004], type="l") # Vuodenaika-viikonpäivä-interaktio!
plot(resid(m3)[d$Year==2005], type="l") 
plot(resid(m3)[d$Year==2006], type="l") # Talvella jotain viikkojutskaa
plot(resid(m3)[d$Year==2007], type="l") 
acf(resid(m3))
<<<<<<< HEAD
ar(resid(m3)) # Päiville 1 ja 2 selvä efekti, myös edelleen joku viikkoefekti (vuodenaikainteraktio?)
# Vielä korrelaatiot selittäjien kanssa
=======
ar(resid(m3))
>>>>>>> 94d7db5faca793970c947a9303c63a15ae7752e6

# TODO
# - tmin, tmax
# - lagit jotenkin (stan, tai kokeillaan lumi ym. taaksepäin?)
# - muut mittauspisteet, entä toinen lämpötilasetti?
# - 

m4 <- gam(count ~ 
            s(tday, k=10) + Year + s(Day, bs="cc", k=100) +
            rrday*weekday +
            I(rrday==0)*weekday +
            snow*weekday +
            I(snow==0)*weekday
          , 
          family=nb(link="log"), optimizer="perf", data=d)    
# Clear weekday snow interaction
# Viikko-lägitys-efekti katosi, tai ainakin pieneni
spikeSlabGAM?

m5 <- gam(count ~ s(Day, k=5, bs="cc") + 
            s(tday, k=10) + Year + 
            rrday*weekday +
            I(rrday==0)*weekday +
            snow*weekday +
            I(snow==0)*weekday
          , 
          family=nb(link="log"), optimizer="perf", data=d)   
