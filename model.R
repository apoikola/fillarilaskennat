library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("gridExtra")


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

# Change NA's in snow to 0
d$snow[is.na(d$snow)] <- 0
stopifnot(!any(is.na(d)))

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
ar(resid(m3)) # Päiville 1 ja 2 selvä efekti, myös edelleen joku viikkoefekti (vuodenaikainteraktio?)
# Vielä korrelaatiot selittäjien kanssa

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
            rrday + snow + 
            I((snow>0)*rrday) +
            I(rrday==0)*weekday +
            I(snow==0)*weekday 
          , 
          family=nb(link="log"), optimizer="perf", data=d)   

## Plot m5 with ggplot2 
# from http://stackoverflow.com/questions/19735149/is-it-possible-to-plot-the-smooth-components-of-a-gam-fit-with-ggplot2
source("functions.R")
smooths <- EvaluateSmooths(m5)
# p.smooth <- ggplot(smooths, aes(x.val, value)) + 
#   geom_line() + 
#   geom_line(aes(y=value + 2*se), linetype="dashed") + 
#   geom_line(aes(y=value - 2*se), linetype="dashed") + 
#   facet_grid(. ~ x.var, scales="free")
p.day <- ggplot(subset(smooths, x.var=="Day"), aes(x=x.val, y=value))+ geom_line() + 
  geom_line(aes(y=value + 2*se), linetype="dashed") + 
  geom_line(aes(y=value - 2*se), linetype="dashed") +
  labs(x="Day of the year", y="Coefficient")
p.temp <- ggplot(subset(smooths, x.var=="tday"), aes(x=x.val, y=value))+ geom_line() + 
  geom_line(aes(y=value + 2*se), linetype="dashed") + 
  geom_line(aes(y=value - 2*se), linetype="dashed") +
  labs(x="Temperature (day mean)", y="Coefficient")
p.smooth <- arrangeGrob(p.day, p.temp, nrow=1, main="Effect of day and temperature")

# Plot the rest of the coefficients
coefs <- m5$coefficients[-unlist(lapply(m5$smooth, function(x) x$first.para:x$last.para))]
names(coefs) <- gsub("I\\(rrday == 0\\)TRUE", "NoRain", names(coefs))
names(coefs) <- gsub("I\\(snow == 0\\)TRUE", "NoSnow", names(coefs))
# Extract weekday stuff
coefs.weekdays <- coefs[grep("weekday", names(coefs))]
names(coefs.weekdays) <- gsub("weekday", "", names(coefs.weekdays))
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
temp <- strsplit(names(coefs.weekdays), split=":")
stopifnot(all(sapply(temp, length) <=2))
temp[sapply(temp, length)==1] <- lapply(temp[sapply(temp, length)==1], c, "-")
# Reorder
temp[!(sapply(temp, "[", 1) %in% weekdays)] <- lapply(temp[!(sapply(temp, "[", 1) %in% weekdays)], function(x) x[2:1])
# Process into data frame
cw.df <- data.frame(Weekday=sapply(temp, "[", 1), Factor=sapply(temp, "[", 2), Coefficient=coefs.weekdays, row.names=NULL)
cw.df$Weekday <- factor(cw.df$Weekday, levels=weekdays)
p.cw <- ggplot(cw.df, aes(x=Factor, y=Coefficient)) + geom_bar(stat="identity") + facet_wrap(~ Weekday, nrow=1) + ggtitle("Effects of weekdays")
  
# Extract the rest
coefs.other <- coefs[-grep("weekday", names(coefs))]
# Remove Intercept for now (how to interpret?)
coefs.other <- coefs.other[-grep("Intercept", names(coefs.other))]
co.df <- data.frame(Factor=names(coefs.other), Coefficient=coefs.other)
p.co <- ggplot(co.df, aes(x=Factor, y=Coefficient)) + geom_bar(stat="identity") + ggtitle("Other effects")

# Plot residuals
resid.df <- data.frame(d, residuals=m5$residuals)
p.r <- ggplot(resid.df, aes(x=Day, y=residuals)) + geom_line() + facet_grid(Year ~ ., scales="free") + ggtitle("Residuals left")

# Put together
p.fillari <- arrangeGrob(p.smooth, p.cw, p.co, p.r, ncol=1, heights=c(0.2, 0.2, 0.2, 0.4))
ggsave(plot=p.fillari, file="figures/Fillari_M5.png", width=8, height=15)

