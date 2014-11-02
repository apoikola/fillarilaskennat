library("dplyr")
library("ggplot2")
#library("lubridate")
#library("tidyr")
library("gridExtra")
library("reshape2")
library("mgcv")

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
            s(main.site, bs="re") +
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

saveRDS(m7, "processed/m7.rds")

## PLOT M7 WITH GGPLOT2 ########

m7 <- readRDS("processed/m7.rds")

# Custom colour theme
# install.packages("ggthemes")
library("ggthemes")
theme_set(theme_bw(base_size = 16))
# theme_set(theme_solarized(light=FALSE))


# For plotting GAM smooths with ggplot2
# from http://stackoverflow.com/questions/19735149/is-it-possible-to-plot-the-smooth-components-of-a-gam-fit-with-ggplot2
source("R/functions.R")
model <- m7

# Use real main.site names
main.site.names <- c(
"1020"="Kuusisaarensilta",
"1030"="Kulosaarensilta",
"1070"="Eläintarhanlahti",
"1080"="Hesperian puisto",
"1120"="Veräjämäki",
"1150"="Eteläesplanadi",
"1160"="Kantelettarentie",
"1170"="Lauttasaaren silta",
"1180"="Kehä I, Vantaajoki",
"1200"="Nordenskiöldinpolku",
"1210"="Tuntematon"
)
d$main.site.named <- d$main.site
levels(d$main.site.named) <- paste(levels(d$main.site.named), main.site.names[levels(d$main.site.named)], sep="\n")
# Construct intuitive y scale
percent.vals <- c(25, 33, 50, 67, 100, 150, 200, 300)
y.vals <- log(percent.vals/100)

# Plot raw data and predicted model
d$predCount <- exp(predict.gam(model, d))
d2 <- melt(d[c("main.site.named", "date", "year", "yday", "count", "predCount")], id.vars = c("main.site.named", "date", "year", "yday"), value.name = "Count")
d2$year <- 2000 + d2$year
d2$date <- as.Date(d2$date)
levels(d2$variable) <- c("Raw", "Model fit")
p.rp <- ggplot(d2, aes(x=date, y=Count, colour=variable)) + 
  geom_line(alpha=0.8) + 
  facet_grid(main.site.named ~ .) +
  scale_y_log10() + 
  ggtitle("Raw data (red) and model fit (blue)") +
  theme(strip.text.y=element_text(angle=0), legend.position="none") 
# FIXME
# - remove lines over missing values

# Plot residuals
# percent.vals2 <- c(25, 50, 100, 200)
# y.vals2 <- log(percent.vals2/100)
resid.df <- data.frame(d, residuals=model$residuals)
resid.df$date <- as.Date(resid.df$date)
p.resid <- ggplot(resid.df, aes(x=date, y=residuals)) + 
  geom_line(alpha=0.8) + 
  facet_grid(main.site.named ~ .) +
  ggtitle("Residuals left") + 
  ylab("Effect on log_e scale") +
#  scale_y_continuous(breaks=y.vals2, labels=percent.vals2) +
  theme(strip.text.y=element_text(angle=0))

p.data <- arrangeGrob(p.rp, p.resid, ncol=1)
ggsave(plot=p.data, file="figures/Fillari_M7_data_v2.png", width=8, height=15)

# List biggest residuals
write.csv(head(resid.df[order(abs(resid.df$residuals), decreasing = T), ], 100), file="M7_residuals_v1.csv")

# Remove site effect from smooth plots
model2 <- model
model2$smooth <- model2$smooth[-4]
smooths <- EvaluateSmooths(model2)
# Desribe variables better
levels(smooths$x.var) <- paste(levels(smooths$x.var), c("(proxy for day length)", "(mean temperature for day)", "(proxy for cloudyness)"), sep="\n")
p.smooth <- ggplot(smooths, aes(x=x.val, y=value)) + geom_line() + 
  geom_line(aes(y=value + 2*se), linetype="dashed") + 
  geom_line(aes(y=value - 2*se), linetype="dashed") +
  scale_y_continuous(breaks=y.vals, labels=percent.vals, limits=range(smooths$value)) +
  geom_hline(y=0, linetype="dashed") + 
  facet_grid(. ~ x.var, scales="free_x") +
  labs(y="Effect (%)")


# Plot scalar coefficients and standard errors
percent.vals3 <- c(50, 67, 80, 100, 125, 150, 200)
y.vals3 <- log(percent.vals3/100)
smooth.param.inds <- unlist(lapply(model$smooth, function(x) x$first.para:x$last.para))
coefs <- model$coefficients[-smooth.param.inds]
ses <- summary(model)$se[-smooth.param.inds]
names(coefs) <- gsub("I\\(rrday != 0\\)TRUE", "AnyRain", names(coefs))
names(coefs) <- gsub("I\\(snow != 0\\)TRUE", "AnySnow", names(coefs))

# Extract weekday stuff
coefs.weekdays <- coefs[grep("weekday", names(coefs))]
ses.weekdays <- ses[grep("weekday", names(coefs))]
names(coefs.weekdays) <- gsub("weekday", "", names(coefs.weekdays))
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
temp <- strsplit(names(coefs.weekdays), split=":")
stopifnot(all(sapply(temp, length) <=2))
temp[sapply(temp, length)==1] <- lapply(temp[sapply(temp, length)==1], c, "baseline")
# Reorder
temp[!(sapply(temp, "[", 1) %in% weekdays)] <- lapply(temp[!(sapply(temp, "[", 1) %in% weekdays)], function(x) x[2:1])
# Process into data frame
cw.df <- data.frame(Weekday=sapply(temp, "[", 1), Factor=sapply(temp, "[", 2), Coefficient=coefs.weekdays, row.names=NULL, SE=ses.weekdays)
cw.df$Weekday <- factor(cw.df$Weekday, levels=weekdays)
cw.df$INDEX <- 1:nrow(cw.df)
p.cw <- ggplot(cw.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE, colour=Weekday)) + 
  geom_point(size=3, position=position_dodge(width=0.4)) + 
  geom_errorbar(width=0, position=position_dodge(width=0.4)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +  
  labs(x=NULL, y="Effect (%)") +
  ggtitle("Effects of weekdays\n(Friday is the baseline)") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE))
# + facet_wrap(~ Weekday, nrow=1) + theme(axis.text.x=element_text(angle=60, hjust=1)) 


# Extract the rest
coefs.other <- coefs[-grep("weekday", names(coefs))]
ses.other <- ses[-grep("weekday", names(coefs))]

# Extract the main effects
coefs.main <- coefs.other[-c(grep(":", names(coefs.other)), grep("\\*", names(coefs.other)))]
ses.main <- ses.other[names(coefs.main)]
# Remove Intercept for now (how to interpret?)
coefs.main <- coefs.main[-grep("Intercept", names(coefs.main))]
ses.main <- ses.main[-grep("Intercept", names(ses.main))]
cm.df <- data.frame(Factor=names(coefs.main), Coefficient=coefs.main, SE=ses.main)
cm.df$Factor <- factor(cm.df$Factor, levels=cm.df$Factor[order(cm.df$Coefficient)])
p.cm <- ggplot(cm.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE)) + 
  geom_point(size=3) + 
  geom_errorbar(width=0) + 
  ggtitle("Main effects") + 
  labs(x=NULL, y="Effect (%)") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +
  geom_hline(y=0, linetype="dashed") +
  coord_flip()

# Extract the interaction effects
coefs.interaction <- coefs.other[c(grep(":", names(coefs.other)), grep("\\*", names(coefs.other)))]
ses.interaction <- ses.other[names(coefs.interaction)]
ci.df <- data.frame(Factor=names(coefs.interaction), Coefficient=coefs.interaction, SE=ses.interaction)
ci.df$Factor <- factor(ci.df$Factor, levels=ci.df$Factor[order(ci.df$Coefficient)])
p.ci <- ggplot(ci.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE)) + 
  geom_point(size=3) + 
  geom_errorbar(width=0) + 
  ggtitle("Interaction effects") + 
  labs(x=NULL, y="Effect (%)") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +
  geom_hline(y=0, linetype="dashed") +
  coord_flip()

# Plot main site effects
site.inds <- model$smooth[[4]]$first.para:model$smooth[[4]]$last.para
site.coefs <- model$coefficients[site.inds]
site.ses <- summary(model)$se[site.inds]
site.df <- data.frame(Site=main.site.names, Effect=site.coefs, SE=site.ses)
site.df$Site <- factor(site.df$Site, levels=site.df$Site[order(site.df$Effect)])
p.site <- ggplot(site.df, aes(x=Site, y=Effect, ymin=Effect-SE, ymax=Effect+SE)) + 
  geom_point(size=3) + 
  geom_errorbar(width=0) + 
  ggtitle("Site effects") + 
  labs(x=NULL, y="Effect (%)") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +
  geom_hline(y=0, linetype="dashed") +
  coord_flip()
  
# Put together
p.f1 <- arrangeGrob(p.site, p.smooth, nrow=1, widths=c(1.5, 3))
p.f2 <- arrangeGrob(arrangeGrob(p.cm, p.ci, ncol=1), p.cw, nrow=1)
p.fillari <- arrangeGrob(p.f1, p.f2, ncol=1, heights=c(1.2, 2))
ggsave(plot=p.fillari, file="figures/Fillari_M7_model_v2.png", width=12, height=10)

