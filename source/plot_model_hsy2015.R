# Script for plotting the bike count model for
# HSY paikkatietoseminaari 2015

message("Need to run 'plot_model.R' first!")
source("source/plot_model.R")
theme_set(theme_bw(base_size = 16))
library("lubridate")
library("tidyr")

fig.folder <- "hsy_2015"


# Read and process raw data
bike.raw <- tbl_df(read.csv("data_clean/ kuu_20140927.csv"))
# bike.raw$Date <- ymd(bike.raw$Date)
bike.dat <- bike.raw %>%
  mutate(Data = ymd(Date),
         Year=year(Date),
         Day = yday(Date)) %>%
  mutate(WeekEnd=ifelse(WeekDay %in% c("Saturday", "Sunday"), TRUE, FALSE)) %>%
  filter(LocationID2 %in% c(107, 115) & Year %in% 2004:2009) %>%
  group_by(LocationID2, Year, Day, WeekEnd) %>%
  summarise(Count=sum(Value, na.rm=TRUE))

# Fix locations
fix.loc <- c("107"="Eläintarhanlahti", "115"="Eteläesplanadi")
bike.dat$LocationName <- fix.loc[as.character(bike.dat$LocationID2)]

# Load preprocessed weather data
load("fmi_weather/FMI_weather_Helsinki_PROCESSED_2004-01-01_2011-08-31.RData")


# 1 raw data

# Plot daily sums
ggplot(bike.dat, aes(x=Day, y=Count, colour=WeekEnd)) + 
  geom_point(size=1) + facet_grid(Year ~ LocationName) + 
  labs(y="Määrä", x="Päivä", colour="Viikonloppu") +
  scale_y_log10() + 
  theme(legend.position="top") + 
  ggsave(width=6, height=8, file=file.path(fig.folder, "raw_data.png"))

## 2 sample of bike data
bike.dat %>%
  filter(LocationName == "Eläintarhanlahti" & Year == 2007) %>%
  ggplot(data =., aes(x=Day, y=Count, colour=WeekEnd)) + 
  geom_point(size=1) + facet_grid(Year ~ LocationName) + 
  labs(y="Määrä", x="Päivä", colour="Viikonloppu") +
  scale_y_log10() + 
  theme(legend.position="none") + 
  ggsave(width=5, height=3, file=file.path(fig.folder, "raw_data_subset.png"))

## 3 sample of weather data
weather.df %>%
  mutate(date=ymd(date), Year = year(date), Day=yday(date)) %>%
  gather(Measurement, Value, tday:tmin) %>%
  mutate(Measurement = factor(Measurement, levels=c("tmax", "tday", "tmin"))) %>%
  filter(name == "Helsinki Kaisaniemi" & Year==2007) %>%
  ggplot(data =., aes(x=Day, y=Value, colour=Measurement)) + 
  geom_path() + facet_grid(Year ~ name) +
  theme(legend.position="none") + labs(x="Päivä", y="Lämpötila")
  ggsave(width=5, height=3, file=file.path(fig.folder, "weather_data_subset.png"))

## 4 model + data example
d2.subset <- droplevels(subset(d2, main.site.named=="1070\nEläintarhanlahti" & year %in% 2007))
levels(d2.subset$variable) <- c("raakadata", "sovitettu malli")
ggplot(d2.subset, aes(x=yday(date), y=Count, colour=variable)) + 
  geom_line(alpha=0.8) + 
  scale_y_log10() + theme(legend.position="bottom") +
  ggtitle("Mallinnettu data") +
  labs(x="Päivä", y="Määrä", colour="Data") +
  ggsave(width=10, height=4, file=file.path(fig.folder, "data+model.png"))


## 5 temperature
tday.df <- droplevels(subset(smooths, x.var == "tday\n(mean temperature for day)")) %>%
  ggplot(data = ., aes(x=x.val, y=value, colour=x.val)) + 
  geom_line(size=1.5) + 
  geom_line(aes(y=value + 2*se)) + #, linetype="dashed") + 
  geom_line(aes(y=value - 2*se)) +#, linetype="dashed") +
  scale_colour_gradient2(low=muted("blue"), mid="gray", high=muted("red")) + 
  scale_y_continuous(breaks=y.vals, labels=percent.vals-100, limits=range(smooths$value)) +
  geom_hline(y=0, linetype="dashed") + 
  labs(x="Lämpötila", y="Vaikutus (%) ± keskivirhe") +
  theme(legend.position="none") + 
  ggtitle("Lämpötilan vaikutus") +
  ggsave(width=4, height=4, file=file.path(fig.folder, "temperature.png"))

## 6 main effects: weather and time

percent.vals4 <- c(67, 80, 90, 100, 110, 125)
y.vals4 <- log(percent.vals4/100)

# Discrete vars
factors.discrete <- c("julyTRUE", "AnySnow", "holidayTRUE", "AnyRain")
main.discrete.df <- droplevels(subset(cm.df, Factor %in% factors.discrete))
levels(main.discrete.df$Factor) <- c("heinäkuu", "lunta maassa", "juhlapyhä", "sataa")
main.discrete.df$Group <- c("Aika", "Aika", "Sää", "Sää")
# levels(main.discrete.df$Factor) <- gsub(" \\(kyllä/ei\\)", "", levels(main.discrete.df$Factor))
main.discrete.df$Factor <- factor(main.discrete.df$Factor, levels=rev(main.discrete.df$Factor[order(main.discrete.df$Coefficient)]))
main.discrete.df$Group <- factor(main.discrete.df$Group, levels=c("Sää", "Aika"))
p.main.discrete <- ggplot(main.discrete.df, aes(y=Factor, x=Coefficient, xmin=Coefficient-SE, xmax=Coefficient+SE)) + 
  geom_point(size=3) + 
  geom_errorbarh(height=0) + 
  ggtitle("Sään ja ajankohdan vaikutus") + 
  labs(y=NULL, x="Vaikutus (%)") +
  scale_x_continuous(breaks=y.vals4, labels=percent.vals4-100) +
  geom_vline(x=0, linetype="dashed") +
  facet_grid(Group ~ ., scales="free_y", space="free_y") +
  theme(strip.text.y=element_text(angle=0)) +
  ggsave(width=4, height=4, file=file.path(fig.folder, "main_effects.png"))


# 7 weekdays
weekday.df <- droplevels(subset(cw.df, Factor %in% c("baseline", "AnyRain")))
levels(weekday.df$Weekday) <- c("sunnuntai", "lauantai", "perjantai", "torstai", 
                                "keskiviikko", "tiistai", "maanantai")
levels(weekday.df$Factor) <- c("muutos\nsateella", "lähtötaso")
ggplot(weekday.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE, colour=Weekday)) + 
  geom_point(size=3, position=position_dodge(width=0.7)) + 
  geom_errorbar(width=0, position=position_dodge(width=0.7)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals4, labels=percent.vals4-100) +  
  labs(x=NULL, y="Vaikutus (%)", colour="viikonpäivä") +
  ggtitle("Viikonpäivien vaikutus") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE)) + 
  ggsave(width=8, height=6, file=file.path(fig.folder, "weekday.png"))



# 8 location/site

site.df <- droplevels(subset(cms.df, Factor %in% c("baseline", "year", "holidayTRUE") &
                               main.site.named != "Tuntematon"))
site.df[site.df$Factor=="year", "Coefficient"] <- 10*site.df[site.df$Factor=="year", "Coefficient"]
site.df[site.df$Factor=="year", "SE"] <- 10*site.df[site.df$Factor=="year", "SE"]
levels(site.df$Factor)[3] <- "years_10"
levels(site.df$Factor) <- c("lähtötaso", "muutos\njuhlapyhänä", "muutos 10\nvuoden aikana")
site.df$Factor <- factor(site.df$Factor, levels=c("muutos 10\nvuoden aikana", "muutos\njuhlapyhänä", "lähtötaso"))

ggplot(site.df, aes(x=Factor, y=Coefficient, colour=main.site.named)) + 
  geom_point(size=3, position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymin=Coefficient-SE, ymax=Coefficient+SE), width=0, position=position_dodge(width=0.7)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3-100) +  
  labs(x=NULL, y="Vaikutus (%)", colour="sijainti") +
  ggtitle("Sijainnin vaikutus") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE)) + 
  ggsave(width=8, height=6, file=file.path(fig.folder, "location.png"))
