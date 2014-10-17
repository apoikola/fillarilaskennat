# Visualize data

library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")

## VISUALIZE FILLARIDATA ########

# Read fillarilaskennat data
bike.raw <- tbl_df(read.csv("output/ kuu_20140927.csv"))
bike.raw$Date <- ymd(bike.raw$Date)

# Compute sums over years, months, weeks
bike.dat <- list()
bike.dat$Year <- bike.raw %>%
  mutate(Year=year(Date)) %>%
  group_by(LocationID2, Year) %>%
  summarise(Count=sum(Value, na.rm=TRUE))

ggplot(bike.dat$Year, aes(x=Year, y=log10(Count))) + geom_point() + facet_wrap(~LocationID2)

# Compute monhtly sums
bike.dat$Month <- bike.raw %>%
  mutate(Year=year(Date), Month = month(Date)) %>%
  # mutate(Month = paste(year(Date), month(Date), sep="-")) %>%
  group_by(LocationID2, Year, Month) %>%
  summarise(Count=sum(Value, na.rm=TRUE))

# Plot monhtly
p.ym <- ggplot(bike.dat$Month, aes(x=Month, y=log10(Count))) + 
  geom_point() + facet_grid(Year~LocationID2) + 
  ggtitle("RAW bike data, monthly counts by year and LocationID2") + 
  scale_x_continuous(breaks=seq(3,12,3))
# ggsave(plot=p.ym, width=12, height=8, file="figures/Bikedata_RAW_MonthlyCounts_Year-Location_20141017.png")

# ggplot(bike.dat$Month, aes(x=Month, y=log10(Count), colour=Year)) + geom_point() + facet_wrap(~LocationID2)
# ggplot(bike.dat$Month, aes(x=Month, y=log10(Count), colour=Year)) + geom_path(aes(group=Year)) + facet_wrap(~LocationID2)

# Compute weekly sums
bike.dat$Week <- bike.raw %>%
  mutate(Year=year(Date), Week = week(Date)) %>%
  group_by(LocationID2, Year, Week) %>%
  summarise(Count=sum(Value, na.rm=TRUE))

# Plot weekly sums
p.yw <- ggplot(bike.dat$Week, aes(x=Week, y=log10(Count))) + 
  geom_point() + facet_grid(Year~LocationID2) +
  ggtitle("RAW bike data, weekly counts by year and LocationID2")  
ggsave(plot=p.yw, width=12, height=8, file="figures/Bikedata_RAW_WeeklyCounts_Year-Location_20141017.png")

# Compute daily sums for a subset of the data
bike.dat$Day <- bike.raw %>%
  mutate(Year=year(Date), Day = yday(Date)) %>%
  mutate(WeekEnd=ifelse(WeekDay %in% c("Saturday", "Sunday"), TRUE, FALSE)) %>%
  filter(LocationID2 %in% c(107, 115) & Year %in% 2004:2009) %>%
  group_by(LocationID2, Year, Day, WeekEnd) %>%
  summarise(Count=sum(Value, na.rm=TRUE))

# Plot daily sums
p.yd <- ggplot(bike.dat$Day, aes(x=Day, y=log10(Count), colour=WeekEnd)) + 
  geom_point(size=1) + facet_grid(Year~LocationID2) +
  ggtitle("RAW bike data, daily counts by year and LocationID2 (subset)")  
ggsave(plot=p.yd, width=8, height=8, file="figures/Bikedata_RAW_DailyCounts_Year-Location_Subset_20141017.png")



## VISUALIZE WEATHER DATA ###########

# Load weather data, processed in "fmi_weather/get_fmi_data.R"
load("fmi_weather/FMI_weather_Helsinki_PROCESSED_2004-01-01_2011-08-31.RData")

# Add Year and Day
weather.df <- weather.df %>%
  mutate(date=ymd(date), Year = year(date), Day=yday(date))

# Process temperature data
temp.df <- gather(weather.df, Measurement, Value, tday:tmin)
temp.df$Measurement <- factor(temp.df$Measurement, levels=c("tmax", "tday", "tmin"))
# Plot temperature (min, average, max)
p.t <- ggplot(temp.df, aes(x=Day, y=Value, colour=Measurement)) + 
  geom_path() + facet_grid(Year ~ name)
  
ggsave(plot=p.t, width=15, height=10, file="figures/Weatherdata_RAW_DailyTemperature_Year-Location_20141017.png")


# Plot rain and snow (only available for a subset of the locations)
rain.df <- weather.df %>%
  filter(name %in% c("Helsinki Kaisaniemi", "Helsinki Kumpula"))
rain.df <- gather(rain.df, Measurement, Value, rrday:snow)
p.rs <- ggplot(rain.df, aes(x=Day, y=Value, colour=Measurement)) + 
  geom_path() + facet_grid(Year ~ name)
ggsave(plot=p.rs, width=10, height=10, file="figures/Weatherdata_RAW_DailyRain+Snow_Year-Location_20141017.png")

## PLOT TOGETHER #############

# Pick one location from both
# Eteläesplanadi from bike data
b.df <- bike.raw %>%
  filter(LocationID2 == "115") %>%
  group_by(Date) %>%
  summarise(BikeCountLog10=log10(sum(Value, na.rm=TRUE)))
b.df$BikeCountLog10[b.df$BikeCountLog10==-Inf] <- NA
# Kaisaniemi from weather data
w.df <- weather.df %>%
  filter(name == "Helsinki Kaisaniemi") %>%
  select(date, tday)

# Combine by date
combined.df <- merge(b.df, w.df, by.x="Date", by.y="date", all=TRUE)

# Plot original values
c.df <- gather(combined.df, Measurement, Value, BikeCountLog10:tday)
p.comb.orig <- ggplot(c.df, aes(x=Date, y=Value)) + geom_point(size=1) + facet_wrap(~Measurement,nrow=2, scales="free")
ggsave(plot=p.comb.orig, width=12, height=8, file="figures/Bike+Weather_Original_temp1_20141017.png")


# Normalize and plot together
cn.df <- combined.df %>%
  mutate(BikeCountLog10Norm = scale(BikeCountLog10), TDayNorm=scale(tday)) %>%
  select(-(BikeCountLog10:tday))
cn.df <- gather(cn.df, Measurement, Value, BikeCountLog10Norm:TDayNorm)
p.comb.norm <- ggplot(cn.df, aes(x=Date, y=Value, colour=Measurement)) + 
  geom_path(alpha=0.8) +
  theme(legend.position="bottom")
ggsave(plot=p.comb.norm, width=15, height=5, file="figures/Bike+Weather_Normalized_temp1_20141017.png")


