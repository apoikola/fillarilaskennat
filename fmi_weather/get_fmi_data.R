## Script for downloading FMI weather data for fillarilaskennat

## INSTALL FMI PACKAGE #########

# Follow instructions in https://github.com/rOpenGov/fmi/blob/master/vignettes/fmi_tutorial.md

# For OSX, install GDAL and rgdal based on this: http://www.kyngchaos.com/software/frameworks

# Install rwfs and fmi from ropengov github
install.packages(c("devtools"))
library(devtools)
install_github("rOpenGov/rwfs")
install_github("rOpenGov/fmi")

message("Note! You may have to open Rstudio with 'open -a rstudio' to get ogr2ogr work!")

## GET DATA #########

library("fmi")
library("rgdal")

# Read your own api key from file (ignored in git!)
apiKey <- scan(file="fmi_weather/fmi_api.txt", what="character")

# Construct a request object
request <- FMIWFSRequest(apiKey=apiKey)
client <- FMIWFSClient()

# Default bounding box gives whole Finland
response <- client$getDailyWeather(request=request, startDateTime=as.POSIXlt("2014-01-01"), endDateTime=as.POSIXlt("2014-01-01"))

# Use bounding box for Helsinki
response <- client$getDailyWeather(request=request, startDateTime=as.POSIXlt("2014-01-01"), endDateTime=as.POSIXlt("2014-01-01"), bbox=raster::extent(c(24.831249,60.130150,25.202221,60.289291)))

# # Giving a range for dates results in a funny data format
# response <- client$getDailyWeather(request=request, startDateTime=as.POSIXlt("2014-01-01"), endDateTime=as.POSIXlt("2014-01-02"), bbox=raster::extent(c(24.831249,60.130150,25.202221,60.289291)))

# => Fetch data for each date separately instead
# Get all dates between 2004-01 and 2011-08
library("lubridate")
dates <- ymd("2004-01-01") + days(0:(7*365+244))
# Check
range(dates)
# [1] "2004-01-01 UTC" "2011-08-31 UTC"

# dates2 <- dates[-(1:504)]
# dates3 <- dates[-(1:1275)]
# dates4 <- dates[-(1:1954)]

weather.raw <- list()
for (i in 1:length(dates)) {
  weather.raw[[i]] <- client$getDailyWeather(request=request, startDateTime=as.POSIXlt(dates[i]), endDateTime=as.POSIXlt(dates[i]), bbox=raster::extent(c(24.831249,60.130150,25.202221,60.289291)))
  # Set one second delay to keep your computer and FMI happy
  Sys.sleep(1)
}
names(weather.raw) <- dates

# I actually fetched the data in four pieces
# save(weather.raw1, file="FMI_weather_Helsinki_2004-01-01_2005-05-18.RData")
# save(weather.raw2, file="FMI_weather_Helsinki_2005-05-19_2007-06-28.RData")
# save(weather.raw3, file="FMI_weather_Helsinki_2007-06-29_2009-05-07.RData")
# save(weather.raw4, file="FMI_weather_Helsinki_2009-05-08_2011-08-31.RData")
# weather.raw <- c(weather.raw1, weather.raw2, weather.raw3, weather.raw4)

save(weather.raw, file="FMI_weather_Helsinki_RAW_2004-01-01_2011-08-31.RData")
  
  
## PROCESS FOR CLAIMS #######

library("reshape2")

# Load raw weather data for 2013 09-12
load("FMI_weather_Helsinki_RAW_2004-01-01_2011-08-31.RData")

# Get unique locations
unique.locations <- unique(do.call(rbind, lapply(weather.raw, function(x) unique(as.data.frame(x)[c("name1", "coords.x1", "coords.x2")]))))
write.csv(unique.locations, file="FMI_Helsinki_locations.csv", row.names=FALSE, quote=FALSE)


# Function for extracting the wanted weather dat
ExtractWeatherData <- function(dat.raw) {
  # to data frame
  df <- as.data.frame(dat.raw)
  # Choose variables
  df <- df[c("name1", "measurement1", "variable")]
  # Rename variables
  names(df) <- c("name", "measurement", "variable")
  # Change measurement to numeric
  df$measurement <- as.numeric(as.vector(df$measurement))
  # Change NaN to NA (missing measurement)
  df$measurement[is.nan(df$measurement)] <- NA
  # -1.0 means zero
  df$measurement[df$measurement == -1.0] <- 0
  return(df)
}

# Process data for each date
weather.df.list <- lapply(weather.raw, ExtractWeatherData)
# Combine into one dataframe and transform to wide format
weather.df <- reshape2::melt(weather.df.list)
names(weather.df)[4] <- "date"
weather.df <- reshape2::dcast(weather.df, date + name ~ variable)
# Looks like this:
head(weather.df)
#         date                               name rrday snow  tday  tmax  tmin
# 1 2004-01-01 Helsinki Helsinki-Malmi lentoasema    NA   NA  -8.2  -1.8 -11.3
# 2 2004-01-01                Helsinki Kaisaniemi     0    0  -7.2  -1.1  -9.9
# 3 2004-01-02 Helsinki Helsinki-Malmi lentoasema    NA   NA -13.5 -10.5 -15.1
# 4 2004-01-02                Helsinki Kaisaniemi     0    0 -11.9  -9.3 -12.8
# 5 2004-01-03 Helsinki Helsinki-Malmi lentoasema    NA   NA -15.0 -10.0 -19.0
# 6 2004-01-03                Helsinki Kaisaniemi     0    0 -10.2  -4.2 -15.4

# Plot check
library("ggplot2")
ggplot(weather.df, aes(x=as.Date(date), y=tday)) + geom_point()

save(weather.df, file="FMI_weather_Helsinki_PROCESSED_2004-01-01_2011-08-31.RData")
write.csv(weather.df, file="FMI_weather_Helsinki_PROCESSED_2004-01-01_2011-08-31.csv", row.names=FALSE, quote=FALSE)


