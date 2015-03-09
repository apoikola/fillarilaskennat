# Scrape Finnish holidays from http://www.timeanddate.com/holidays/finland/

library("XML")
library("reshape2")
library("lubridate")

# Get Finnish holidays for year 2004-2011
url.base <- "http://www.timeanddate.com/holidays/finland/"
years <- 2004:2011
res <- list()
for (i in seq(years)) {
  res[[i]] <- readHTMLTable(paste(url.base, years[i], sep="/"))[[1]]
}
names(res) <- years

# Process holidays
holidays.df <- melt(res)
names(holidays.df) <- c("Weekday", "Date.raw", "Holiday.name", "Holiday.type", "Year")
temp <- strsplit(as.vector(holidays.df$Date.raw), split=" ")
stopifnot(all(sapply(temp, length)==2))
# Days
holidays.df$Day <- as.character(sapply(temp, "[", 2))
#holidays.df$Day[nchar(holidays.df$Day)==1] <- paste("0", )
# Months
holidays.df$Month <- sapply(temp, "[", 1)
months <- c("Jan"=01, "Feb"=02, "Mar"=03, "Apr"=04, "May"=05, "Jun"=06,
            "Jul"=07, "Aug"=08, "Sep"=09, "Oct"=10, "Nov"=11, "Dec"=12)
holidays.df$Month <- months[holidays.df$Month]
# Construct date
holidays.df$Date <- ymd(paste(holidays.df$Year, holidays.df$Month, holidays.df$Day, sep="-"))

# Save
save(holidays.df, file="Finnish_holidays.RData")
