# Code for analysing and visualising traffic data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen <juuso.parkkinen@gmail.com> and Antti Poikola <antti poikola@gmail.com>. All rights reserved.

library(ggplot2)
library(reshape)


search.pattern <-"kuu" # pattern used to search for folder names which will be included in the analysis
data.folder <- "/Users/apoikola/Documents/github/fillarilaskennat/data/"
output.folder <- "/Users/apoikola/Documents/github/fillarilaskennat/output/"

# data.folder <- "./data/"
# output.folder <- "./output/"

# TODO
# Add place names and coordinates (if found)


# Visualize cycling counters on a map

# coordinate.file <- paste(data.folder, "Laskentapisteet 2011 - Laskentapisteet 2011 FIX.csv", sep = "")
# laskentapisteet <- read.csv(coordinate.file)
# laskentapisteet$KX <- as.numeric(gsub(",", ".", laskentapisteet$KX))
# laskentapisteet$KY <- as.numeric(gsub(",", ".", laskentapisteet$KY))
# hel.plot <- ggplot(hel.coastline.df, aes(x=long, y=lat)) + geom_polygon(aes(group=group), fill="grey80")
# hel.plot <- hel.plot + xlim(bbox.Helsinki.center["x","min"], bbox.Helsinki.center["x","max"]) + ylim(bbox.Helsinki.center["y","min"], bbox.Helsinki.center["y","max"])
# hel.plot3 <- hel.plot2 + geom_text(data=laskentapisteet, aes(x=KX, y=KY, label=X), size=2, hjust=1, vjust=1, angle=-45)
# hel.plot3 <- hel.plot3 + geom_point(data=laskentapisteet, aes(x=KX, y=KY, label=X), colour="red")
# ggsave("HSOpen4/Liikkennelaskentapisteet2_20120309.pdf", plot=hel.plot3, width=8, height=8)





#Clean up the raw data files
loc.names <- read.table(paste(data.folder, "pyoralaskennat_readme.txt", sep=""), skip=17, sep="=", fileEncoding="ISO-8859-1")
month.folders <- dir(data.folder, pattern=search.pattern)
final.df <- c()

for (mi in 1:length(month.folders)) {
  month.files <- dir(paste(data.folder, month.folders[mi],sep=""), pattern="\\.2") # pattern needed to skip corrupted files which filename ends with .104
    
  for (fi in 1:length(month.files)) {
    
    if (length(month.files)==0){
      print(paste("no useful files in folder",month.folders[mi]))
      break
    }
    
    # Extract site number and name
    filename <- paste(data.folder, month.folders[mi], "/", month.files[fi],sep="")
    site.number <- as.numeric(unlist(strsplit(unlist(strsplit(filename, split="\\."))[1], split="/"))[9])
    site.init <- as.numeric(substr(as.character(site.number), 1, 3))
    if (site.init==117)
      site.init <- as.numeric(substr(as.character(site.number), 1, 4))
    site.name <- as.vector(loc.names$V2[match(site.init, loc.names$V1)])
    dat.raw <- scan(filename, what=character(), sep="\n", strip.white=TRUE)
    
    
    # Separate locations
    loc.rows <- grep("LOCATION", dat.raw)
    loc.list <- list()

  # 2007_06 12001803.207 has only one week of data
    if (length(loc.rows)==1) {
      loc.list[[1]] <- dat.raw[loc.rows[1]:length(dat.raw)]
    }
    else {
      for (li in 1:(length(loc.rows)-1)){
        loc.list[[li]] <- dat.raw[loc.rows[li]:(loc.rows[li+1]-1)]
        loc.list[[li+1]] <- dat.raw[loc.rows[li+1]:length(dat.raw)]
      }
    }

    # If clause needed to skip corrupted files recognized by the second location row being longer than 79 characters i.e. 2005_09 11803512.205
    if (nchar(dat.raw[loc.rows[1]])<79){    
      
      # Extract data from each location
      loc.mat <- c()
      for (li in 1:length(loc.list)) {
        date.row <- grep("DATE", loc.list[[li]])
        # Check whether tabs or spaces were used as separator
        if (length(grep("\t", loc.list[[li]][date.row]))>0) {
          # Fix first date row
          loc.list[[li]][date.row] <- gsub("\t/", "/", loc.list[[li]][date.row])
          # Change then all "\t" to " "
          loc.list[[li]][date.row:(date.row+25)] <- gsub("\t", " ", loc.list[[li]][date.row:(date.row+25)])
        }
        date.temp <- unlist(strsplit(loc.list[[li]][date.row], split=" "))
        date.temp <- date.temp[-which(date.temp=="")]
        dates <- gsub("00", "20", as.character(as.Date(date.temp[2])+0:6))
        dat.mat <- matrix(NA, nrow=7, ncol=24, dimnames=list(dates, 0:23))
        for (hi in 1:24) {
          hrow <- date.row + hi + 1
          h.temp <- unlist(strsplit(loc.list[[li]][hrow], split=" "))
          if (!is.na(h.temp)[1]) { # Needed as e.g. 1171001.211 in January is not complete
            if (any(h.temp=="")) # Needed for '\t' -files 
              h.temp <- h.temp[-which(h.temp=="")]
          }
          dat.mat[,hi] <- suppressWarnings(as.numeric(h.temp[2:8]))
        }
        loc.mat <- rbind(loc.mat, dat.mat)
      }
      loc.df <- data.frame(LocationID1=site.number, LocationID2=site.init, melt(loc.mat))
      names(loc.df)[3:5] <- c("Date", "Hour", "Value")
      final.df <- rbind(final.df, loc.df)
    }
  }

# Inside for loop this writes monthly CSV files   
#   if (!is.null(final.df)){
#     # Reorder based on 1) Location, 2) Date, 3) Hour
#     final.df <- final.df[order(final.df$LocationID1, final.df$Date, final.df$Hour),]
#     # Add weekday
#     final.df$WeekDay <- factor(weekdays(as.Date(final.df$Date)))
#     final.df <- final.df[c(1:3, 6, 4:5)]
#   
#     #save(final.df, file=paste(output.folder, paste(month.folders[mi],".RData", sep = "")))
#     write.csv(final.df, file=paste(output.folder, paste(month.folders[mi],".csv", sep = "")))
#     
#     final.df <- c()  
#   }
}

if (!is.null(final.df)){
  # Reorder based on 1) Location, 2) Date, 3) Hour
  final.df <- final.df[order(final.df$LocationID1, final.df$Date, final.df$Hour),]
  # Add weekday
  final.df$WeekDay <- factor(weekdays(as.Date(final.df$Date)))
  final.df <- final.df[c(1:3, 6, 4:5)]
  
  #save(final.df, file=paste(output.folder, paste(month.folders[mi],".RData", sep = "")))
  write.csv(final.df, file=paste(output.folder, paste(search.pattern,"_20140927.csv", sep = "")))
  
  final.df <- c()  
}
