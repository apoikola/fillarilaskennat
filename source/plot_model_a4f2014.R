# Script for plotting the bike count model
# Done for the fillarilaskennat article in Apps4Finland 2014

## PLOTS FOR A4F IN FINNISH ########

message("Need to run 'plot_model.R' first!")
source("source/plot_model.R")

# ADD DATA + MODEL FIGURE IN FINNISH!
# Temperature
tday.df <- droplevels(subset(smooths, x.var == "tday\n(mean temperature for day)"))
p.temperature <- ggplot(tday.df, aes(x=x.val, y=value)) + geom_line() + 
  geom_line(aes(y=value + 2*se), linetype="dashed") + 
  geom_line(aes(y=value - 2*se), linetype="dashed") +
  scale_y_continuous(breaks=y.vals, labels=percent.vals, limits=range(smooths$value)) +
  geom_hline(y=0, linetype="dashed") + 
  labs(x="Lämpötila", y="Vaikutus (%)")
ggsave(plot=p.temperature, file="a4f_2014/a4f_lampotila_v1.png", width=4, height=4)

p.temperature2 <- ggplot(tday.df, aes(x=x.val, y=value, colour=x.val)) + 
  geom_line(size=1.5) + 
  geom_line(aes(y=value + 2*se)) + #, linetype="dashed") + 
  geom_line(aes(y=value - 2*se)) +#, linetype="dashed") +
  scale_colour_gradient2(low=muted("blue"), mid="gray", high=muted("red")) + 
  scale_y_continuous(breaks=y.vals, labels=percent.vals-100, limits=range(smooths$value)) +
  geom_hline(y=0, linetype="dashed") + 
  labs(x="Lämpötila", y="Vaikutus (%) ± keskivirhe") +
  theme(legend.position="none") + 
  ggtitle("Lämpötilan vaikutus")
ggsave(plot=p.temperature2, file="a4f_2014/a4f_lampotila_vari_v2.png", width=4, height=4)

# Main effects
# Split into continuous and discrete factors
percent.vals4 <- c(67, 80, 90, 100, 110, 125)
y.vals4 <- log(percent.vals4/100)

# Discrete vars
factors.discrete <- c("julyTRUE", "AnySnow", "holidayTRUE", "near.julyTRUE", "AnyRain")
main.discrete.df <- droplevels(subset(cm.df, Factor %in% factors.discrete))
levels(main.discrete.df$Factor) <- c("heinäkuu", "lunta maassa", "juhlapyhä", 
                                     "kesäkuun loppu ja\nelokuun alku", "sataa (kyllä/ei)")
main.discrete.df$Group <- c("Aika", "Aika", "Aika", "Sää", "Sää")
levels(main.discrete.df$Factor) <- gsub(" \\(kyllä/ei\\)", "", levels(main.discrete.df$Factor))
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
  theme(strip.text.y=element_text(angle=0))
ggsave(plot=p.main.discrete, file="a4f_2014/a4f_suorat_v2.png", width=5, height=5)

# # Continuous vars (not done properly as not shown currently!)
# main.df <- droplevels(subset(main.df, Factor!="dtemp"))
# factors.10 <- c("year", "rrday", "snow", "rrday1", "I(pmax(0, dsnow))")
# main.df[main.df$Factor %in% factors.10, "Coefficient"] <- 10*main.df[main.df$Factor %in% factors.10, "Coefficient"]
# main.df[main.df$Factor %in% factors.10, "SE"] <- 10*main.df[main.df$Factor %in% factors.10, "SE"]
# levels(main.df$Factor)[levels(main.df$Factor) %in% factors.10] <- paste0(levels(main.df$Factor)[levels(main.df$Factor) %in% factors.10], "_10")
# levels(main.df$Factor) <- c("heinäkuu (kyllä/ei)", "lunta maassa (kyllä/ei)", 
#                             "juhlapyhä (kyllä/ei)", "kesäkuun loppu ja\nelokuun alku (kyllä/ei)",
#                             "sataa (kyllä/ei)", "lumen määrä (10cm)", "uuden lumen määrä (10cm)", 
#                             "sateen määrä (10mm)",
#                             "eilinen sateen määrä (10mm)", "aika (10 vuotta)")
# main.df$Factor <- factor(main.df$Factor, levels=main.df$Factor[order(main.df$Coefficient)])
# p.main <- ggplot(main.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE)) + 
#   geom_point(size=3) + 
#   geom_errorbar(width=0) + 
#   ggtitle("Suorat vaikutukset") + 
#   labs(x=NULL, y="Vaikutus (%)") +
#   scale_y_continuous(breaks=y.vals4, labels=percent.vals4) +
#   geom_hline(y=0, linetype="dashed") +
#   coord_flip()
# ggsave(plot=p.main, file="a4f_2014/a4f_suorat_v1.png", width=6, height=6)




# Main site
# show baseline, holidayTRUE, year
site.df <- droplevels(subset(cms.df, Factor %in% c("baseline", "year", "holidayTRUE")))
site.df[site.df$Factor=="year", "Coefficient"] <- 10*site.df[site.df$Factor=="year", "Coefficient"]
site.df[site.df$Factor=="year", "SE"] <- 10*site.df[site.df$Factor=="year", "SE"]
levels(site.df$Factor)[3] <- "years_10"
levels(site.df$Factor) <- c("lähtötaso", "muutos\njuhlapyhänä", "muutos 10\nvuoden aikana")
site.df$Factor <- factor(site.df$Factor, levels=c("muutos 10\nvuoden aikana", "muutos\njuhlapyhänä", "lähtötaso"))

p.mainsite <- ggplot(site.df, aes(x=Factor, y=Coefficient, colour=main.site.named)) + 
  geom_point(size=3, position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymin=Coefficient-SE, ymax=Coefficient+SE), width=0, position=position_dodge(width=0.7)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3-100) +  
  labs(x=NULL, y="Vaikutus (%)", colour="sijainti") +
  ggtitle("Sijainnin vaikutus") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE))
ggsave(plot=p.mainsite, file="a4f_2014/a4f_sijainti_v2.png", width=8, height=6)

p.mainsite2 <- ggplot(site.df, aes(x=main.site.named, y=Coefficient, colour=Factor)) + 
  geom_point(size=3, position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymin=Coefficient-SE, ymax=Coefficient+SE), width=0, position=position_dodge(width=0.7)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3-100) +  
  labs(x=NULL, y="Vaikutus (%)", colour="sijainti") +
  ggtitle("Sijainnin vaikutus") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE))
ggsave(plot=p.mainsite2, file="a4f_2014/a4f_sijainti_v2_flipped.png", width=8, height=6)

# Weekday
# show baseline, AnyRain, AnySnow, 
weekday.df <- droplevels(subset(cw.df, Factor %in% c("baseline", "AnyRain", "AnySnow")))
levels(weekday.df$Weekday) <- c("sunnuntai", "lauantai", "perjantai", "torstai", 
                                "keskiviikko", "tiistai", "maanantai")
levels(weekday.df$Factor) <- c("muutos\nsateella", "muutos kun\nlunta maassa", "lähtötaso")
p.weekday <- ggplot(weekday.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE, colour=Weekday)) + 
  geom_point(size=3, position=position_dodge(width=0.7)) + 
  geom_errorbar(width=0, position=position_dodge(width=0.7)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals4, labels=percent.vals4-100) +  
  labs(x=NULL, y="Vaikutus (%)", colour="viikonpäivä") +
  ggtitle("Viikonpäivien vaikutus") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE))
ggsave(plot=p.weekday, file="a4f_2014/a4f_viikonpaivat_v2.png", width=8, height=6)
p.weekday2 <- ggplot(weekday.df, aes(x=Weekday, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE, colour=Factor)) + 
  geom_point(size=3, position=position_dodge(width=0.7)) + 
  geom_errorbar(width=0, position=position_dodge(width=0.7)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals4, labels=percent.vals4-100) +  
  labs(x=NULL, y="Vaikutus (%)", colour="viikonpäivä") +
  ggtitle("Viikonpäivien vaikutus") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE))
ggsave(plot=p.weekday2, file="a4f_2014/a4f_viikonpaivat_v2_flipped.png", width=8, height=6)


# Plot example data + model
d2.subset <- droplevels(subset(d2, main.site.named=="1070\nEläintarhanlahti" & year %in% 2009:2010))
levels(d2.subset$variable) <- c("raakadata", "mallin ennuste")
p.data <- ggplot(d2.subset, aes(x=date, y=Count, colour=variable)) + 
  geom_line(alpha=0.8) + 
  scale_y_log10() + 
  ggtitle("Esimerkki datasta: Eläintarhanlahti") +
  labs(x="päivämäärä", y="pyöräilijöiden määrä", colour="data") +
  scale_x_date(labels=date_format("%Y/%m"))
ggsave(plot=p.data, file="a4f_2014/a4f_data_v2.png", width=10, height=3)


## OLD!!! PLOTS FOR A4F DATA JOURNALISM PIECE #######

# # Temperature
# tday.df <- droplevels(subset(smooths, x.var == "tday\n(mean temperature for day)"))
# p.temperature <- ggplot(tday.df, aes(x=x.val, y=value)) + geom_line() + 
#   geom_line(aes(y=value + 2*se), linetype="dashed") + 
#   geom_line(aes(y=value - 2*se), linetype="dashed") +
#   scale_y_continuous(breaks=y.vals, labels=percent.vals, limits=range(smooths$value)) +
#   geom_hline(y=0, linetype="dashed") + 
#   labs(x="Temperature", y="Effect (%)")
# 
# # Main effects
# main.df <- cm.df
# factors.10 <- c("year", "rrday", "snow", "rrday1", "I(pmax(0, dsnow))")
# main.df[main.df$Factor %in% factors.10, "Coefficient"] <- 10*main.df[main.df$Factor %in% factors.10, "Coefficient"]
# main.df[main.df$Factor %in% factors.10, "SE"] <- 10*main.df[main.df$Factor %in% factors.10, "SE"]
# levels(main.df$Factor)[levels(main.df$Factor) %in% factors.10] <- paste0(levels(main.df$Factor)[levels(main.df$Factor) %in% factors.10], "_10")
# p.main <- ggplot(main.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE)) + 
#   geom_point(size=3) + 
#   geom_errorbar(width=0) + 
#   ggtitle("Main effects") + 
#   labs(x=NULL, y="Effect (%)") +
#   scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +
#   geom_hline(y=0, linetype="dashed") +
#   coord_flip()
# 
# 
# # Main site
# # show baseline, holidayTRUE, year
# site.df <- droplevels(subset(cms.df, Factor %in% c("baseline", "year", "holidayTRUE")))
# site.df[site.df$Factor=="year", "Coefficient"] <- 10*site.df[site.df$Factor=="year", "Coefficient"]
# site.df[site.df$Factor=="year", "SE"] <- 10*site.df[site.df$Factor=="year", "SE"]
# levels(site.df$Factor)[3] <- "years_10"
# p.mainsite <- ggplot(site.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE, colour=main.site.named)) + 
#   geom_point(size=3, position=position_dodge(width=0.4)) + 
#   geom_errorbar(width=0, position=position_dodge(width=0.4)) + 
#   geom_hline(y=0, linetype="dashed") +
#   scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +  
#   labs(x=NULL, y="Effect (%)") +
#   ggtitle("Main site interactions") +
#   coord_flip() + 
#   guides(colour = guide_legend(reverse=TRUE))
# 
# # Weekday
# # show baseline, AnyRain, AnySnow, 
# weekday.df <- droplevels(subset(cw.df, Factor %in% c("baseline", "AnyRain", "AnySnow")))
# p.weekday <- ggplot(weekday.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE, colour=Weekday)) + 
#   geom_point(size=3, position=position_dodge(width=0.4)) + 
#   geom_errorbar(width=0, position=position_dodge(width=0.4)) + 
#   geom_hline(y=0, linetype="dashed") +
#   scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +  
#   labs(x=NULL, y="Effect (%)") +
#   ggtitle("Weekday interactions") +
#   coord_flip() + 
#   guides(colour = guide_legend(reverse=TRUE))
# 
# p.a4f1 <- arrangeGrob(p.temperature, p.main, p.mainsite, p.weekday, ncol=1)
# ggsave(plot=p.a4f1, file="figures/Fillari_M7_model_A4F_v1.png", width=8, height=16)

