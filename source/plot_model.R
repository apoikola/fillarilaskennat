# Script for plotting the output of the bike count model
library("dplyr")
library("ggplot2")
theme_set(theme_bw(base_size = 16))
library("gridExtra")
library("reshape2")
library("scales")
library("mgcv")


## PLOT M7 WITH GGPLOT2 ########

# Load prepared data, done in 'source/prepare_data.R'
d <- readRDS("model_data/data_prepared.rds")
# Load model object created in 
m7 <- readRDS("model_data/m7.rds")


# For plotting GAM smooths with ggplot2
# from http://stackoverflow.com/questions/19735149/is-it-possible-to-plot-the-smooth-components-of-a-gam-fit-with-ggplot2
source("source/functions.R")
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
# percent.vals2 <- c(25, 50, 100, 200)
# y.vals2 <- log(percent.vals2/100)
percent.vals3 <- c(50, 67, 80, 100, 125, 150, 200)
y.vals3 <- log(percent.vals3/100)

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
resid.df <- data.frame(d, residuals=model$residuals)
resid.df$date <- as.Date(resid.df$date)
p.resid <- ggplot(resid.df, aes(x=date, y=residuals)) + 
  geom_line(alpha=0.8) + 
  facet_grid(main.site.named ~ .) +
  ggtitle("Residuals left") + 
  ylab("Effect on log_e scale") +
  theme(strip.text.y=element_text(angle=0))

p.data <- arrangeGrob(p.rp, p.resid, ncol=1)
# ggsave(plot=p.data, file="figures/Fillari_M7_data_v3.png", width=8, height=15)

# List biggest residuals
# write.csv(head(resid.df[order(abs(resid.df$residuals), decreasing = T), ], 100), file="processed/M7_residuals_v3.csv")

## Plot continuous smooths
# Remove categorical variables from smooth plots
model2 <- model
model2$smooth <- model2$smooth[sapply(model2$smooth, length)==23]
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

# ## Plot main site effects - NOT HERE ANYMORE
# site.inds <- model$smooth[[4]]$first.para:model$smooth[[4]]$last.para
# site.coefs <- model$coefficients[site.inds]
# site.ses <- summary(model)$se[site.inds]
# site.df <- data.frame(Site=main.site.names, Effect=site.coefs, SE=site.ses)
# site.df$Site <- factor(site.df$Site, levels=site.df$Site[order(site.df$Effect)])
# p.site <- ggplot(site.df, aes(x=Site, y=Effect, ymin=Effect-SE, ymax=Effect+SE)) + 
#   geom_point(size=3) + 
#   geom_errorbar(width=0) + 
#   ggtitle("Site effects") + 
#   labs(x=NULL, y="Effect (%)") +
#   scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +
#   geom_hline(y=0, linetype="dashed") +
#   coord_flip()
# # Flat => no need to show

## Plot main.site:earth.phase effects - 
site.ep.inds <- model$smooth[[4]]$first.para:model$smooth[[4]]$last.para
site.ep.coefs <- model$coefficients[site.ep.inds]
site.ep.ses <- summary(model)$se[site.ep.inds]
site.ep.df <- data.frame(Site=main.site.names, Effect=site.ep.coefs, SE=site.ep.ses)
site.ep.df$Site <- factor(site.ep.df$Site, levels=site.ep.df$Site[order(site.ep.df$Effect)])
p.site.ep <- ggplot(site.ep.df, aes(x=Site, y=Effect, ymin=Effect-SE, ymax=Effect+SE)) + 
  geom_point(size=3) + 
  geom_errorbar(width=0) + 
  ggtitle("Site-earth phase interaction effects") + 
  labs(x=NULL, y="Effect (%)") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +
  geom_hline(y=0, linetype="dashed") +
  coord_flip()

# ## Plot main.site:weekday effects - NOT NOW
# site.inds <- model$smooth[[5]]$first.para:model$smooth[[5]]$last.para
# site.coefs <- model$coefficients[site.inds]
# site.ses <- summary(model)$se[site.inds]
# site.df <- data.frame(Site=main.site.names, Effect=site.coefs, SE=site.ses)
# site.df$Site <- factor(site.df$Site, levels=site.df$Site[order(site.df$Effect)])
# p.site <- ggplot(site.df, aes(x=Site, y=Effect, ymin=Effect-SE, ymax=Effect+SE)) + 
#   geom_point(size=3) + 
#   geom_errorbar(width=0) + 
#   ggtitle("Site effects") + 
#   labs(x=NULL, y="Effect (%)") +
#   scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +
#   geom_hline(y=0, linetype="dashed") +
#   coord_flip()


## Plot scalar coefficients and standard errors
smooth.param.inds <- unlist(lapply(model$smooth, function(x) x$first.para:x$last.para))
coefs <- model$coefficients[-smooth.param.inds]
ses <- summary(model)$se[-smooth.param.inds]
names(coefs) <- gsub("I\\(rrday != 0\\)TRUE", "AnyRain", names(coefs))
names(coefs) <- gsub("I\\(snow != 0\\)TRUE", "AnySnow", names(coefs))

## Extract and plot weekday stuff
coefs.weekdays <- coefs[grep("weekday", names(coefs))]
ses.weekdays <- ses[grep("weekday", names(coefs))]
names(coefs.weekdays) <- gsub("weekday", "", names(coefs.weekdays))
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
temp <- strsplit(names(coefs.weekdays), split=":")
stopifnot(all(sapply(temp, length) <=2))
temp[sapply(temp, length)==1] <- lapply(temp[sapply(temp, length)==1], c, "baseline")
# Reorder
temp[!(sapply(temp, "[", 1) %in% weekdays)] <- lapply(temp[!(sapply(temp, "[", 1) %in% weekdays)], function(x) x[2:1])
cw.df <- data.frame(Weekday=sapply(temp, "[", 1), Factor=sapply(temp, "[", 2), Coefficient=coefs.weekdays, row.names=NULL, SE=ses.weekdays)
# Add Friday as zeros
cw.df <- rbind(cw.df, data.frame(Weekday="Friday", Factor=levels(cw.df$Factor), Coefficient=0, SE=0))
cw.df$Weekday <- factor(cw.df$Weekday, levels=rev(weekdays))
p.cw <- ggplot(cw.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE, colour=Weekday)) + 
  geom_point(size=3, position=position_dodge(width=0.4)) + 
  geom_errorbar(width=0, position=position_dodge(width=0.4)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +  
  labs(x=NULL, y="Effect (%)") +
  ggtitle("Weekday interactions\n(Friday is the baseline)") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE))

## Extract and plot remaining main.site interaction effects
coefs.sites <- coefs[grep("main.site", names(coefs))]
ses.sites <- ses[grep("main.site", names(ses))]
temp <- strsplit(names(coefs.sites), split=":")
stopifnot(all(sapply(temp, length) <=2))
temp[sapply(temp, length)==1] <- lapply(temp[sapply(temp, length)==1], function(x) c("baseline", x))
# Reorder
wrong.order.inds <- grep("main.site", sapply(temp, "[", 1))
temp[-wrong.order.inds] <- lapply(temp[-wrong.order.inds], function(x) x[2:1])

cms.df <- data.frame(main.site=sapply(temp, "[", 1), Factor=sapply(temp, "[", 2), Coefficient=coefs.sites, row.names=NULL, SE=ses.sites)
cms.df$main.site.named <- main.site.names[substr(cms.df$main.site, 10, 14)]
# Add Kuusisaarensilta as zeros
cms.df <- rbind(cms.df, data.frame(main.site="x", Factor=levels(cms.df$Factor), Coefficient=0, SE=0, main.site.named="Kuusisaarensilta"))
# Add site.main:earth.phase from smooth
cms.df <- rbind(cms.df, data.frame(main.site="x", Factor="earth.phase", Coefficient=site.ep.df$Effect, SE=site.ep.df$SE, main.site.named=site.ep.df$Site))


# Remove Veräjämäki
cms.df <- droplevels(subset(cms.df, main.site.named != "Veräjämäki"))
# Reorder
temp <- subset(cms.df, Factor=="baseline")
cms.df$main.site.named <- factor(cms.df$main.site.named, levels=temp$main.site.named[order(temp$Coefficient)])
p.cms <- ggplot(cms.df, aes(x=Factor, y=Coefficient, ymin=Coefficient-SE, ymax=Coefficient+SE, colour=main.site.named)) + 
  geom_point(size=3, position=position_dodge(width=0.4)) + 
  geom_errorbar(width=0, position=position_dodge(width=0.4)) + 
  geom_hline(y=0, linetype="dashed") +
  scale_y_continuous(breaks=y.vals3, labels=percent.vals3) +  
  labs(x=NULL, y="Effect (%)") +
  ggtitle("Main site interactions\n(Kuusisaarensilta is the baseline)") +
  coord_flip() + 
  guides(colour = guide_legend(reverse=TRUE))


# Extract the rest of the coefficients
coefs.other <- coefs[-c(grep("weekday", names(coefs)), grep("main.site", names(coefs)))]
ses.other <- ses[-c(grep("weekday", names(coefs)), grep("main.site", names(coefs)))]

# Extract and plot remaining main effects
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

# Extract and plot remaining interaction effects
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


# Put together
# p.f1 <- arrangeGrob(p.site, p.smooth, nrow=1, widths=c(1.5, 3))
# p.f2 <- arrangeGrob(arrangeGrob(p.cm, p.ci, ncol=1), p.cw, nrow=1)
# p.fillari <- arrangeGrob(p.f1, p.f2, ncol=1, heights=c(1.2, 2))
p.f1 <- arrangeGrob(p.cm, p.ci, nrow=1)
p.f2 <- arrangeGrob(p.cms, p.cw, nrow=1)
p.fillari <- arrangeGrob(p.smooth, p.f1, p.f2, ncol=1, heights=c(1, 1, 2))
# ggsave(plot=p.fillari, file="figures/Fillari_M7_model_v3.png", width=12, height=15)

