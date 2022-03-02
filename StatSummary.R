
## Data analysis of SNAP data (2018-2020)
##
#### Repeated measures mixed effect model


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")

library(lme4)
library(lmerTest)
library(ggplot2)

## run R script RsquaredGLMM

## data set-up
{
x <- read.csv("SNAP_3year_harm.csv", head = T)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Phenol", "Suva254")

## add variable for phenolics normalized by DOC
x$PhenolDOC <- x$Phenol/x$DOC   

## reorder and select variables of interest
x <- subset(x, select = c("Date", "Site", "Treatment", "Depth", "Core", "Roots", "pH", "LOI", 
                          "DOC", "Phenol", "PhenolDOC", "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", 
                          "SM", "BD", "Cl", "SO4", "Na", "K", "Mg", "Ca", "TDN"))

### replace NAs with 0.005, which is half detection limit. 
x$Mg[is.na(x$Mg)] <- 0.005
x$Ca[is.na(x$Ca)] <- 0.005
x$Mg[x$Mg < 0] <- 0.005
x$Ca[x$Ca < 0] <- 0.005

### log10 transform ions 
lX<-log10(x[,c(18:23)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)

## add unique identifier
x$ID<-paste(x$Site,x$Treatment,x$Core,sep="")
x$Site <- as.factor(x$Site)

## Subset, shallow depth only
x5 <- x[which(x$Depth == "(0-5)"),]
x5$month <- c(rep(46,30), rep(32,30), rep(19,30), rep(21,30))
x5$ID2<-paste(x5$Site,x5$Treatment,sep="")
}



### model structure for repeated measures mixed effects model

## variable slopes and intercepts

## dependent var = RESPONSE
## fixed effect = Treatment, month and their interaction 
## random effect = Site


###### Full model

x5$RESPONSE <- x5$Cmin_c
yaxis <- "Cmin_c"

mod.full <- lmer(RESPONSE ~ Treatment*month + (1 + Treatment*month|Site) , data = x5)
mod.treat <- lmer(RESPONSE ~ Treatment + (1 + Treatment|Site) , data = x5) # Treatment(fixed) + Site(random) + Interaction
mod.time<- lmer(RESPONSE ~ month + (1 + month|Site) , data = x5)  # Trend over time, varies by site
null <- lmer(RESPONSE ~ (1|Site), data = x5)

AIC(mod.full, mod.treat, mod.time, null)


## other models not included in analysis
#mod.null1 <- lm(RESPONSE ~ Treatment, data = x5)  # Treatment only
#mod1 <- lmer(RESPONSE ~ Treatment + (1|Site) , data = x5)  # Treatment(fixed) + Site(random)
#mod.null2 <- lm(RESPONSE ~ month, data = x5)  # Trend over time
#mod2 <- lmer(RESPONSE ~ month + (1|Site) , data = x5)   # Trend over time, different starting point by site



summary(mod.full)
r.squaredGLMM(mod.full)
summary(mod.treat)
r.squaredGLMM(mod.treat)
summary(mod.time)
r.squaredGLMM(mod.time)
summary(null)
r.squaredGLMM(null)

## model selected based on lowest AIC
## Summary
mod <- mod.full
mod <- mod.treat
mod <- mod.time
mod <- null
anova(mod)



## PLOT
fit <- fixef(mod)[2]
se.fit <- sqrt(diag(vcov(mod)))[2]
plot <- ggplot(x5, aes(x = month, y = RESPONSE, colour = Site)) +
  facet_wrap(~Treatment, nrow=1) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_bw() +
  geom_line(data = cbind(x5, pred = predict(mod)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  #geom_ribbon(data = cbind(x5, pred=predict(mod)), aes(ymin=pred-1.96*se.fit, ymax = pred+1.96*se.fit),
  #            alpha = 0.2, fill = "gray70") +
  theme(panel.spacing = unit(2, "lines"),    # adding space between panels
        legend.direction = "horizontal",
        legend.position = c(0.5,-0.15),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.9, l = 0.2, unit = "cm")) +
  scale_x_continuous(breaks = c(20, 32, 45), labels = c("2018", "2019", "2020")) +
  scale_color_manual(labels = c("Dry", "Int.", "Wet"), values = c("#f23f1b", "#8707A6", "#0D0887")) +
  ylab(yaxis) + xlab(" ")
plot







## no model plot
plot <- ggplot(x5, aes(x = month, y = RESPONSE, colour = Site)) +
  facet_wrap(~Treatment, nrow=1) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_bw() +
  #geom_line(data = cbind(x5, pred = predict(mod)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  #geom_ribbon(data = cbind(x5, pred=predict(mod)), aes(ymin=pred-1.96*se.fit, ymax = pred+1.96*se.fit),
  #            alpha = 0.2, fill = "gray70") +
  theme(panel.spacing = unit(2, "lines"),    # adding space between panels
        legend.direction = "horizontal",
        legend.position = c(0.5,-0.15),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.9, l = 0.2, unit = "cm")) +
  scale_x_continuous(breaks = c(20, 32, 45), labels = c("2018", "2019", "2020")) +
  scale_color_manual(labels = c("Dry", "Int.", "Wet"), values = c("#f23f1b", "#8707A6", "#0D0887")) +
  ylab(yaxis) + xlab(" ")
plot





