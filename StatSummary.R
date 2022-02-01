



setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")


#library(nlme)
library(lme4)

x <- read.csv("SNAP_3year_harm.csv", head = T)


names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Phenol", "Suva254")

x$PhenolDOC <- x$Phenol/x$DOC

## reorder and select variables
x <- subset(x, select = c("Date", "Site", "Treatment", "Depth", "Core", "Roots", "pH", "LOI", 
                          "DOC", "Phenol", "PhenolDOC", "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", 
                          "SM", "BD", "Cl", "SO4", "Na", "K", "Mg", "Ca", "TDN"))

### replace NAs with 0.005, which is half detection limit. 
x$Mg[is.na(x$Mg)] <- 0.005
x$Ca[is.na(x$Ca)] <- 0.005
x$Mg[x$Mg < 0] <- 0.005
x$Ca[x$Ca < 0] <- 0.005

### log transform ions 
lX<-log(x[,c(18:23)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)

x$ID<-paste(x$Site,x$Treatment,x$Core,sep="")
x$Site <- as.factor(x$Site)




## Subset, shallow depth only
x5 <- x[which(x$Depth == "(0-5)"),]
x5$month <- c(rep(46,30), rep(32,30), rep(19,30), rep(21,30))
x5$ID2<-paste(x5$Site,x5$Treatment,sep="")


## dependent var = pH
## fixed effect = date, treatment, 
## random effect = site


###### pH

mod4 <- lmer(pH ~ Treatment*month + (1+ Treatment*month|Site) , data = x5)
summary(mod4)
## now site explains 65% of the residual variance

## treatment is still not a significant fixed effect 
plot(mod4)
qqnorm(resid(mod4))
qqline(resid(mod4))  ## better

fit <- fixef(mod4)[2]
se.fit <- sqrt(diag(vcov(mod4)))[2]
plot <- ggplot(x5, aes(x = month, y = pH, colour = Treatment)) +
  facet_wrap(~Site, nrow=1) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_bw() +
  geom_line(data = cbind(x5, pred = predict(mod4)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  geom_ribbon(data = cbind(x5, pred=predict(mod4)), aes(ymin=pred-1.96*se.fit, ymax = pred+1.96*se.fit),
              alpha = 0.2, fill = "gray70") +
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
plot







##### Sodium
mod4 <- lmer(logNa ~ Treatment*month + (1+ Treatment*month|Site) , data = x5)
summary(mod4)
## now site explains 65% of the residual variance

## treatment is still not a significant fixed effect 
plot(mod4)
qqnorm(resid(mod4))
qqline(resid(mod4))  ## better

fit <- fixef(mod4)[2]
se.fit <- sqrt(diag(vcov(mod4)))[2]
plot <- ggplot(x5, aes(x = month, y = logNa, colour = Treatment)) +
  facet_wrap(~Site, nrow=1) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_bw() +
  geom_line(data = cbind(x5, pred = predict(mod4)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  geom_ribbon(data = cbind(x5, pred=predict(mod4)), aes(ymin=pred-1.96*se.fit, ymax = pred+1.96*se.fit),
              alpha = 0.2, fill = "gray70") +
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
plot


library(stargazer)
stargazer(mod4, type = "text",
          digits = 3, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")








