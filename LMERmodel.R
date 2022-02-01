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
## fixed effect = date, treatment, depth, Site
## random effect = (depth and site don't have enought levels to be considered a random effect)

mod1 <- lmer(pH ~ Treatment + (1|Site), data = x5)
summary(mod1)

## site explains about 66% of the residual variance 
.6349/(0.6349+0.331)
## Treatment is not a significant fixed effect because the estimate is smaller than the error
plot(mod1)
qqnorm(resid(mod1))
qqline(resid(mod1))  #bad



## hierarchical mixed effects model
## RANDOM INTERCEPT

mod2 <- lmer(pH ~ Treatment*month + (1|Site), data = x5)
summary(mod2)
## now site explains 87% of the residual variance
0.64/(0.64+0.095)
## treatment is still not a significant fixed effect, but month is. 
plot(mod2)
qqnorm(resid(mod2))
qqline(resid(mod2))  ## better

confint(mod2)



fit <- fixef(mod2)[2]
se.fit <- sqrt(diag(vcov(mod2)))[2]
plot <- ggplot(x5, aes(x = month, y = pH, colour = Treatment)) +
  facet_wrap(~Site, nrow=1) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_bw() +
  geom_line(data = cbind(x5, pred = predict(mod2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  geom_ribbon(data = cbind(x5, pred=predict(mod2)), aes(ymin=pred-1.96*se.fit, ymax = pred+1.96*se.fit),
              alpha = 0.2, fill = "gray70") +
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
plot



## hierarchical mixed effects model
## RANDOM SLOPE + RANDOM INTERCEPT

mod3 <- lmer(pH ~ Treatment*month + (1+ Treatment|Site), data = x5)
summary(mod3)
## now site explains 79% of the residual variance
0.63/(0.63+0.094 + 0.078)
## treatment is still not a significant fixed effect, but month is. 
plot(mod3)
qqnorm(resid(mod3))
qqline(resid(mod3))  ## better

fit <- fixef(mod3)[2]
se.fit <- sqrt(diag(vcov(mod3)))[2]
plot <- ggplot(x5, aes(x = month, y = pH, colour = Treatment)) +
  facet_wrap(~Site, nrow=1) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_bw() +
  geom_line(data = cbind(x5, pred = predict(mod3)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  geom_ribbon(data = cbind(x5, pred=predict(mod3)), aes(ymin=pred-1.96*se.fit, ymax = pred+1.96*se.fit),
              alpha = 0.2, fill = "gray70") +
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
plot




## hierarchical mixed effects model
## RANDOM SLOPE + RANDOM INTERCEPT
 
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


anova(mod3, mod4)







