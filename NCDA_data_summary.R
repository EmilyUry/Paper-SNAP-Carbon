

## NCDA Soil Analyses Data Summary

setwd("C:/Users/uryem/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")

library(dplyr)


dat <- read.csv("NCDA_soils_data.csv", head = T)

names(dat) <- c("Site", "Plot", "Depth", "ID", "HM", "W_V_g_cm3", "pH", "BaseSat", "AC", "CEC", "Na", "P", "K",
                "Ca", "Mg", "S", "Mn", "Cu", "Zn", "EC")
subset <- subset(dat, select = c(Site, Plot, Depth, ID, HM, pH, BaseSat, AC, CEC, EC))
dat$Site <- as.factor(dat$Site)
dat$Plot <- as.factor(dat$Plot)
dat$Depth <- as.factor(dat$Depth)


pH <- subset %>%
  group_by(Site, Plot, Depth) %>%
  summarise(mean =round(mean(pH), 1), 
            se = round(sd(pH)/sqrt(5), 3))
pH

pH <- as.data.frame(pH)


dat$group <- paste(dat$Site, dat$Plot, dat$Depth)
dat$group <- as.factor(dat$group)

Dry.shallow <- dat[which(dat$Site == "1" & dat$Depth == "(0-5)"),]
Dry.deep <- dat[which(dat$Site == "1" & dat$Depth == "(5-10)"),]
Int.shallow <- dat[which(dat$Site == "3" & dat$Depth == "(0-5)"),]
Int.deep <- dat[which(dat$Site == "3" & dat$Depth == "(5-10)"),]
Wet.shallow <- dat[which(dat$Site == "5" & dat$Depth == "(0-5)"),]
Wet.deep <- dat[which(dat$Site == "5" & dat$Depth == "(5-10)"),]

wilcox.test(pH ~ Plot, data = Dry.shallow)
wilcox.test(pH ~ Plot, data = Dry.deep)
wilcox.test(pH ~ Plot, data = Int.shallow)
wilcox.test(pH ~ Plot, data = Int.deep)
wilcox.test(pH ~ Plot, data = Wet.shallow)
wilcox.test(pH ~ Plot, data = Wet.deep)


wilcox.test(HM ~ Plot, data = Dry.shallow)
wilcox.test(HM ~ Plot, data = Dry.deep)
wilcox.test(HM ~ Plot, data = Int.shallow)
wilcox.test(HM ~ Plot, data = Int.deep)
wilcox.test(HM ~ Plot, data = Wet.shallow)
wilcox.test(HM ~ Plot, data = Wet.deep)


wilcox.test(BaseSat ~ Plot, data = Dry.shallow)
wilcox.test(BaseSat ~ Plot, data = Dry.deep)
wilcox.test(BaseSat ~ Plot, data = Int.shallow)
wilcox.test(BaseSat ~ Plot, data = Int.deep)
wilcox.test(BaseSat ~ Plot, data = Wet.shallow)
wilcox.test(BaseSat ~ Plot, data = Wet.deep)


wilcox.test(CEC ~ Plot, data = Dry.shallow)
wilcox.test(CEC ~ Plot, data = Dry.deep)
wilcox.test(CEC ~ Plot, data = Int.shallow)
wilcox.test(CEC ~ Plot, data = Int.deep)
wilcox.test(CEC ~ Plot, data = Wet.shallow)
wilcox.test(CEC ~ Plot, data = Wet.deep)

wilcox.test(AC ~ Plot, data = Dry.shallow)
wilcox.test(AC ~ Plot, data = Dry.deep)
wilcox.test(AC ~ Plot, data = Int.shallow)
wilcox.test(AC ~ Plot, data = Int.deep)
wilcox.test(AC ~ Plot, data = Wet.shallow)
wilcox.test(AC ~ Plot, data = Wet.deep)



BaseSat <- subset %>%
  group_by(Site, Plot, Depth) %>%
  summarise(mean =round(mean(BaseSat), 1), 
            se = round(sd(BaseSat)/sqrt(5), 1))
BaseSat


CEC <- subset %>%
  group_by(Site, Plot, Depth) %>%
  summarise(mean =round(mean(CEC), 1), 
            se = round(sd(CEC)/sqrt(5), 2))
CEC

EC <- subset %>%
  group_by(Site, Plot, Depth) %>%
  summarise(mean =round(mean(EC), 2), 
            se = round(sd(EC)/sqrt(5), 3))
EC

AC <- subset %>%
  group_by(Site, Plot, Depth) %>%
  summarise(mean =round(mean(AC), 1), 
            se = round(sd(AC)/sqrt(5), 2))
AC

HM <- subset %>%
  group_by(Site, Plot, Depth) %>%
  summarise(mean =round(mean(HM), 1), 
            se = round(sd(HM)/sqrt(5), 2))
HM
