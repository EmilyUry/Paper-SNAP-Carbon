

## NCDA Soil Analyses Data Summary

setwd("C:/Users/uryem/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")

library(dplyr)


dat <- read.csv("NCDA_soils_data.csv", head = T)
names(dat) <- c("Site", "Plot", "Depth", "ID", "HM", "W_V_g_cm3", "pH", "BaseSat", "AC", "CEC", "Na", "P", "K",
                "Ca", "Mg", "S", "Mn", "Cu", "Zn", "EC")
subset <- subset(dat, select = c(Site, Plot, Depth, ID, HM, pH, BaseSat, AC, CEC, EC))


pH <- subset %>%
  group_by(Site, Plot, Depth) %>%
  summarise(mean =round(mean(pH), 1), 
            se = round(sd(pH)/sqrt(5), 3))
pH


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
