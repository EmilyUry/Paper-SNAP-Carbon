
## Manuscript Figures

setwd("C:/Users/uryem/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")

library(dplyr)
library(tidyr)

#####
# Supplement


# Fig. S2 Well data
#####


data <- read.csv("Site5_Welldata.csv", head = TRUE)
names(data) <- c("date_c", "sal_c", "sal_c2", " ", " ", " ", "date_S", "sal_S", "sal_S2")
data <- data[-3874,]  # remove outliers
data <- data[-3874,]


tiff(filename = "FigS2.tiff", height=3600, width=5600, units= "px", res=800, compression= "lzw")

plot(data$sal_S, type = 'l', col = "red", xlim = c(-29000, 97000), 
     ylim = c(0, 18000), xaxt = 'n',
     xlab = ' ', ylab = "Conductivity uS/cm")
points(data$sal_c2, type = 'l')
legend(70000, 14000, c("Salt Addition","Salt","Control"), cex = 0.8, pch = c(25, 20, 20), 
       pt.cex = c(1.3,0.01,0.01), pt.bg = c("black"),
       col = c("white", "red", "black"), lty = 1)
axis(1, c(-28000, 30000, 90000), c("October 2016", "February 2018", "June 2020"))       

### salt additions marks

SA <- c(-28000,-25216,-1930,700,6600,13200,16400,33330,39400,45370,52600,
        57000,61840,64000,67820,70480,74310,76654,92934,
        94566,96774)
ys <- rep(17000, 21)
points(SA, ys, pch = 25, bg = "black")

dev.off()

#####
# Fig. S5 Tree growth by species
#####
