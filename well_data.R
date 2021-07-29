
library(ggplot2)
library(ggpubr)
##Well data from Marcelo


setwd("C:/Users/uryem/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/well_data_2")

C1 <- read.csv("Site_1_control.csv", skip = 1)
C3 <- read.csv("Site_3_control.csv", skip = 1)
C5 <- read.csv("Site_5_control.csv", skip = 1)

S1 <- read.csv("Site_1_salt.csv", skip = 1)
S3 <- read.csv("Site_3_salt.csv", skip = 1)
S5 <- read.csv("Site_5_salt.csv", skip = 1)


head(S1)
names(S1) <- c("no.", "date_time", "pres", "temp", "pres_b", "sens", "surf", "water_depth", "d", "e", "f")
names(S3) <- c("no.", "date_time", "pres", "temp", "pres_b", "sens", "surf", "water_depth", "d", "e", "f", "g")
names(S5) <- c("no.", "date_time", "pres", "temp", "pres_b", "sens", "surf", "water_depth", "d", "e", "f", "g")


names(C1) <- c("no.", "date_time", "pres", "temp", "pres_b", "sens", "surf", "water_depth", "d", "e", "f", "g", "h", "i")
names(C3) <- c("no.", "date_time", "pres", "temp", "pres_b", "sens", "surf", "water_depth", "d", "e", "f", "g")
names(C5) <- c("no.", "date_time", "pres", "temp", "pres_b", "sens", "surf", "water_depth", "d", "e", "f", "g")



uS1 <- mean(as.numeric(S1$water_depth), na.rm = TRUE)
hist(as.numeric(S1$water_depth), na.rm = TRUE, xlim = c(-1,1))
hist(as.numeric(S3$water_depth), na.rm = TRUE, xlim = c(-1,1))
hist(as.numeric(S5$water_depth), na.rm = TRUE, xlim = c(-1,1), breaks = 200)
hist(as.numeric(C1$water_depth), na.rm = TRUE, xlim = c(-1,1))
hist(as.numeric(C3$water_depth), na.rm = TRUE, xlim = c(-1,1))
hist(as.numeric(C5$water_depth), na.rm = TRUE, xlim = c(-1,1))



s1 <- as.numeric(S1$water_depth)
s3 <- as.numeric(S3$water_depth)
s5 <- as.numeric(S5$water_depth)
c1 <- as.numeric(C1$water_depth)
c3 <- as.numeric(C3$water_depth)
c5 <- as.numeric(C5$water_depth)

d <- as.data.frame(stack(list(s1 = s1, c1 = c1,  s3 = s3, c3 = c3, s5 = s5, c5 = c5)))
dd <- as.data.frame(stack(list(Dry = c(s1, c1), Int = c(s3, c3), Wet = c(s5, c5))))

ggplot(d, aes(x = ind, y = values)) + 
  geom_boxplot() +
  theme_bw() +
  ylim(-0.1,1)



theme_set(
  theme_pubr() +
    theme(legend.position = "right")
)

tiff(filename = "Well_levle.tiff", height=2800, width=2800, units= "px", res=800, compression= "lzw")
ggplot(dd, aes(x = ind, y = values)) + 
  geom_boxplot() +
  ylim(-0.1,1) +
  theme(axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16))+ 
  labs(x = " ", y = "Water depth (m)")
dev.off()

site1 <- dd[which(dd$ind == "site1"),]
medSite1 <- median(site1$values, na.rm = TRUE)
site3 <- dd[which(dd$ind == "site3"),]
medSite3 <- median(site3$values, na.rm = TRUE)
site5 <- dd[which(dd$ind == "site5"),]
medSite5 <- median(site5$values, na.rm = TRUE)






medS1 <- median(as.numeric(S1$water_depth), na.rm = TRUE)
medS3 <- median(as.numeric(S3$water_depth), na.rm = TRUE)
medS5 <- median(as.numeric(S5$water_depth), na.rm = TRUE)
medC1 <- median(as.numeric(C1$water_depth), na.rm = TRUE)
medC3 <- median(as.numeric(C3$water_depth), na.rm = TRUE)
medC5 <- median(as.numeric(C5$water_depth), na.rm = TRUE)



