

## Loess curve plots 

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")


library(dplyr)
library(ggplot2)
library(cowplot)



### data set-up
{
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


## data set-up 
x$Site <- as.factor(x$Site)
x$Treatment <- as.factor(x$Treatment)
x$Depth <- as.factor(x$Depth)
date <- c(rep(32, 60), rep(18, 60), rep(5, 60), rep(7, 60))
x$date <- date


depth.labs <- c("0-5 cm", "5-10 cm")
names(depth.labs) <- c("(0-5)", "(5-10)")
site.labs <- c("Dry", "Int.", "Wet")
names(site.labs) <- c("1", "3", "5")
}


######## soil response [FIGURE 1]
{
SM <- ggplot(x, aes(x=date, y = SM, color = Treatment, fill = Treatment)) +
  geom_point(cex = 0.3) +
  geom_smooth(size = 0.3) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 7) + 
  theme(strip.text.x = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.25, "cm"), legend.title = element_blank(),
        legend.position='none',
        plot.margin = margin(t=0, r = 5, b = 5, l = 6.5)) +
  #scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  scale_x_continuous(limits = c(3,34)) +
  ylab("Soil Moisture (%)") +   
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

Cl <- ggplot(x, aes(x=date, y = Cl/1000, color = Treatment, fill = Treatment)) +
  geom_point(cex = 0.3) +
  geom_smooth(size = 0.3) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 7) + 
  theme(strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.2, "cm"), legend.title = element_blank(),
        legend.position = c(0.9,0.9), 
        legend.background = element_rect(fill="#FFFFFF00"),
        plot.margin = margin(t=5, r = 5, b = 5, l = 1)) +
  #scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  scale_x_continuous(limits = c(3,34)) +
  ylab(expression(paste('Cl (mg · gds' ^-1, ')'))) +   
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

pH <- ggplot(x, aes(x=date, y = pH, color = Treatment, fill = Treatment)) +
  geom_point(cex = 0.3) +
  geom_smooth(size = 0.3) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 7) + 
  theme(strip.text.x = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.25, "cm"), legend.title = element_blank(),
        legend.position='none',
        plot.margin = margin(t=0, r = 5, b = 1, l = 10)) +
  scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  ylab("pH")
}

tiff(filename = "NewFigs/Treatment_effects.tif", height=3000, width=2400, units= "px", res=800, compression= "lzw")
plot_grid(Cl, SM, pH, labels = c("A", "B", "C"), ncol = 1, label_size = 7, rel_heights = c(20,17.5,20))
dev.off()




############ [FIGURE 2]
{
DOC <- ggplot(x, aes(x=date, y = DOC, color = Treatment, fill = Treatment)) +
  geom_point(cex = 0.3) +
  geom_smooth(size = 0.3) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 7) + 
  theme(strip.text.x = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.25, "cm"), legend.title = element_blank(),
        legend.position='none',
        plot.margin = margin(t=0, r = 5, b = 5, l = 5)) +
  #scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  scale_x_continuous(limits = c(3,34)) +
  ylab(expression(paste('DOC (mg · L'^-1, ')'))) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

Cmin <- ggplot(x, aes(x=date, y = Cmin_c, color = Treatment, fill = Treatment)) +
  geom_point(cex = 0.3) +
  geom_smooth(size = 0.3) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 7) + 
  theme(strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.2, "cm"), legend.title = element_blank(),
        legend.position = c(0.9,0.9), 
        legend.background = element_rect(fill="#FFFFFF00"),
        plot.margin = margin(t=5, r = 5, b = 5, l = 5)) +
  #scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  scale_x_continuous(limits = c(3,34)) +
  ylab(expression(paste('C'[min], ' (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


LOI <- ggplot(x, aes(x=date, y = LOI, color = Treatment, fill = Treatment)) +
  geom_point(cex = 0.3) +
  geom_smooth(size = 0.3) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 7) + 
  theme(strip.text.x = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.25, "cm"), legend.title = element_blank(),
        legend.position='none',
        plot.margin = margin(t=0, r = 5, b = 1, l = 8.5)) +
  scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  ylab("LOI (%)")
}

tiff(filename = "NewFigs/Soil_C_responses.tif", height=3000, width=2400, units= "px", res=800, compression= "lzw")
plot_grid(Cmin, DOC, LOI, labels = c("A", "B", "C"), ncol = 1, label_size = 7, rel_heights = c(20,17.5,20))
dev.off()












########## supplemental figures


