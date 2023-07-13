

## Figures for submission 2 of manuscript

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
  geom_point(cex = 1) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 14) + 
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
  geom_point(cex = 1) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 14) + 
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
  geom_point(cex = 1) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 14) + 
  theme(strip.text.x = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.25, "cm"), legend.title = element_blank(),
        legend.position='none',
        plot.margin = margin(t=0, r = 5, b = 1, l = 10)) +
  scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  ylab("pH")
}

#tiff(filename = "NewFigs/Treatment_effects.tif", height=3000, width=2400, units= "px", res=800, compression= "lzw")

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/Second Submission - Plos1/Figures")
tiff(filename = "Treatment_effects.tif", height=6.5, width=5.5, units= "in", res=800, compression= "lzw")

plot_grid(Cl, SM, pH, labels = c("A", "B", "C"), ncol = 1, label_size = 11, rel_heights = c(20,17.5,20))
dev.off()


plot(x$SM, log(x$Cl))
fit1 <- lm(SM ~ log(Cl), x)
summary(fit1)

############ [FIGURE 3]
{
DOC <- ggplot(x, aes(x=date, y = DOC, color = Treatment, fill = Treatment)) +
  geom_point(cex = 1) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 14) + 
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
  geom_point(cex = 1) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 14) + 
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
  geom_point(cex = 1) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 14) + 
  theme(strip.text.x = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.25, "cm"), legend.title = element_blank(),
        legend.position='none',
        plot.margin = margin(t=0, r = 5, b = 1, l = 8.5)) +
  scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  ylab("LOI (%)")
}

#tiff(filename = "NewFigs/Soil_C_responses.tif", height=3000, width=2400, units= "px", res=800, compression= "lzw")
tiff(filename = "Soil_C_responses.tif", height=6.5, width=5.5, units= "in", res=800, compression= "lzw")

plot_grid(Cmin, DOC, LOI, labels = c("A", "B", "C"), ncol = 1, label_size = 11, rel_heights = c(20,17.5,20))
dev.off()




#### [FIGURE 4] Vegetation responses
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")

{
data <- read.csv("Trees_2021.csv", head = TRUE)

data$fSite <- as.factor(data$Site)
data$fTreatment <- as.factor(data$Treatment)
data$growth <- data$X2021_DBH_cm - data$X2015_DBH_cm
data$pc <- (data$X2021_DBH_cm - data$X2015_DBH_cm)/data$X2015_DBH_cm*100

df <- data[which(data$Treatment == "Salt" | data$Treatment == "Control"),]
df <- na.omit(df)

labs <- c("Dry", "Int.", "Wet")
names(labs) <- c("1","3", "5")

tree <- ggplot(df, aes(x = Treatment, y = pc, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values=c("#FFFFFF", "#fc796f"))+
  facet_grid(. ~ Site, labeller = labeller(Site = labs))+
  theme_bw(base_size = 14) +
  theme(strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = margin(t=5, r = 17, b = 0, l = 7)) +
  ylab("Tree growth (%)") +
  xlab(" ")

roots <- ggplot(x, aes(x=date, y = Roots, color = Treatment, fill = Treatment)) +
  geom_point(cex = 1) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 14) + 
  theme(strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.2, "cm"), legend.title = element_blank(),
        legend.position= c(0.9, 0.9),
        legend.background = element_rect(fill="#FFFFFF00"),
        plot.margin = margin(t=0, r = 6, b = 5, l = 13)) +
  scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  ylab("Roots (g, dry)") 
}

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/Second Submission - Plos1/Figures")

tiff(filename = "veg.tif", height=5, width=6, units= "in", res=800, compression= "lzw")
plot_grid(tree, roots, labels = c("A", "B"), ncol = 1, label_size = 11, rel_heights = c(2.5,3))
dev.off()

########## supplemental figures


## tree growth by species [FIGURE S5]
{list <- c("LIQSTY", "PERPAL", "PINTAE", "QUEALB", "QUEMIC", "QUENIG", "QUEPAG", 
          "QUEPHE")
df2 <- subset(df, Species %in% list)

species <- ggplot(df2, aes(x = Treatment, y = pc, fill = Species)) +
  geom_boxplot() +
  facet_grid(Site ~ ., labeller = labeller(Site = labs)) +
  theme_bw() +
  ylab("Tree growth (%)") +
  scale_fill_discrete(name = "Species", labels = c("Liquidambar styraciflua", "Persea palustris", "Pinus taeda",
                                                   "Quercus alba", "Quercus michauxii", "Quercus nirga",
                                                   "Quercus pagoda", "Quercus phellos")) +
  theme(legend.text = element_text(face = "italic"))
}

tiff(filename = "NewFigs/tree_species.tif", height=3600, width=3600, units= "px", res=800, compression= "lzw")
species
dev.off()


############ [FIGURE S3]
{  
  Cmin_s <- ggplot(x, aes(x=date, y = Cmin_s, color = Treatment, fill = Treatment)) +
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
          plot.margin = margin(t=5, r = 5, b = 5, l = 10.5)) +
    #scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
    scale_x_continuous(limits = c(3,34)) +
    ylab(expression(paste('C'[min], ' (', mu, 'g C-CO'[2], ' gds'^-1, ')'))) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  
  SIR_c <- ggplot(x, aes(x=date, y = SIR_c, color = Treatment, fill = Treatment)) +
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
          plot.margin = margin(t=0, r = 5, b = 5, l = 9)) +
    scale_y_continuous(breaks = c(0, 50, 100, 150), limits = c(-50, 160)) +
    scale_x_continuous(limits = c(3,34)) +
    ylab(expression(paste('SIR (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  SIR_s <- ggplot(x, aes(x=date, y = SIR_s, color = Treatment, fill = Treatment)) +
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
          plot.margin = margin(t=0, r = 5, b = 1, l = 12)) +
    scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
    ylab(expression(paste('SIR (', mu, 'g C-CO'[2], ' gds'^-1, ')'))) +
    scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(-5, 16))
    
    
}

tiff(filename = "NewFigs/cmin_SIR.tif", height=3000, width=2400, units= "px", res=800, compression= "lzw")
plot_grid(Cmin_s, SIR_c, SIR_s, labels = c("A", "B", "C"), ncol = 1, label_size = 7, rel_heights = c(20,17.5,20))
dev.off()




############ [FIGURE S4] -- Phenols
{
  Phenol <- ggplot(x, aes(x=date, y = Phenol, color = Treatment, fill = Treatment)) +
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
          plot.margin = margin(t=5, r = 5, b = 5, l = 10.5)) +
    #scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
    scale_x_continuous(limits = c(3,34)) +
    ylab(expression(paste('Phenolics (mg · L'^-1, ')'))) +  
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

PhenolDOC <- ggplot(x, aes(x=date, y = PhenolDOC, color = Treatment, fill = Treatment)) +
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
        plot.margin = margin(t=0, r = 5, b = 1, l = 11)) +
  scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  ylab(expression(paste('Phenolics (mg · mg DOC'^-1, ')'))) 

}

tiff(filename = "NewFigs/Phenols.tif", height=2000, width=2400, units= "px", res=800, compression= "lzw")
plot_grid( Phenol, PhenolDOC, labels = c("A", "B", "C"), ncol = 1, label_size = 7, rel_heights = c(20,20))
dev.off()











