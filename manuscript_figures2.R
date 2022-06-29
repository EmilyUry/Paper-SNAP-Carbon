

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



############ [FIGURE 3]  -- effect size

### Data Setup
{x$TS_ueq <- x$Cl/35.35 + x$SO4/96.056*2 + x$Na/22.99 + x$K/39.098 + x$Mg/24.305*2 + x$Ca/40.078*2
data <- x %>%
  group_by(Site, Date, Treatment, Depth) %>%
  summarise(DOC_mean = mean(DOC), DOC_sd = sd(DOC),
            Phenol_mean = mean(Phenol), Phenol_sd = sd(Phenol),
            PhenolDOC_mean = mean(PhenolDOC), PhenolDOC_sd = sd(PhenolDOC),
            
            LOI_mean = mean(LOI), LOI_sd = sd(LOI),
            
            Cmin_c_mean = mean(Cmin_c), Cmin_c_sd = sd(Cmin_c),
            Cmin_s_mean = mean(Cmin_s), Cmin_s_sd = sd(Cmin_s),
            SIR_c_mean = mean(SIR_c, na.rm = TRUE), SIR_c_sd = sd(SIR_c, na.rm = TRUE),
            SIR_s_mean = mean(SIR_s, na.rm = TRUE), SIR_s_sd = sd(SIR_s, na.rm = TRUE),
            
            pH_mean = mean(pH), pH_sd = sd(pH),
            
            Roots_mean = mean(Roots), Roots_sd = sd(Roots),
            Cl_mean = mean(Cl), Cl_sd = sd(Cl),
            TS_ueq_mean = mean(TS_ueq), TS_ueq_sd = sd(TS_ueq),)

d2 <- data %>%
  group_by(Site, Date, Depth) %>%
  summarise(DOC_sd_pooled = sqrt(mean(DOC_sd^2)),
            Phenol_sd_pooled = sqrt(mean(Phenol_sd^2)),
            PhenolDOC_sd_pooled = sqrt(mean(PhenolDOC_sd^2)),
            LOI_sd_pooled = sqrt(mean(LOI_sd^2)),
            Cmin_c_sd_pooled = sqrt(mean(Cmin_c_sd^2)),
            Cmin_s_sd_pooled = sqrt(mean(Cmin_s_sd^2)),
            SIR_c_sd_pooled = sqrt(mean(SIR_c_sd^2)),
            SIR_s_sd_pooled = sqrt(mean(SIR_s_sd^2)),
            pH_sd_pooled = sqrt(mean(pH_sd^2)),
            Roots_sd_pooled = sqrt(mean(Roots_sd^2)),
            Cl_sd_pooled = sqrt(mean(Cl_sd^2)),
            TS_ueq_sd_pooled = sqrt(mean(TS_ueq_sd^2)))


d3 <- data %>%
  left_join(d2)

Hedges <- d3 %>%
  group_by(Site, Date, Depth) %>%
  summarise(DOC_Hedges = diff(DOC_mean)/DOC_sd_pooled,
            Phenol_Hedges = diff(Phenol_mean)/Phenol_sd_pooled,
            PhenolDOC_Hedges = diff(PhenolDOC_mean)/PhenolDOC_sd_pooled,
            LOI_Hedges = diff(LOI_mean)/LOI_sd_pooled,
            Cmin_c_Hedges = diff(Cmin_c_mean)/Cmin_c_sd_pooled,
            Cmin_s_Hedges = diff(Cmin_s_mean)/Cmin_s_sd_pooled,
            SIR_c_Hedges = diff(SIR_c_mean)/SIR_c_sd_pooled,
            SIR_s_Hedges = diff(SIR_s_mean)/SIR_s_sd_pooled,
            pH_Hedges = diff(pH_mean)/pH_sd_pooled,
            Roots_Hedges = diff(Roots_mean)/Roots_sd_pooled,
            Cl_Hedges = diff(Cl_mean)/Cl_sd_pooled,
            TS_ueq_Hedges = diff(TS_ueq_mean)/TS_ueq_sd_pooled ) %>%
  distinct() %>%
  left_join(d2)

levels(Hedges$Site) <- c("Dry", "Int.", "Wet")
Hedges$Date <- as.factor(Hedges$Date)
levels(Hedges$Date) <- c("Aug 2020", "Jul 2018", "Jun 2019", "May 2018")
Hedges$Date <- factor(Hedges$Date, levels = c("May 2018", "Jul 2018",  "Jun 2019", "Aug 2020"))

}

### plot
{
## Cmin_c
HCmin <- ggplot(Hedges, aes(x = Cl_Hedges, y = Cmin_c_Hedges, pch = Site, color = Date)) +
  geom_point(cex = 1) +
  theme_bw(base_size = 8) +
  facet_grid(Depth ~ .) +
  scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = Cmin_c_Hedges - Cmin_c_sd_pooled, ymax = Cmin_c_Hedges + Cmin_c_sd_pooled,
                    width = 0.5)) +
  xlab("Soil Cl-, Effect Size") +
  ylab("Cmin, Effect Size") +
  theme(legend.position = "none",
        strip.background = element_rect(colour="black", fill="white",),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0, 5, 10))
  
  
## DOC
HDOC <- ggplot(Hedges, aes(x = Cl_Hedges, y = DOC_Hedges, pch = Site, color = Date)) +
  geom_point(cex = 1) +
  theme_bw(base_size = 8) +
  facet_grid(Depth ~ .) +
  scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = DOC_Hedges - DOC_sd_pooled, ymax = DOC_Hedges + DOC_sd_pooled,
                    width = 0.5)) +
  xlab("Soil Cl-, Effect Size") +
  ylab("DOC, Effect Size") +
  theme(legend.position = "none",
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0, 5, 10))

## LOI
HLOI <- ggplot(Hedges, aes(x = Cl_Hedges, y = LOI_Hedges, pch = Site, color = Date)) +
  geom_point(cex = 1) +
  theme_bw(base_size = 8) +
  facet_grid(Depth ~ .) +
  scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = LOI_Hedges - LOI_sd_pooled, ymax = LOI_Hedges + LOI_sd_pooled,
                    width = 0.5)) +
  xlab("Soil Cl-, Effect Size") +
  ylab("LOI, Effect Size") +
  scale_x_continuous(breaks = c(0, 5, 10)) +
  theme(strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0, 'lines'))

}

tiff(filename = "NewFigs/effectsize.tif", height=1800, width=4200, units= "px", res=800, compression= "lzw")
plot_grid(HCmin, HDOC, HLOI, labels = c("A", "B", "C"), ncol = 3, label_size = 9, rel_widths= c(3.1,3,4.5))
dev.off()


### stats
{
shallow <- Hedges[which(Hedges$Depth == "(0-5)"),]
deep <- Hedges[which(Hedges$Depth != "(0-5)"),]


fit1 <- lm(Cmin_c_Hedges ~ Cl_Hedges, data = shallow)
summary(fit1)
fit2 <- lm(Cmin_c_Hedges ~ Cl_Hedges, data = deep)
summary(fit2)
fit3 <- lm(DOC_Hedges ~ Cl_Hedges, data = shallow)
summary(fit3)
fit4 <- lm(DOC_Hedges ~ Cl_Hedges, data = deep)
summary(fit4)
fit5 <- lm(LOI_Hedges ~ Cl_Hedges, data = shallow)
summary(fit5)
fit6 <- lm(LOI_Hedges ~ Cl_Hedges, data = deep)
summary(fit6)
}





#### [FIGURE 4] Vegetation responses
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
  theme_bw(base_size = 7) +
  theme(strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = margin(t=5, r = 17, b = 0, l = 7)) +
  ylab("Tree growth (%)") +
  xlab(" ")

roots <- ggplot(x, aes(x=date, y = Roots, color = Treatment, fill = Treatment)) +
  geom_point(cex = 0.3) +
  geom_smooth(size = 0.3) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("gray50", "#fc796f")) +
  facet_grid(Depth ~ Site, labeller = labeller(Depth = depth.labs, Site = site.labs)) +
  theme_bw(base_size = 7) + 
  theme(strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.2, "cm"), legend.title = element_blank(),
        legend.position= c(0.9, 0.9),
        legend.background = element_rect(fill="#FFFFFF00"),
        plot.margin = margin(t=0, r = 6, b = 5, l = 13)) +
  scale_x_continuous(name = " ", breaks = c(6, 18, 30), labels = c("2018", "2019", "2020"), limits = c(3,34)) +
  ylab("Roots (g, dry)") 
}

tiff(filename = "NewFigs/veg.tif", height=2000, width=2400, units= "px", res=800, compression= "lzw")
plot_grid(tree, roots, labels = c("A", "B"), ncol = 1, label_size = 9, rel_heights = c(2.5,3))
dev.off()

########## supplemental figures


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





