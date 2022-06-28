


## effect size


## look at change in response vs change in salinity
## calculate Hedge's G (effect size), with pooled standard deviation

setwd("C:/Users/uryem/OneDrive - University of Waterloo/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")


library(dplyr)
library(ggplot2)


x <- read.csv("SNAP_3year_harm.csv", head = T)


names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Phenol", "Suva254")

x$PhenolDOC <- x$Phenol/x$DOC

## reorder and select variables
x <- subset(x, select = c("Date", "Site", "Treatment", "Depth", "Core", "Roots", "pH", "LOI", 
                          "DOC", "Phenol", "PhenolDOC", "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", 
                          "SM", "BD", "Cl", "SO4", "Na", "K", "Mg", "Ca", "TDN"
))

### replace NAs with 0.005, which is half detection limit. 

x$Mg[is.na(x$Mg)] <- 0.005
x$Ca[is.na(x$Ca)] <- 0.005
x$Mg[x$Mg < 0] <- 0.005
x$Ca[x$Ca < 0] <- 0.005

x$TS_ueq <- x$Cl/35.35 + x$SO4/96.056*2 + x$Na/22.99 + x$K/39.098 + x$Mg/24.305*2 + x$Ca/40.078*2



## data set-up 
{  x$Site <- as.factor(x$Site)
  levels(x$Site) <- c("Dry", "Int.", "Wet")
  x$Date <- as.factor(x$Date)
  x$Date <- factor(x$Date, levels = c("May 10th, 2018", "July 10th, 2018",  "June 20th, 2019", "August 8th, 2020"  ))
  x$Treatment <- as.factor(x$Treatment)
  x$Depth <- as.factor(x$Depth)
}

  
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


plot(Hedges$TS_ueq_Hedges, Hedges$DOC_Hedges, pch = 21, col = Hedges$Site, 
     bg = Hedges$Site, cex = c(0.8, 1, 1.2, 1.4)[Hedges$Date])


## LOI
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = LOI_Hedges, color = Site)) +
  geom_point(cex = 2) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = LOI_Hedges - LOI_sd_pooled, ymax = LOI_Hedges + LOI_sd_pooled,
                    width = 0.5))

  
## DOC
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = DOC_Hedges, color = Site)) +
  geom_point(cex = 4) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = DOC_Hedges - DOC_sd_pooled, ymax = DOC_Hedges + DOC_sd_pooled,
                    width = 0.5))

## Phenol
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = Phenol_Hedges, color = Site)) +
  geom_point(cex = 3) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = Phenol_Hedges - Phenol_sd_pooled, ymax = Phenol_Hedges + Phenol_sd_pooled,
                    width = 0.5))

## PhenolDOC
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = PhenolDOC_Hedges, color = Site)) +
  geom_point(cex = 2) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = PhenolDOC_Hedges - PhenolDOC_sd_pooled, ymax = PhenolDOC_Hedges + PhenolDOC_sd_pooled,
                    width = 0.5))

## Cmin_c
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = Cmin_c_Hedges, color = Site)) +
  geom_point(cex = 3) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = Cmin_c_Hedges - Cmin_c_sd_pooled, ymax = Cmin_c_Hedges + Cmin_c_sd_pooled,
                    width = 0.5)) #+
  geom_errorbar(aes(xmin = TS_ueq_Hedges - TS_ueq_sd_pooled, xmax = TS_ueq_Hedges + TS_ueq_sd_pooled,
                    width = 0.5))

  
## Cmin_s
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = Cmin_s_Hedges, color = Site)) +
  geom_point(cex = 3) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = Cmin_s_Hedges - Cmin_s_sd_pooled, ymax = Cmin_s_Hedges + Cmin_s_sd_pooled,
                    width = 0.5))  
  
## SIR_c
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = SIR_c_Hedges, color = Site)) +
  geom_point(cex = 3) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = SIR_c_Hedges - SIR_c_sd_pooled, ymax = SIR_c_Hedges + SIR_c_sd_pooled,
                    width = 0.5))  

## SIR_s
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = SIR_s_Hedges, color = Site)) +
  geom_point(cex = 3) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = SIR_s_Hedges - SIR_s_sd_pooled, ymax = SIR_s_Hedges + SIR_s_sd_pooled,
                    width = 0.5))    
  

## Cl
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = Cl_Hedges, color = Site)) +
  geom_point(cex = 4) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = Cl_Hedges - Cl_sd_pooled, ymax = Cl_Hedges + Cl_sd_pooled,
                    width = 0.5))


## roots
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = Roots_Hedges, color = Site)) +
  geom_point(cex = 4) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = Roots_Hedges - Roots_sd_pooled, ymax = Roots_Hedges + Roots_sd_pooled,
                    width = 0.5))

## pH
ggplot(Hedges, aes(x = TS_ueq_Hedges, y = pH_Hedges, color = Site)) +
  geom_point(cex = 2) +
  theme_bw(base_size = 20) +
  facet_grid(Depth ~ Date) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = pH_Hedges - pH_sd_pooled, ymax = pH_Hedges + pH_sd_pooled,
                    width = 0.5)) #+
  geom_errorbar(aes(xmin = TS_ueq_Hedges - TS_ueq_sd_pooled, xmax = TS_ueq_Hedges + TS_ueq_sd_pooled,
                    width = 0.5))


########################
  
  
  
  
  
  ## Phenol
  ggplot(Hedges, aes(x = Cl_Hedges, y = Phenol_Hedges, color = Site)) +
    geom_point(cex = 3) +
    theme_bw(base_size = 20) +
    facet_grid(Depth ~ Date) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = Phenol_Hedges - Phenol_sd_pooled, ymax = Phenol_Hedges + Phenol_sd_pooled,
                      width = 0.5))
  
  

  
  
  
    
  
## Cmin_c
  ggplot(Hedges, aes(x = TS_ueq_Hedges, y = Cmin_c_Hedges, pch = Site, color = Date)) +
    geom_point(cex = 3) +
    theme_bw(base_size = 20) +
    facet_grid(Depth ~ .) +
    scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = Cmin_c_Hedges - Cmin_c_sd_pooled, ymax = Cmin_c_Hedges + Cmin_c_sd_pooled,
                      width = 0.5)) 
  ## SIR_s
  ggplot(Hedges, aes(x = TS_ueq_Hedges, y = SIR_s_Hedges, pch = Site, color = Date)) +
    geom_point(cex = 3) +
    theme_bw(base_size = 20) +
    facet_grid(Depth ~ .) +
    scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = SIR_s_Hedges - SIR_s_sd_pooled, ymax = SIR_s_Hedges + SIR_s_sd_pooled,
                      width = 0.5))   
  
  
fit <- lm(SIR_s_Hedges ~ TS_ueq_Hedges + Depth , Hedges)  
summary(fit)
  
  ## LOI
  ggplot(Hedges, aes(x = TS_ueq_Hedges, y = LOI_Hedges, pch = Site, color = Date)) +
    geom_point(cex = 3) +
    theme_bw(base_size = 20) +
    facet_grid(Depth ~ .) +
    scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = LOI_Hedges - LOI_sd_pooled, ymax = LOI_Hedges + LOI_sd_pooled,
                      width = 0.5))
  
  
  ## DOC
  ggplot(Hedges, aes(x = TS_ueq_Hedges, y = DOC_Hedges, pch = Site, color = Date)) +
    geom_point(cex = 3) +
    theme_bw(base_size = 20) +
    facet_grid(Depth ~ .) +
    scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = DOC_Hedges - DOC_sd_pooled, ymax = DOC_Hedges + DOC_sd_pooled,
                      width = 0.5))
  
  ## Phenol
  ggplot(Hedges, aes(x = TS_ueq_Hedges, y = Phenol_Hedges, pch = Site, color = Date)) +
    geom_point(cex = 3) +
    theme_bw(base_size = 20) +
    facet_grid(Depth ~ .) +
    scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = Phenol_Hedges - Phenol_sd_pooled, ymax = Phenol_Hedges + Phenol_sd_pooled,
                      width = 0.5))
  
  fit <- lm(Phenol_Hedges ~ TS_ueq_Hedges + Depth , Hedges)  
  summary(fit)
  
  
  
  ## PhenolDOC
  ggplot(Hedges, aes(x = TS_ueq_Hedges, y = PhenolDOC_Hedges, pch = Site, color = Date)) +
    geom_point(cex = 3) +
    theme_bw(base_size = 20) +
    facet_grid(Depth ~ .) +
    scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = PhenolDOC_Hedges - PhenolDOC_sd_pooled, ymax = PhenolDOC_Hedges + PhenolDOC_sd_pooled,
                      width = 0.5))

  
  
  ## roots
  ggplot(Hedges, aes(x = TS_ueq_Hedges, y = Roots_Hedges, pch = Site, color = Date)) +
    geom_point(cex = 3) +
    theme_bw(base_size = 20) +
    facet_grid(Depth ~ .) +
    scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = Roots_Hedges - Roots_sd_pooled, ymax = Roots_Hedges + Roots_sd_pooled,
                      width = 0.5))
  
  ## pH
  ggplot(Hedges, aes(x = TS_ueq_Hedges, y = pH_Hedges, pch = Site, color = Date)) +
    geom_point(cex = 3) +
    theme_bw(base_size = 20) +
    facet_grid(Depth ~ .) +
    scale_color_manual(values = c("#8EEFC5", "#53AFBA", "#196FB0", "#000E65")) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = pH_Hedges - pH_sd_pooled, ymax = pH_Hedges + pH_sd_pooled,
                      width = 0.5)) 
  
  fit <- lm(pH_Hedges ~ TS_ueq_Hedges + Depth , Hedges)  
  summary(fit)
  