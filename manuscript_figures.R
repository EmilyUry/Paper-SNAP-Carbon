


## Manuscript Figures

setwd("C:/Users/uryem/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")


library(ggplot2)
library(data.table)
library(plotrix)
library(gridExtra)


x <- read.csv("SNAP_3year_harm.csv", head = T)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Phenol", "Suva254")

### replace NAs in nutrient (NO3, NH4, PO4) data with 0.005, which is half detection limit. 

x <- data.table(x)
x$Mg[is.na(x$Mg)] <- 0.005  ## set below detection limit
x$Ca[is.na(x$Ca)] <- 0.005
x$Mg[x$Mg < 0] <- 0.005
x$Ca[x$Ca < 0] <- 0.005

date <- c(rep("Aug 2020", 12), rep("Jun 2019", 12), rep("May 2018", 12), rep("Jul 2018", 12))
date <- factor(date, levels = c("May 2018", "Jul 2018", "Jun 2019", "Aug 2020") )


############ Figure 8 -- pH
#####

x$response <- x$pH
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("pH", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.pH <- output

### FIGURE  points and error bars 


## pH 

  x$response <- x$pH
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("pH", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  s.pH <- output
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
tiff(filename = "Fig8.tiff", height=2400, width=3200, units= "px", res=800, compression= "lzw")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
    scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
    scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab("pH") +
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
dev.off()
  
############ Figure 5 -- LOI
#####
## FIGURE  points and error bars 

x$response <- x$LOI
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("LOI", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.LOI <- output

### FIGURE  points and error bars 
labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(labs) <- c("(0-5)", "(5-10)")
tiff(filename = "Fig5.tiff", height=2400, width=3200, units= "px", res=800, compression= "lzw")
ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  xlab(" ") +
  ylab("LOI (%)") +
  geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
dev.off()

############ Figure 3 -- DOC
#####
## FIGURE  points and error bars 

x$response <- x$DOC
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("DOC", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.DOC <- output

### FIGURE  points and error bars 
labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(labs) <- c("(0-5)", "(5-10)")
tiff(filename = "Fig3.tiff", height=2400, width=3200, units= "px", res=800, compression= "lzw")
ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  xlab(" ") +
  ylab(expression(paste('DOC (mg · L'^-1, ')'))) +
  geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
dev.off()







############ Figure 1 -- SIR
#####
## FIGURE  points and error bars 


x$response <- x$SIR_s
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("SIRs", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.SIRs <- output


x$response <- x$SIR_c
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("SIRc", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.SIRc <- output

### FIGURE  points and error bars 
labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(labs) <- c("(0-5)", "(5-10)")
tiff(filename = "Fig1.tiff", height=2400, width=6400, units= "px", res=800, compression= "lzw")
a <- ggplot(s.SIRs, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(" ") +
  ylab(expression(paste('SIR (', mu, 'g C-CO'[2], ' gds'^-1, ')'))) +
  geom_errorbar(data = s.SIRs, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
b <- ggplot(s.SIRc, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  xlab(" ") +
  ylab(expression(paste('SIR (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +  
  geom_errorbar(data = s.SIRc, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
grid.arrange(a, b, nrow = 1, widths =c(2.3,3.0))
dev.off()







############ Figure 2 -- CMin
#####
## FIGURE  points and error bars 


x$response <- x$Cmin_s
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("Cmins", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.Cmins <- output


x$response <- x$Cmin_c
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("Cminc", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.Cminc <- output

### FIGURE  points and error bars 
labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(labs) <- c("(0-5)", "(5-10)")
tiff(filename = "Fig2.tiff", height=2400, width=6400, units= "px", res=800, compression= "lzw")
a <- ggplot(s.Cmins, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(" ") +
  ylab(expression(paste('C'[mineralization], '(', mu, 'g C-CO'[2], ' gds'^-1, ')'))) + 
  geom_errorbar(data = s.Cmins, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
b <- ggplot(s.Cminc, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  xlab(" ") +
  ylab(expression(paste('C'[mineralization], '(', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +    
  geom_errorbar(data = s.Cminc, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
grid.arrange(a, b, nrow = 1, widths =c(2.3,3.0))
dev.off()





############ Figure 4 -- Phenolics
#####
## FIGURE  points and error bars 


x$response <- x$Phenol
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("phenolics", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.phenol <- output


x$response <- x$Phenol/x$DOC
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("phenolics/doc", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.phenolics.doc <- output

### FIGURE  points and error bars 
labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(labs) <- c("(0-5)", "(5-10)")
tiff(filename = "Fig4.tiff", height=2400, width=6400, units= "px", res=800, compression= "lzw")
a <- ggplot(s.phenol, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(" ") +
  ylab(expression(paste('Phenolics (mg · L'^-1, ')'))) +  
  geom_errorbar(data = s.phenol, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
b <- ggplot(s.phenolics.doc, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  xlab(" ") +
  ylab(expression(paste('Phenolics (mg · mg DOC'^-1, ')'))) +     
  geom_errorbar(data = s.phenolics.doc, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
grid.arrange(a, b, nrow = 1, widths =c(2.3,3.0))
dev.off()




############ Figure 6 -- Cl and soil moisture
#####
## FIGURE  points and error bars 


x$response <- x$Cl
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("Cl", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.Cl <- output


x$response <- x$SM
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("SM", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.SM <- output

### FIGURE  points and error bars 
labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(labs) <- c("(0-5)", "(5-10)")
tiff(filename = "Fig6.tiff", height=2400, width=6400, units= "px", res=800, compression= "lzw")
a <- ggplot(s.Cl, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(" ") +
  ylab(expression(paste('Cl (ug · gds' ^-1, ')'))) +   
  geom_errorbar(data = s.Cl, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
b <- ggplot(s.SM, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  xlab(" ") +
  ylab("Soi moisture (%)") +    
  geom_errorbar(data = s.SM, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
grid.arrange(a, b, nrow = 1, widths =c(2.3,3.0))
dev.off()




############ Supplemental Figure 3 -- Roots
#####
## FIGURE  points and error bars 

x$response <- x$Roots
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("Roots", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.roots <- output

### FIGURE  points and error bars 
labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(labs) <- c("(0-5)", "(5-10)")
tiff(filename = "FigSupp3.tiff", height=2400, width=3200, units= "px", res=800, compression= "lzw")
ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Treatment, linetype = Site, shape = Site)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#000000", "#db351f"), labels = c("Control", "Salt")) +  
  scale_linetype_manual(values=c("dotted", "longdash", "solid"), labels = c("Dry", "Int.", "Wet"))+
  scale_shape_manual(values=c(17, 16, 15), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  xlab(" ") +
  ylab("Roots (g)") +
  geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
dev.off()

