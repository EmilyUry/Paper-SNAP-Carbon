


## Manuscript Figures

setwd("C:/Users/uryem/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")


library(ggplot2)
library(data.table)
library(plotrix)


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


############ Figure 2 -- pH
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
tiff(filename = "Fig2.tiff", height=2400, width=3200, units= "px", res=800, compression= "lzw")
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
  

  #####


