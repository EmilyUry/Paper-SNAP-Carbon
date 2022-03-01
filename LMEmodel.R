


## statistical analysis of SNAP data (2018-2020)

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")


library(nlme)
#library(lme4)

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

### log transform ions 
lX<-log(x[,c(18:23)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)

x$ID<-paste(x$Site,x$Treatment,x$Core,sep="")
x$Site <- as.factor(x$Site)




## Subset, shallow depth only
x5 <- x[which(x$Depth == "(0-5)"),]
x5$month <- c(rep(46,30), rep(32,30), rep(19,30), rep(21,30))
x5$ID2<-paste(x5$Site,x5$Treatment,sep="")


## dependent var = pH
## fixed effect = date, treatment, depth, Site
## random effect = (depth and site don't have enought levels to be considered a random effect)

mod1 <- lme(pH ~ Treatment*month, random = ~ Treatment|Site, data = x5)
mod1

summary(mod1)

sum <- summary(mod1)
tab1 <- sum$tTable
tab1

anova(mod1)



library(ggplot2)

### scatter plot
plot<- ggplot(x5, aes(x=month, y=pH,  color=Treatment, shape = Treatment, group = ID), xlab(month) ) + 
  geom_point()+
  geom_line(color="grey") +
  scale_x_continuous(name="Months from start of experiment", limits=c(16, 48), breaks =c(20,32,46)) +
  scale_y_continuous(name="pH", limits=c(3, 7)) +
  facet_grid(.~Site) +
  theme_bw()

plot

### boxplot
x5$Month <- as.factor(x5$month)

labs = c("Dry", "Intermediate", "Wet")
names(labs) <- c("1", "3", "5")


plot<- ggplot(x5, aes(x=Month, y=pH,  fill = Treatment), xlab(month) ) + 
  geom_boxplot() +
  #geom_line(color="grey") +
  scale_x_discrete(name=" ", breaks = c(19, 21, 32, 46), labels = c("M'18", "J'18", "J'19", "A'20"), limits = c(19, 21, "skip", 32, "skip", 46)) +
    scale_y_continuous(name="pH", limits=c(3, 7)) +
  facet_grid(.~Site, labeller = labeller(Site = labs)) + 
  scale_fill_manual(values=c("#FFFFFF", "#db351f")) +
  theme_bw() +
  theme(text = element_text(size = 14), strip.text.x = element_text(size = 16), panel.grid.minor = element_blank())
plot

tiff(filename = "Figures/Fig8.tiff", height= 2400, width=5600, units= "px", res=800, compression= "lzw")
plot
dev.off()



mod1 <- lmer(pH ~ Treatment + Date + Depth + (1|Site), data = x)


















