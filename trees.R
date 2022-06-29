

library(ggplot2)

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")
data <- read.csv("Trees_2021.csv", head = TRUE)

data$fSite <- as.factor(data$Site)
data$fTreatment <- as.factor(data$Treatment)

data$growth <- data$X2021_DBH_cm - data$X2015_DBH_cm
data$pc <- (data$X2021_DBH_cm - data$X2015_DBH_cm)/data$X2015_DBH_cm*100


plot(data$X2021_DBH_cm, data$X2015_DBH_cm, col = data$Site)


df <- data[which(data$Treatment == "Salt" | data$Treatment == "Control"),]
df <- na.omit(df)


ggplot(df, aes(x = Treatment, y = growth)) +
  geom_boxplot() +
  facet_grid(. ~ Site)

aov <- aov(growth ~ as.factor(fTreatment)*as.factor(fSite), df)
summary(aov)
TukeyHSD(aov)


###############FIGURE PLOT

labs <- c("Dry", "Intm.", "Wet")
names(labs) <- c("1","3", "5")

ggplot(df, aes(x = Treatment, y = pc)) +
  geom_boxplot() +
  facet_grid(. ~ Site, labeller = labeller(Site = labs))+
  theme_bw() +
  ylab("Tree growth (%)")

aov <- aov(pc ~ as.factor(fTreatment)*as.factor(fSite), df)
summary(aov)
TukeyHSD(aov)



library(data.table)
library(plotrix)

ddt <- data.table(df)

output <- ddt[,.("mean" = round(mean(pc),4), "se" = round(std.error(pc),3), "sdev" = round(sd(pc), 3)),
            by = c("fSite", "fTreatment")]
output <- ddt[,.("count" = length(pc), "mean" = round(mean(pc),4), "se" = round(std.error(pc),3), "sdev" = round(sd(pc), 3)),
              by = c("fSite", "fTreatment")]

##########
#############supplement FIGURE

ggplot(df, aes(x = Treatment, y = pc, fill = Species)) +
  geom_boxplot() +
  facet_grid(Site ~ .) +
  theme_bw() +
  ylab("Tree growth (%)")


list <- c("LIQSTY", "PERPAL", "PINTAE", "QUEALB", "QUEMIC", "QUENIG", "QUEPAG", 
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







mod <- aov(pc ~ fSite*fTreatment, df)
summary(mod)
TukeyHSD(mod)


Site1 <- df[which(df$Site == "1"),]
Site3 <- df[which(df$Site == "3"),]
Site5 <- df[which(df$Site == "5"),]

M1 <- aov(pc ~ Treatment, Site1)
summary(M1)

M1 <- aov(pc ~ Treatment, Site3)
summary(M1)

M1 <- aov(pc ~ Treatment, Site5)
summary(M1)

ggplot(Site1, aes(x = Treatment, y = pc, fill = Species)) +
  geom_boxplot() 

quni <- Site1[which(Site1$Species == "QUENIG"),]
qn <- aov(pc ~ Treatment, quni)
summary(qn)




ggplot(Site3, aes(x = Treatment, y = pc, fill = Species)) +
  geom_boxplot() 

qupa <- Site3[which(Site3$Species == "QUEPAG"),]
qp <- aov(pc ~ Treatment, qupa)
summary(qp)


ggplot(Site5, aes(x = Treatment, y = pc, fill = Species)) +
  geom_boxplot()

pita <- Site5[which(Site5$Species == "PINTAE"),]
pt <- aov(pc ~ Treatment, pita)
summary(pt)
