

### two way anovas -- SNAP --- ALL data


x <- read.csv("SNAP_3year_harm.csv", head = T)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")
x$PhenolDOC <- x$Phenol/x$DOC

## reorder and select variables
x <- subset(x, select = c("Date", "Site", "Treatment", "Depth", "Core", "Roots", "pH", "LOI", 
                          "DOC", "Phenol", "PhenolDOC", "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", 
                          "SM", "BD", "Cl", "SO4", "Na", "K", "Mg", "Ca", "TDN", "NH4", "ICNO3", "PO4"
                            ))

### replace NAs in nutrient (NO3, NH4, PO4) data with 0.005, which is half detection limit. 
x$ICNO3[is.na(x$ICNO3)] <- 0.005
x$NH4[is.na(x$NH4)] <- 0.005
x$PO4[is.na(x$PO4)] <- 0.005
x$Mg[is.na(x$Mg)] <- 0.005
x$Ca[is.na(x$Ca)] <- 0.005
x$Mg[x$Mg < 0] <- 0.005
x$Ca[x$Ca < 0] <- 0.005

## data set-up 
{
x$Site <- as.factor(x$Site)
x$Date <- as.factor(x$Date)
x$Treatment <- as.factor(x$Treatment)
x$Depth <- as.factor(x$Depth)

a <- x[which(x$Depth == "(0-5)"),]
b <- x[which(x$Depth == "(5-10)"),]

a1 <- a[which(a$Site == "1"),]
a3 <- a[which(a$Site == "3"),]
a5 <- a[which(a$Site == "5"),]

b1 <- b[which(b$Site == "1"),]
b3 <- b[which(b$Site == "3"),]
b5 <- b[which(b$Site == "5"),]

a1.1 <- a1[which(a1$Date == "May 10th, 2018"),]
a1.2 <- a1[which(a1$Date == "July 10th, 2018"),]
a1.3 <- a1[which(a1$Date == "June 20th, 2019"),]
a1.4 <- a1[which(a1$Date == "August 8th, 2020"),]

a3.1 <- a3[which(a3$Date == "May 10th, 2018"),]
a3.2 <- a3[which(a3$Date == "July 10th, 2018"),]
a3.3 <- a3[which(a3$Date == "June 20th, 2019"),]
a3.4 <- a3[which(a3$Date == "August 8th, 2020"),]

a5.1 <- a5[which(a5$Date == "May 10th, 2018"),]
a5.2 <- a5[which(a5$Date == "July 10th, 2018"),]
a5.3 <- a5[which(a5$Date == "June 20th, 2019"),]
a5.4 <- a5[which(a5$Date == "August 8th, 2020"),]

b1.1 <- b1[which(b1$Date == "May 10th, 2018"),]
b1.2 <- b1[which(b1$Date == "July 10th, 2018"),]
b1.3 <- b1[which(b1$Date == "June 20th, 2019"),]
b1.4 <- b1[which(b1$Date == "August 8th, 2020"),]

b3.1 <- b3[which(b3$Date == "May 10th, 2018"),]
b3.2 <- b3[which(b3$Date == "July 10th, 2018"),]
b3.3 <- b3[which(b3$Date == "June 20th, 2019"),]
b3.4 <- b3[which(b3$Date == "August 8th, 2020"),]

b5.1 <- b5[which(b5$Date == "May 10th, 2018"),]
b5.2 <- b5[which(b5$Date == "July 10th, 2018"),]
b5.3 <- b5[which(b5$Date == "June 20th, 2019"),]
b5.4 <- b5[which(b5$Date == "August 8th, 2020"),]


all.dfs <- list(a1.1, a1.2, a1.3, a1.4, a3.1, a3.2, a3.3, a3.4, a5.1, a5.2, a5.3, a5.4,
             b1.1, b1.2, b1.3, b1.4, b3.1, b3.2, b3.3, b3.4, b5.1, b5.2, b5.3, b5.4)
subset <- c("a1.1", "a1.2", "a1.3", "a1.4", "a3.1", "a3.2", "a3.3", "a3.4", "a5.1", "a5.2", "a5.3", "a5.4",
            "b1.1", "b1.2", "b1.3", "b1.4", "b3.1", "b3.2", "b3.3", "b3.4", "b5.1", "b5.2", "b5.3", "b5.4")
depth <- c(rep("(0-5)", 12), rep("(5-10)", 12))
site <- c(rep("1", 4), rep("3", 4), rep("5", 4), rep("1", 4), rep("3", 4), rep("5", 4))
date <- rep(c("May2018", "July2018", "June2019", "Aug2020"), 6)


rm(a,b, a1, a3, a5, b1, b3, b5, a1.1, a1.2, a1.3, a1.4, a3.1, a3.2, a3.3, a3.4, a5.1, a5.2, a5.3, a5.4,
   b1.1, b1.2, b1.3, b1.4, b3.1, b3.2, b3.3, b3.4, b5.1, b5.2, b5.3, b5.4 )

diffs <- vector(mode = "numeric", length = 24)
p.vals <- vector(mode = "numeric", length = 24)
sigs <- vector(mode = "character", length = 24)
}

####### pH
{
for(i in 1:24){
  data <- as.data.frame(all.dfs[i])
  data$response <- data$pH
  TR <- TukeyHSD(aov(response ~ Treatment, data = data))
  diff <- TR$Treatment[1]
  diffs[i] <- diff
  p.val <- TR$Treatment[4]
  p.vals[i] <- p.val
}

var <- rep("pH",24)
at.pH  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))

for(i in 1:24){
  if(at.pH[i,7] <= 0.05){
    sig <- "SIG." } else {
    sig <- "n.s."  }
  sigs[i] <- sig
    }
at.pH$significance <- sigs
}

####### LOI
{
for(i in 1:24){
  data <- as.data.frame(all.dfs[i])
  data$response <- data$LOI
  TR <- TukeyHSD(aov(response ~ Treatment, data = data))
  diff <- TR$Treatment[1]
  diffs[i] <- diff
  p.val <- TR$Treatment[4]
  p.vals[i] <- p.val
}

var <- rep("LOI",24)
at.LOI  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))

for(i in 1:24){
  if(at.LOI[i,7] <= 0.05){
    sig <- "SIG." } else {
      sig <- "n.s."  }
  sigs[i] <- sig
}
at.LOI$significance <- sigs
}

####### DOC
{
for(i in 1:24){
  data <- as.data.frame(all.dfs[i])
  data$response <- data$DOC
  TR <- TukeyHSD(aov(response ~ Treatment, data = data))
  diff <- TR$Treatment[1]
  diffs[i] <- diff
  p.val <- TR$Treatment[4]
  p.vals[i] <- p.val
}

var <- rep("DOC",24)
at.DOC  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))

for(i in 1:24){
  if(at.DOC[i,7] <= 0.05){
    sig <- "SIG." } else {
      sig <- "n.s."  }
  sigs[i] <- sig
}
at.DOC$significance <- sigs
}

####### Phenol
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$Phenol
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("Phenol",24)
  at.Phenol  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.Phenol[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.Phenol$significance <- sigs
}

####### Phenol/DOC
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$PhenolDOC
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("PhenolDOC",24)
  at.PhenolDOC  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.PhenolDOC[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.PhenolDOC$significance <- sigs
}

####### Cmin_s
{
  diffs <- vector(mode = "numeric", length = 24)
  p.vals <- vector(mode = "numeric", length = 24)
  sigs <- vector(mode = "character", length = 24)
  
  for(i in c(2,3,4,6,7,8,10,11,12,14,15,16,18,19,20,22,23,24)){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$Cmin_s
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("Cmin_s",24)
  at.Cmin_s  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.Cmin_s[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.Cmin_s$significance <- sigs
}

####### Cmin_c
{
  diffs <- vector(mode = "numeric", length = 24)
  p.vals <- vector(mode = "numeric", length = 24)
  sigs <- vector(mode = "character", length = 24)
  
  for(i in c(2,3,4,6,7,8,10,11,12,14,15,16,18,19,20,22,23,24)){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$Cmin_c
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("Cmin_c",24)
  at.Cmin_c  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.Cmin_c[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.Cmin_c$significance <- sigs
}

####### SIR_c
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$SIR_c
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("SIR_c",24)
  at.SIR_c  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.SIR_c[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.SIR_c$significance <- sigs
}

####### SIR_s
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$SIR_s
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("SIR_s",24)
  at.SIR_s  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.SIR_s[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.SIR_s$significance <- sigs
}


####### TDN
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$TDN
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("TDN",24)
  at.TDN  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.TDN[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.TDN$significance <- sigs
}

####### NH4
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$NH4
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("NH4",24)
  at.NH4  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.NH4[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.NH4$significance <- sigs
}

####### NO3
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$ICNO3
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("NO3",24)
  at.NO3  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.NO3[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.NO3$significance <- sigs
}

####### PO4
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$PO4
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("PO4",24)
  at.PO4  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.PO4[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.PO4$significance <- sigs
}

####### Roots
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$Roots
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("Roots",24)
  at.Roots  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.Roots[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.Roots$significance <- sigs
}

####### SM
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$SM
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("SM",24)
  at.SM  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.SM[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.SM$significance <- sigs
}

####### BD
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$SIR_s
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("BD",24)
  at.BD  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.BD[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.BD$significance <- sigs
}

####### Cl
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$Cl
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("Cl",24)
  at.Cl  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.Cl[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.Cl$significance <- sigs
}

####### SO4
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$SO4
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("SO4",24)
  at.SO4  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.SO4[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.SO4$significance <- sigs
}

####### Na
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$Na
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("Na",24)
  at.Na  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.Na[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.Na$significance <- sigs
}

####### K
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$K
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("K",24)
  at.K  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.K[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.K$significance <- sigs
}

####### Ca
{
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$Ca
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("Ca",24)
  at.Ca  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.Ca[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.Ca$significance <- sigs
}

####### Mg
{
  diffs <- vector(mode = "numeric", length = 24)
  p.vals <- vector(mode = "numeric", length = 24)
  sigs <- vector(mode = "character", length = 24)
  
  for(i in 1:24){
    data <- as.data.frame(all.dfs[i])
    data$response <- data$Mg
    TR <- TukeyHSD(aov(response ~ Treatment, data = data))
    diff <- TR$Treatment[1]
    diffs[i] <- diff
    p.val <- TR$Treatment[4]
    p.vals[i] <- p.val
  }
  
  var <- rep("Mg",24)
  at.Mg  <- data.frame(subset, depth, site, date, var, diffs, round(p.vals, 5))
  
  for(i in 1:24){
    if(at.Mg[i,7] <= 0.05){
      sig <- "SIG." } else {
        sig <- "n.s."  }
    sigs[i] <- sig
  }
  at.Mg$significance <- sigs
}



df <- rbind(at.pH, at.LOI, at.DOC, at.Phenol, at.PhenolDOC, at.Cmin_c, at.Cmin_s, at.SIR_c, at.SIR_s, at.Roots, at.BD, at.SM,
            at.TDN, at.NH4, at.NO3, at.PO4, at.Cl, at.SO4, at.Na, at.K, at.Mg, at.Ca)

write.csv(df, "ANOVA_all_vars2.csv")



