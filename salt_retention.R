

## ion recover calcs and

#### ion tracer plot for talk


##setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")
library(plyr)

# data <- read.csv("2020_SNAP_master.csv", header = TRUE)
# names(data) <- c("Date","Site", "Treatment", "Depth", "Core", "Cond", "BD", 
#                  "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
#                  "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
#                  "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4")


data <- x[which(x$Date == "August 8th, 2020"),]


element <- c("Cl", "SO4", "Na", "K", "Mg", "Ca")
IO <- c(521, 23, 462, 9.4, 52, 9.4) ## mmol/kg IO  ## reported concentration of each ion in the above citation
#### note that these are per kg of "sea water mixture" made by adding 35 g of IO to DI up to 1kg 

MW <- c(35.45, 96.06, 23, 39.1, 24.3, 40.1) #gram/mol
EqF <- c(1,2,1,1,2,2)
IOg <- IO*MW/1000 ### grams of each ion in 35 grams of IO
IOgg <- IO*MW/1000/35 ## grams of each ion in 1 gram of IO
Eq <- c(35.45, 48, 23, 39, 12, 20) ## grams/Eq
IOeq <- IOgg/Eq*1000  ## mEq of each ion in 1 gram of IO



#' How much salt have we added to the plots?
#' We have done 19 salt additions to date (however only 16 of these were prior to 
#' the 2019 soil sampling effort.
#' 
#' Each addition delivers 30 pounds of salt to a treatment plot (200 m2)

22*30/200   # 22 salt additions * pounds / area

3.3/2.20462
#' So far we have added 3.3 pounds of salt per square meter
#' Or 1.49686 kg (3.3/2.20462) for each square meter

cdata <- ddply(data, c("Depth"), summarise,  ## also include "Site" and "Treatment" with "Depth"
               N    = length(BD),
               mean = mean(BD),
               sd   = sd(BD),
               se   = sd / sqrt(N))
cdata

#' So, the mean bulk density for the (0-5) cm depth is 1.4566 g/cm3
#' And, in the top 5cm of soil, there is 1m2 x 0.05m = 0.05 m3 of soil,
#' or 50,000 cm3 of soil (100 x 100 x 5).
#' 

1496.86/50000/1.4566   ## grams of salt/ area bulk density = grams salt added per gram soil

#' 0.02055 grams of salt added to each gram of soil. 
#' OR 20.55 mg of salt per gram of soil


ion.mEq <- IOeq * 0.02055 ## amount of each ion added to each gram of soil is the
## amount of each ion in each gram of IO * grams of IO added
## to each gram of soil 
ion.uEq <- ion.mEq *1000
ion.uEq              #### uEQ of each ion added to each g of soil


gIO <- IOgg * 0.02055
gIO  ## grams of each ion added per gram of soil
ugIO <- gIO*1000000   ### ug of each ion added per gram of soil

#' Cl = 306  mEq/g
#' SO4 = 27
#' Na = 271
#' K = 5.5
#' Mg = 62
#' Ca = 11
sum(ion.uEq[3:6])
#' TCC = 349.7

ions <- (data[,c(18:23)])/Eq/100*1000 ### mg/L --> mEq/L  /100 --> uEq/g 
colnames(ions)<-paste(colnames(ions), "_uEq_g", sep="")
data<-cbind(data,ions)


data <- data[which(data$Depth == "(0-5)"),]

means <- aggregate(data[,18:23], list(data$Treatment), FUN = mean)
sds <- aggregate(data[,18:23], list(data$Treatment), FUN = sd)

###### results in TABLE 1
means <- aggregate(data[,26:31], list(data$Treatment), FUN = mean)
sds <- aggregate(data[,26:31], list(data$Treatment), FUN = sd)
se <- sds[,2:7]/sqrt(15)




### percent recovery

element
ugIO

x <- data[which(data$Treatment == "Salt" | data$Treatment == "Control"),]
xS <- x[which(x$Treatment == "Salt" ),]
xC <- x[which(x$Treatment == "Control" ),]


sum.clS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
                 mean.cl = mean(Cl),
                 sd.cl   = sd(Cl))
sum.clC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
                 mean.cl = mean(Cl),
                 sd.cl   = sd(Cl))
ES.cl <- sum.clS$mean.cl - sum.clC$mean.cl
err.cl <- sqrt((sum.clS$sd.cl)^2 + (sum.clC$sd.cl)^2)

ES.cl/ugIO[1]*100   ### percent recovery of Chloride at each site



sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(SO4),
              sd   = sd(SO4))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(SO4),
              sd   = sd(SO4))
ES.SO4 <- sumS$mean - sumC$mean
err.SO4 <- sqrt((sumS$sd)^2 + (sumC$sd)^2)

ES.SO4/ugIO[2]*100 


sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Na),
              sd   = sd(Na))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Na),
              sd   = sd(Na))
ES.Na <- sumS$mean - sumC$mean
err.Na <- sqrt((sumS$sd)^2 + (sumC$sd)^2)

ES.Na/ugIO[3]*100 

element

sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(K),
              sd   = sd(K))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(K),
              sd   = sd(K))
ES.K <- sumS$mean - sumC$mean
err.K <- sqrt((sumS$sd)^2 + (sumC$sd)^2)

ES.K/ugIO[4]*100 





sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Mg),
              sd   = sd(Mg))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Mg),
              sd   = sd(Mg))
#sumC[is.na(sumC)] <- 0
ES.Mg <- sumS$mean - sumC$mean
err.Mg <- sqrt((sumS$sd)^2 + (sumC$sd)^2)

element
ES.Mg/ugIO[5]*100 




sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Ca),
              sd   = sd(Ca))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Ca),
              sd   = sd(Ca))
ES.Ca <- sumS$mean - sumC$mean
err.Ca <- sqrt((sumS$sd)^2 + (sumC$sd)^2)


ES.Ca/ugIO[6]*100 




### end



