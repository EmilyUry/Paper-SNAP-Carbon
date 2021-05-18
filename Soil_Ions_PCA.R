


### PCA scratch
setwd("C:/Users/uryem/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")


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


x <- x[which(x$Year == 2020),]


info <- subset(x, select = c(Site, Treatment, Depth, Core))
res <- subset(x, select = c(pH, Cl, SO4, Ca, Na, Mg, K))



pca <-prcomp(res, center = TRUE, scale = TRUE)
print(pca)
plot(pca)
plot(pca,type="line",cex.lab=1.5, cex.main=1.5)
abline(h=1,lty=3, col="red") ## keep only the first three principle components (var > 1)


## explore some correlations
cor(res$Cl, pca$x[,1])
plot((res$Cl), pca$x[,1], xlab = "Chloride", ylab = "PC1", frame = F)


summary(pca) #100% of PCA variance, but not the actual NMS axis variance
pca.scores<-pca$x
pca.loading <- pca$rotation
df <- cbind(info, pca.scores[,1:3])
df$Treatment <- as.factor(df$Treatment)
df$Depth <- as.factor(df$Depth)

#' Plot the pca
#' 
plot(df$PC1, df$PC2, pch = 16, cex = 0.9, main = "PCA all soil cores", 
     xlab = "PC1", ylab = "PC2")
arrows(0,0, pca.loading[,1]*4, pca.loading[,2]*4, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*4.5, pca.loading[,2]*4.3, row.names(pca.loading), cex = 1.2, col = "red")



#' Facet out the PCA, a plot for each experimental site
#' 
col.s <- c("#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026")   #reds
col.c <- c("#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")  ##blues

col.s <- c("#fee090", "#a50026")   #reds
col.c <- c("#e0f3f8", "#313695")  ##blues

df1 <- df[which(df$Site == "1"),]
df3 <- df[which(df$Site == "3"),]
df5 <- df[which(df$Site == "5"),]

pca.loading.sub <- pca.loading[c(1:4), c(1:4)]
rownames(pca.loading.sub) <- c("pH", "Cl, Na, Mg", "SO4, K", "Ca")

{
  tiff(filename = "PCA.tiff", height=2800, width=5600, units= "px", res=800, compression= "lzw")
  
  
  par(mfrow = c(1,3), mar = c(4,1,3,0.5), oma = c(1,4,1,1))
  plot(df1$PC1, df1$PC2, 
       xlab = "PC1", ylab = "PC2", xlim = c(-2,11), ylim = c(-5,2), 
       main = "Dry",
       pch = c(21,24)[df1$Treatment], cex = 1.5, 
       col = "black",
       bg = ifelse(df1$Treatment == "Salt", col.s[df1$Depth], col.c[df1$Depth]))
  arrows(0,0, pca.loading.sub[,1]*5, pca.loading.sub[,2]*5, length = 0.1, lwd = 1.5, col = "black")
  text(pca.loading.sub[,1]*5, pca.loading.sub[,2]*5, row.names(pca.loading.sub), cex = 1.2, col = "black", pos = 4)
  
  plot(df3$PC1, df3$PC2, 
       xlab = "PC1", ylab = "", xlim = c(-2,11), ylim = c(-5,2), 
       main = "Itermediate", 
       yaxt = 'n',
       pch = c(21,24)[df1$Treatment], cex = 1.5, 
       col = "black",
       bg = ifelse(df1$Treatment == "Salt", col.s[df1$Depth], col.c[df1$Depth]))
  arrows(0,0, pca.loading.sub[,1]*5, pca.loading.sub[,2]*5, length = 0.1, lwd = 1.5, col = "black")
  text(pca.loading.sub[,1]*5, pca.loading.sub[,2]*5, row.names(pca.loading.sub), cex = 1.2, col = "black", pos = 4)
  legend("bottomright", c("Salt (0-5)", "Salt (5-10)", "Control (0-5)", "Control (5-10)"),
         pch = c(24, 24, 21, 21), cex = 1.1,
         col = "black", pt.bg = c("#a50026", "#fee090", "#313695", "#e0f3f8"),
         title = "Treatment (depth)")
  
  plot(df5$PC1, df5$PC2,
       xlab = "PC1", ylab = "", xlim = c(-2,11), ylim = c(-5,2), 
       main = "Wet", yaxt = 'n',
       pch = c(21,24)[df1$Treatment], cex = 1.5, 
       col = "black",
       bg = ifelse(df1$Treatment == "Salt", col.s[df1$Depth], col.c[df1$Depth]))
  arrows(0,0, pca.loading.sub[,1]*5, pca.loading.sub[,2]*5, length = 0.1, lwd = 1.5, col = "black")
  text(pca.loading.sub[,1]*5, pca.loading.sub[,2]*5, row.names(pca.loading.sub), cex = 1.2, col = "black", pos = 4)
  mtext("PC2", side=2, line=2, cex=0.7, outer=TRUE)

  
  dev.off()
}





