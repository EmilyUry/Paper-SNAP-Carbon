

## NCDA Soil Analyses Data Summary

setwd("C:/Users/uryem/Desktop/DukeBioDrop_backup/Ch3_SNAP_Carbon/data")


library(qwraps2)
library(dplyr)
options(qwraps2_markup = "markdown")

dat <- read.csv("NCDA_soils_data.csv", head = T)

names(dat) <- c("Site", "Plot", "Depth", "ID", "Moisture", "W_V_g_cm3", "pH", "BaseSat", "AC", "CEC", "Na", "P", "K",
                "Ca", "Mg", "S", "Mn", "Cu", "Zn", "EC")
subset <- subset(dat, select = c(Site, Plot, Depth, ID, Moisture, pH, BaseSat, AC, CEC, EC))


mean_sd(dat$pH, denote_sd = "paren")
mci <- mean_ci(dat$pH)
print(mci, show_level = TRUE)
median_iqr(dat$pH)

ST <- list("pH" = 
             list("mean (sd)" = ~qwraps2::mean_sd(pH)),
           "Base Saturation (0-5)" =
             list("mean (sd)" = ~qwraps2::mean_sd(BaseSat[which(Depth == "(0-5)")])),
           "Base Saturation (5-10)" =
             list("mean (sd)" = ~qwraps2::mean_sd(BaseSat[which(Depth == "(5-10)")])),
)

table <- summary_table(dat, ST, by = c("Plot", "Site"))
table




ST <- list("pH" = 
             list("mean (sd)" = ~qwraps2::mean_sd(pH)),
           "Base Saturation" =
             list("mean (sd)" = ~qwraps2::mean_sd(BaseSat))
)

table <- summary_table(dat, ST, by = c("Depth", "Plot", "Site"))
table
