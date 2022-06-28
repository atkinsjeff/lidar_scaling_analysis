##### 
# variables to estimate:   LAI, FHD, Top Rugosity/Rumple, Mean & Median Height, MOCH, Canopy Cover and Canopy Relief Ratio
# 
# 
# 
# 
library(segmented)
library(tidyverse)

# set random seed
set.seed(666)

# bring in data
 df <- read.csv("bart_processed_metrics.csv")
 table(df$res)
# sort data to sampling structure. 
df %>%
    dplyr::filter(CC >= 25 & MOCH >= 5) %>%
    data.frame() -> df
table(df$res)

# set up random sample
df %>% 
    dplyr::filter(res <= 250) %>%
    dplyr::group_by(res)%>% 
    dplyr::sample_n(100) -> df.250

df %>%
    filter(res >= 500) %>%
    data.frame() -> df.500

samp.data <- rbind(df.250, df.500)





##### CANOPY COVER
samp.data %>%
    as.data.frame() %>%
    dplyr::group_by(siteID, res) %>%
    summarize(CC.var = var(CC, na.rm = TRUE),
              CC.sd = sd(CC, na.rm = TRUE),
              CC.mean = mean(CC, na.rm = TRUE)) %>%
    data.frame() -> cc

## Modified version of your function that does not rely on accessing
## variables that is external to its environment.


# CANOPY COVER
cc$CC.cv <- cc$CC.sd / cc$CC.mean
# mean
#fit simple linear regression model
fit <- lm(CC.mean ~ res, data = cc)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
pscore.test(fit, ~res)
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)

#view summary of segmented model
summary(segmented.fit)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)

#plot original data
x11()
plot(cc$res, cc$CC.mean, pch=16, col='steelblue')
plot(segmented.fit, add=T)

# CV
#fit simple linear regression model
fit <- lm(CC.cv ~ res, data = cc)
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)

#view summary of segmented model
summary(segmented.fit)

#plot original data
x11()
plot(cc$res, cc$CC.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)





# CANOPY RELIEF RATIO

samp.data %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(CRR.var = var(CRR, na.rm = TRUE),
              CRR.sd = sd(CRR, na.rm = TRUE),
              CRR.mean = mean(CRR, na.rm = TRUE)) %>%
    data.frame() -> crr
crr$CRR.cv <- crr$CRR.sd / crr$CRR.mean

# mean
#fit simple linear regression model
fit <- lm(CRR.mean ~ res, data = crr)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
pscore.test(fit, ~res)
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
# pscore.test(segmented.fit, ~res)
# davies.test(segmented.fit, ~res)

confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
x11()
plot(crr$res, crr$CRR.mean, pch = 16, col='black', cex = 1.75, 
     #ylim = c(0.3, 0.8),
     xlab = expression(paste("Spatial Resolution (m"^"2"*")")), ylab = expression(paste("Canopy Relief Ratio (CRR"[Mean]*")")))
arrows(x0=crr$res, y0 = crr$CRR.mean - crr$CRR.sd, x1 = crr$res, y1 = crr$CRR.mean + crr$CRR.sd, 
       code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)

# CV
#fit simple linear regression model
fit <- lm(CRR.cv ~ res, data = crr)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11()
plot(crr$res, crr$CRR.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)







# FHD

samp.data %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(FHD.var = var(FHD, na.rm = TRUE),
              FHD.sd = sd(FHD, na.rm = TRUE),
              FHD.mean = mean(FHD, na.rm = TRUE)) %>%
    data.frame() -> fhd

fhd$FHD.cv <- (fhd$FHD.sd / fhd$FHD.mean)

# mean
#fit simple linear regression model
fit <- lm(FHD.mean ~ res, data = fhd)
#fit <- lm(FHD ~ res, data = samp.data)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
pscore.test(fit, ~res)
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
summary(segmented.fit)


confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data

# CV
#fit simple linear regression model
fit <- lm(FHD.cv ~ res, data = fhd)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11()
plot(fhd$res, fhd$FHD.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)

# x11(width = 8, height = 5)
# plot(fhd$res, fhd$FHD.mean, pch = 16, col='black', cex = 1.75, 
#      ylim = c(1.5, 4),
#      xlab = expression(paste("Spatial Resolution (m"^"2"*")")), ylab = expression(paste("Foliar Height Diversity(FHD"[Mean]*")")))
# arrows(x0=fhd$res, y0 = fhd$FHD.mean - fhd$FHD.sd, x1 = fhd$res, y1 = fhd$FHD.mean + fhd$FHD.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
# plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)
# 
# x11(width = 8, height = 5)
# plot(samp.data$res, samp.data$FHD,  col='black', cex = 1, 
#      ylim = c(1.5, 4),
#      xlab = expression(paste("Spatial Resolution (m"^"2"*")")), ylab = expression(paste("Foliar Height Diversity(FHD"[Mean]*")")))
# # arrows(x0=samp.data$res, y0 = samp.data$FHD.mean - samp.data$FHD.sd, x1 = samp.data$res, y1 = samp.data$FHD.mean + samp.data$FHD.sd, 
# #        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
# plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)
# 




#### LAI

df.lai %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(LAI.var = var(LAI, na.rm = TRUE),
              LAI.sd = sd(LAI, na.rm = TRUE),
              LAI.mean = mean(LAI, na.rm = TRUE)) %>%
    data.frame() -> LAI

LAI$LAI.cv <- LAI$LAI.sd / LAI$LAI.mean
# mean
#fit simple linear regression model
fit <- lm(LAI.mean ~ res, data = LAI)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data



x11()
plot(LAI$res, LAI$LAI.mean, pch = 16, col='black', cex = 1.75)
# arrows(x0=crr$res, y0 = crr$CRR.mean - crr$CRR.sd, x1 = crr$res, y1 = crr$CRR.mean + crr$CRR.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)

x11(width = 8, height = 5)
plot(LAI$res, LAI$LAI.mean, pch = 16, col='black', cex = 1.75, 
     ylim = c(2, 20),
     xlab = expression(paste("Spatial Resolution (m"^"2"*")")), ylab = expression(paste("Leaf Area Index (LAI"[Mean]*")")))
arrows(x0=LAI$res, y0 = LAI$LAI.mean - LAI$LAI.sd, x1 = LAI$res, y1 = LAI$LAI.mean + LAI$LAI.sd, 
       code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)

# CV
#fit simple linear regression model
fit <- lm(LAI.cv ~ res, data = LAI)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11(width = 8, height = 5)
plot(LAI$res, LAI$LAI.cv, pch = 16, col='black', cex = 1.75, 
     ylim = c(0.1, 0.9),
     xlab = expression(paste("Spatial Resolution (m"^"2"*")")), ylab = expression(paste("Leaf Area Index (LAI"[CV]*")")))
# arrows(x0=LAI$res, y0 = LAI$LAI.cv - LAI$LAI.sd, x1 = LAI$res, y1 = LAI$LAI.cv + LAI$LAI.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)







# MEAN HEIGHT

samp.data %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(Mean_ht.var = var(Mean_ht, na.rm = TRUE),
              Mean_ht.sd = sd(Mean_ht, na.rm = TRUE),
              Mean_ht.mean = mean(Mean_ht, na.rm = TRUE)) %>%
    data.frame() -> Mean_ht

Mean_ht$Mean_ht.cv <- Mean_ht$Mean_ht.sd / Mean_ht$Mean_ht.mean
# mean
#fit simple linear regression model
fit <- lm(Mean_ht.mean ~ res, data = Mean_ht)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data



x11()
plot(Mean_ht$res, Mean_ht$Mean_ht.mean, pch = 16, col='black', cex = 1.75)
# arrows(x0=crr$res, y0 = crr$CRR.mean - crr$CRR.sd, x1 = crr$res, y1 = crr$CRR.mean + crr$CRR.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)

# CV
#fit simple linear regression model
fit <- lm(Mean_ht.cv ~ res, data = Mean_ht)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11()
plot(Mean_ht$res, Mean_ht$Mean_ht.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)






# MEDIAN HEIGHT

samp.data %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(Median_ht.var = var(Median_ht, na.rm = TRUE),
              Median_ht.sd = sd(Median_ht, na.rm = TRUE),
              Median_ht.mean = mean(Median_ht, na.rm = TRUE)) %>%
    data.frame() -> Median_ht

Median_ht$Median_ht.cv <- Median_ht$Median_ht.sd / Median_ht$Median_ht.mean
# mean
#fit simple linear regression model
fit <- lm(Median_ht.mean ~ res, data = Median_ht)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data



x11()
plot(Median_ht$res, Median_ht$Median_ht.mean, pch = 16, col='black', cex = 1.75)
# arrows(x0=crr$res, y0 = crr$CRR.mean - crr$CRR.sd, x1 = crr$res, y1 = crr$CRR.mean + crr$CRR.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)

# CV
#fit simple linear regression model
fit <- lm(Median_ht.cv ~ res, data = Median_ht)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11()
plot(Median_ht$res, Median_ht$Median_ht.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)







############ MOCH

samp.data %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(MOCH.var = var(MOCH, na.rm = TRUE),
              MOCH.sd = sd(MOCH, na.rm = TRUE),
              MOCH.mean = mean(MOCH, na.rm = TRUE)) %>%
    data.frame() -> MOCH

MOCH$MOCH.cv <- MOCH$MOCH.sd / MOCH$MOCH.mean
# mean
#fit simple linear regression model
fit <- lm(MOCH.mean ~ res, data = MOCH)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data



x11()
plot(MOCH$res, MOCH$MOCH.mean, pch = 16, col='black', cex = 1.75)
# arrows(x0=crr$res, y0 = crr$CRR.mean - crr$CRR.sd, x1 = crr$res, y1 = crr$CRR.mean + crr$CRR.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)

# CV
#fit simple linear regression model
fit <- lm(MOCH.cv ~ res, data = MOCH)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11()
plot(MOCH$res, MOCH$MOCH.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)






############ Top_Rug

samp.data %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(Top_Rug.var = var(Top_Rug, na.rm = TRUE),
              Top_Rug.sd = sd(Top_Rug, na.rm = TRUE),
              Top_Rug.mean = mean(Top_Rug, na.rm = TRUE)) %>%
    data.frame() -> Top_Rug

Top_Rug$Top_Rug.cv <- Top_Rug$Top_Rug.sd / Top_Rug$Top_Rug.mean
# mean
#fit simple linear regression model
fit <- lm(Top_Rug.mean ~ res, data = Top_Rug)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data



x11()
plot(Top_Rug$res, Top_Rug$Top_Rug.mean, pch = 16, col='black', cex = 1.75)
# arrows(x0=crr$res, y0 = crr$CRR.mean - crr$CRR.sd, x1 = crr$res, y1 = crr$CRR.mean + crr$CRR.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE)

# CV
#fit simple linear regression model
fit <- lm(Top_Rug.cv ~ res, data = Top_Rug)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11()
plot(Top_Rug$res, Top_Rug$Top_Rug.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)























