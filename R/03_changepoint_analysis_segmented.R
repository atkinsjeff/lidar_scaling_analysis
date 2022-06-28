##### 
# variables to estimate:   LAI, FHD, Top Rugosity/Rumple, Mean & Median Height, MOCH, Canopy Cover and Canopy Relief Ratio
# 
# 
# 
# 
library(segmented)
library(tidyverse)





## Modified version of your function that does not rely on accessing
## variables that is external to its environment.


# CANOPY COVER

# mean
#fit simple linear regression model
fit <- lm(CC.mean ~ res, data = cc)
selgmented(fit)

davies.test(fit)
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
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)

#view summary of segmented model
summary(segmented.fit)

#plot original data
x11()
plot(cc$res, cc$CC.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)





# CANOPY RELIEF RATIO

grsm.metrics %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(CRR.var = var(CRR, na.rm = TRUE),
              CRR.sd = sd(CRR, na.rm = TRUE),
              CRR.mean = mean(CRR, na.rm = TRUE)) %>%
    data.frame() -> crr

# mean
#fit simple linear regression model
fit <- lm(CRR.mean ~ res, data = crr)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
pscore.test(fit, ~res)
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
pscore.test(segmented.fit, ~res)
davies.test(segmented.fit, ~res)

confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
x11()
plot(crr$res, crr$CRR.mean, pch = 16, col='black', cex = 1.75, 
     ylim = c(0.3, 0.8),
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
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11()
plot(cc$res, cc$CC.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)







# FHD

grsm.metrics %>%
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
fit <- lm(FHD ~ res, data = grsm.metrics)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
pscore.test(fit, ~res, more.break = TRUE)
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
summary(segmented.fit)

davies.test(segmented.fit, ~res)

confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
x11(width = 8, height = 5)
plot(fhd$res, fhd$FHD.mean, pch = 16, col='black', cex = 1.75, 
     ylim = c(1.5, 4),
     xlab = expression(paste("Spatial Resolution (m"^"2"*")")), ylab = expression(paste("Foliar Height Diversity(FHD"[Mean]*")")))
arrows(x0=fhd$res, y0 = fhd$FHD.mean - fhd$FHD.sd, x1 = fhd$res, y1 = fhd$FHD.mean + fhd$FHD.sd, 
       code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)

x11(width = 8, height = 5)
plot(grsm.metrics$res, grsm.metrics$FHD,  col='black', cex = 1, 
     ylim = c(1.5, 4),
     xlab = expression(paste("Spatial Resolution (m"^"2"*")")), ylab = expression(paste("Foliar Height Diversity(FHD"[Mean]*")")))
# arrows(x0=grsm.metrics$res, y0 = grsm.metrics$FHD.mean - grsm.metrics$FHD.sd, x1 = grsm.metrics$res, y1 = grsm.metrics$FHD.mean + grsm.metrics$FHD.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)





# cv
#fit simple linear regression model
fit <- lm(FHD.cv ~ res, data = fhd)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "aic")
pscore.test(fit, ~res, more.break = TRUE)
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit,~res, npsi = 2)
summary(segmented.fit)


confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
x11(width = 8, height = 5)
plot(fhd$res, fhd$FHD.cv, pch = 16, col='black', cex = 1.75, 
     ylim = c(0, 0.2),
     xlab = expression(paste("Spatial Resolution (m"^"2"*")")), ylab = expression(paste("Foliar Height Diversity(FHD"[CV]*")")))
# arrows(x0=fhd$res, y0 = fhd$FHD.mean - fhd$FHD.sd, x1 = fhd$res, y1 = fhd$FHD.mean + fhd$FHD.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)


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

grsm.metrics %>%
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
selgmented(fit, type = "score")
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

grsm.metrics %>%
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
selgmented(fit, type = "davies")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
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
selgmented(fit, type = "davies")
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

grsm.metrics %>%
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
selgmented(fit, type = "davies")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
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
selgmented(fit, type = "davies")
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

grsm.metrics %>%
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
selgmented(fit, type = "davies")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data



x11()
plot(Top_Rug$res, Top_Rug$Top_Rug.mean, pch = 16, col='black', cex = 1.75)
# arrows(x0=crr$res, y0 = crr$CRR.mean - crr$CRR.sd, x1 = crr$res, y1 = crr$CRR.mean + crr$CRR.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)

# CV
#fit simple linear regression model
fit <- lm(Top_Rug.cv ~ res, data = Top_Rug)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
#fit piecewise regression model to original model, estimating a breakpoint at x=9
selgmented(fit, type = "davies")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11()
plot(Top_Rug$res, Top_Rug$Top_Rug.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)


























# MEAN HEIGHT

grsm.metrics %>%
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
selgmented(fit, type = "davies")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
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
selgmented(fit, type = "davies")
davies.test(fit, ~res)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 2)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11()
plot(Median_ht$res, Median_ht$Median_ht.cv, pch=16, col='steelblue')
plot(segmented.fit, add=T)









##### BCP
bcp.cc <- bcp(cc$CC.cv, p0 = 0.1, burnin = 1000, 
    mcmc = 10000)

x11()
plot(bcp.cc, main = "Canopy Cover GRSM")


prob.cc = cbind(cc$res, bcp.cc$posterior.prob)
plot(prob.cc, type = "l", xlab = "Spatial Resolution [m^2]", ylab = "Posterior Probability CC_cv")

bcp.cc.mean <- bcp(cc$CC.mean, p0 = 0.1, burnin = 1000, 
              mcmc = 10000)
legacyplot(bcp.cc.mean)
expression(paste("4"^"th")))
prob.cc.mean = cbind(cc$res, bcp.cc.mean$posterior.prob)
x11()
plot(prob.cc.mean, type = "l", xlab = expression(paste("Spatial Resolution (m"^"2"*")")), 
     ylab = expression(paste("Posterior Probability (CC"[Mean]*")")),
     xlim = c(0, 500))

##### changepoint
    library(changepoint)


cpt.mean(data, penalty="MBIC",pen.value=0,method="AMOC",Q=5,test.stat="Normal",class=TRUE,
         param.estimates=TRUE,minseglen=1)