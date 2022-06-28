# 05 LAI changepoint analysis
library(segmented)
library(tidyverse)

# set random seed
set.seed(666)

# bring in data
df <- read.csv("tall_processed_lai.csv")
df$siteID <- "TALL"
table(df$res)
# sort data to sampling structure. 
df %>%
    dplyr::filter(CC >= 25 & MOCH >= 5) %>%
    data.frame() -> df
table(df$res)

# set up random sample
df %>% 
    filter(res <= 250) %>%
    group_by(res)%>% 
    sample_n(100) -> df.250

df %>%
    filter(res >= 500) %>%
    data.frame() -> df.500

samp.data <- rbind(df.250, df.500)


#### LAI

df %>%
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
segmented.fit <- segmented(fit, seg.Z = ~res, npsi = 1)
confint(segmented.fit,level=0.95, method=c("delta", "score", "gradient"),
        rev.sgn=, var.diff=FALSE, is=FALSE, digits=4)#plot original data
#plot original data
x11(width = 8, height = 5)
plot(LAI$res, LAI$LAI.cv, pch = 16, col='black', cex = 1.75, 
     #ylim = c(0.1, 0.9),
     xlab = expression(paste("Spatial Resolution (m"^"2"*")")), ylab = expression(paste("Leaf Area Index (LAI"[CV]*")")))
# arrows(x0=LAI$res, y0 = LAI$LAI.cv - LAI$LAI.sd, x1 = LAI$res, y1 = LAI$LAI.cv + LAI$LAI.sd, 
#        code = 3, angle = 90, length = 0.1, col="black", lwd = 0.1)
plot(segmented.fit, col = "blue", add = TRUE, rug = TRUE, dens.rug=TRUE, dens.col = grey(0.8), lwd = 2)
