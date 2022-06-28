## Load the packages of interest
library(gstat)
library(sp)
library(ggplot2)

## Computation of the variogram
var.grsm.moch = variogram(MOCH~1, ## Here, we assume that there is a constant trend in the data.
                    ## It would not have been the case for an elevation study along a hillslope where 
                    ##there would have been a clear elevation trend regarding the spatial coordinates (along the hillslope)
                    data = grsm.metrics)

## Plot the experimental variogram
plot(var.grsm.moch)

## Fit a semi-variogram model
Vario_RDT.fit = fit.variogram(var.grsm.moch,
                              ## A semi-variogram model needs to be manually proposed but it is only to drive the fit
                              ## The parameters will certainly be slightly changed after the fit
                              model = vgm(psill = 2, ## Partial Sill (do not confound with the sill)
                                          model = "Sph",  ## A Spherical model seems appropriate
                                          range = 60,     ## Portée pratique = Maximal distance of autocorrelation
                                          nugget = 0.5))  ## Effet pépite = Small-scale variations

x11()
plot(var.grsm.moch, Vario_RDT.fit)

vario <- var.grsm.moch
vario.fit <- fit.variogram(vario, vgm(psill=max(vario$gamma)*0.9, 
                                      model = "Exp", 
                                      range=max(vario$dist)/2, 
                                      nugget = mean(vario$gamma)/4))


x11()
plot(vario, vario.fit)



#### sort to  10 m
# grsm.1k <- grsm.metrics[c( 1465:1473), ]
grsm.5 <- grsm.metrics[c(1:144), ]
var.grsm.moch = variogram(MOCH~1, ## Here, we assume that there is a constant trend in the data.
                          ## It would not have been the case for an elevation study along a hillslope where 
                          ##there would have been a clear elevation trend regarding the spatial coordinates (along the hillslope)
                          data = grsm.5)

## Plot the experimental variogram
plot(var.grsm.moch)

## Fit a semi-variogram model
Vario_RDT.fit = fit.variogram(var.grsm.moch,
                              ## A semi-variogram model needs to be manually proposed but it is only to drive the fit
                              ## The parameters will certainly be slightly changed after the fit
                              model = vgm(psill = 2, ## Partial Sill (do not confound with the sill)
                                          model = "Sph",  ## A Spherical model seems appropriate
                                          range = 60,     ## Portée pratique = Maximal distance of autocorrelation
                                          nugget = 0.5))  ## Effet pépite = Small-scale variations

vario <- var.grsm.moch
vario.fit <- fit.variogram(vario, vgm(psill=max(vario$gamma)*0.9, 
                                      model =  "Mat", 
                                      range=max(vario$dist)/2, 
                                      nugget = mean(vario$gamma)/4))


x11()
plot(vario, vario.fit)













# plot
x11()
ggplot(as.data.frame(grsm.metrics), aes(x = res, y = MOCH))+
    geom_point()+
    stat_smooth(method = "loess")




### BCP
library(bcp)

grsm.metrics %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(MOCH.var = var(MOCH, na.rm = TRUE),
              MOCH.sd = sd(MOCH, na.rm = TRUE),
              MOCH.mean = mean(MOCH, na.rm = TRUE)) %>%
    data.frame() -> moch
##### univariate sequential data #####
# an easy problem with 2 true change points
bcp.1a <- bcp(moch$res, moch$MOCH.sd)
plot(bcp.1a, main="Univariate Change Point Example")
legacyplot(bcp.1a)


x11()
ggplot(moch, aes(x = res, y = (MOCH.sd / MOCH.mean)))+
    geom_point()+
    stat_smooth(method = "loess")+
    xlab("Spatial Grain [m^2]")+
    ylab("CV [%]")    

x11()
ggplot(moch, aes(x = res, y = MOCH.mean))+
    geom_point()+
    stat_smooth(method = "loess")+
    xlab("Spatial Grain [m^2]")+
    ylab("Mean MOCH [m]")    


#### CC
grsm.metrics %>%
    as.data.frame() %>%
    group_by(siteID, res) %>%
    summarize(CC.var = var(CC, na.rm = TRUE),
              CC.sd = sd(CC, na.rm = TRUE),
              CC.mean = mean(CC, na.rm = TRUE)) %>%
    data.frame() -> cc
##### univariate sequential data #####
# an easy problem with 2 true change points
bcp.1a <- bcp(moch$res, moch$MOCH.sd)
plot(bcp.1a, main="Univariate Change Point Example")
legacyplot(bcp.1a)


x11()
ggplot(cc, aes(x = res, y = (CC.sd / CC.mean)))+
    geom_point()+
    stat_smooth(method = "loess")+
    xlab("Spatial Grain [m^2]")+
    ylab("CV [%] of Canopy Cover")    

x11()
ggplot(cc, aes(x = res, y = CC.mean))+
    geom_point()+
    stat_smooth(method = "loess")+
    xlab("Spatial Grain [m^2]")+
    ylab("Mean CC [%]")    



##############
library(segmented)


# MOCH

#fit simple linear regression model
fit <- lm(MOCH ~ res, data = grsm.metrics)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~x)

#view summary of segmented model
summary(segmented.fit)

#plot original data
plot(grsm.metrics$res, grsm.metrics$MOCH, pch=16, col='steelblue')

#add segmented regression model
plot(segmented.fit, add=T)




# IQR
#fit simple linear regression model
fit <- lm(IQR ~ res, data = grsm.metrics)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~x, psi = 60)

#view summary of segmented model
summary(segmented.fit)

#plot original data
plot(grsm.metrics$res, grsm.metrics$IQR, pch=16, col='steelblue')

#add segmented regression model
plot(segmented.fit, add=T)