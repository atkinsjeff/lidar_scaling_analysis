# variogram approach
# a spatial variogram approach using the log values of the standard deviation of each metric
# against the grain size or spatial resolution of the measurement.

library(tidyverse)
library(ggplot2)
library(gstat)
library(sf)
library(rgdal)
library(spdep)
# data
big.df <- read.csv("wref_processed_metrics.csv")

big.df %>%
    group_by(siteID, res) %>%
    mutate_at(.funs = list(sd = ~sd(.)), .vars = c("CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug")) %>%
    mutate_at(.funs = list(log10 = ~log10(.)), .vars = c("CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug")) %>%
    data.frame() -> df

# plot
x11()
ggplot(df, aes(x = res, y = log10(CC_sd)))+
    geom_point()


x11()
ggplot(df, aes(x = res, y = log10(MOCH_sd)))+
    geom_point()
# sort to 25 m
big.df %>%
    filter(res == 500) %>%
    group_by(siteID, res) %>%
    mutate_at(.funs = list(sd = ~sd(.)), .vars = c("CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug")) %>%
    mutate_at(.funs = list(log10 = ~log10(.)), .vars = c("CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug")) %>%
    data.frame() -> df.500

# make a spatial points data frame
coordinates(df.250) <- ~x+y

# variogram at 5
v = variogram(MOCH~ 1, data = df.250)

x11()
plot(v)


v.fit = fit.variogram(v,  vgm(psill= 25, model="Exp", nugget= 200, range=500))
v.fit
x11()
plot(v, v.fit)

# # make big data
# big.df %>%
#     group_by(siteID, res) %>%
#     mutate_at(.funs = list(sd = ~sd(.)), .vars = c("CC", "CRR", "FHD", "Mean_ht", "Median_ht", "MOCH", "Top_Rug")) %>%
#     data.frame() -> df
#     
# 
# x11()
# ggplot(df, aes(x = res, y = log10(CC_sd)))+
#     geom_point()
# 
# x11()
# ggplot(df, aes(x = res, y = log10(CC)))+
#     geom_point()
# 

# create a bubble plot with the random values



v = variogram(log(zinc)~1, )
v.fit = fit.variogram(v, vgm(1, "Sph", 900, 1))
v.fit

#### MORAN'S I
x <- as.matrix(dist(cbind(df.500$x, df.500$y)))
x.inv <- 1/x
diag(x.inv) <- 0
#x.inv

Moran.I(df.500$CC, x.inv, scaled = FALSE, na.rm = TRUE)


#### MORAN'S I
x <- as.matrix(dist(cbind(df.250$x, df.250$y)))
x.inv <- 1/x
diag(x.inv) <- 0
#x.inv

Moran.I(df.250$CC, x.inv)

#############
## Load the packages of interest
library(gstat)
library(sp)
library(ggplot2)
bart <- grsm.metrics

bart$logCC <- log10(bart$CC)
## Computation of the variogram
var.bart.cc = variogram(bart$CC)
, ## Here, we assume that there is a constant trend in the data.
                          ## It would not have been the case for an elevation study along a hillslope where 
                          ##there would have been a clear elevation trend regarding the spatial coordinates (along the hillslope)
                          data = bart)

TheVariogramModel <- vgm(psill=0.15, model="Gau", nugget=0.0001, range=5)
plot(var.bart.cc, model=TheVariogramModel) 

FittedModel <- fit.variogram(var.bart.cc, model=TheVariogramModel)    
plot(TheVariogram, model=FittedModel)

## Plot the experimental variogram
x11()
plot(var.bart.cc)

## Fit a semi-variogram model
vario <- var.bart.cc
vario.fit <- fit.variogram(vario, vgm(psill=max(vario$gamma)*0.9, 
                                      model = "Exp", 
                                      range=max(vario$dist)/2, 
                                      nugget = mean(vario$gamma)/4))


x11()
plot(vario, vario.fit)

