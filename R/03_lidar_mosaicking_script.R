# lidar mosaicking script
library(raster)
library(tidyverse)
# 5 meters
#### creatiing mosaics
grsm.10.list <- list.files(path = "./results/grsm/", pattern = c("d_5.tif"), ignore.case = TRUE, full.names = TRUE)
#names(grsm.10.list)[1:2] <- c('x', 'y')

nFactor <- 16 * length(grsm.10.list)
nFactor500 <- 4 * length(grsm.10.list)
nFactor1000 <- length(grsm.10.list)
# make raster list
rast.list <- list()
for(i in 1:length(grsm.10.list)) { rast.list[i] <- stack(grsm.10.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
grsm.10 <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
grsm.10$res = 5
grsm.10$siteID = "grsm"

x11()
plot(rast.mosaic$CC)
plot(grsm.10, add = TRUE)
#### 
grsm.metrics <- grsm.10[, c(23, 1:2, 22, 3:21)]






# 10 meters
#### creatiing mosaics
#### creatiing mosaics
grsm.10.list <- list.files(path = "./results/grsm", pattern = c("d_10.tif"), ignore.case = TRUE, full.names = TRUE)
#names(grsm.10.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(grsm.10.list)) { rast.list[i] <- stack(grsm.10.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
grsm.10 <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
grsm.10$res = 10
grsm.10$siteID = "grsm"
x11()
plot(rast.mosaic$CC)
plot(grsm.10, add = TRUE)

#### 
xx <- grsm.10[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)






# 25 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_25.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
x$res <-  25
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)







# 30 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_30.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
x$res <-  30
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)







# 50 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_50.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
x$res <-  50
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)






##################


# 60 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_60.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
x$res <-  60
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)








# 90 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_90.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
x$res <-  90
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)








# 100 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_100.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
x$res <-  100
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)







# 150 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_150.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
x$res <-  150
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)







# 250 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_250.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
x$res <-  250
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)

plot(rast.mosaic$MOCH)
plot(x, add = TRUE)



# 500 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_500.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor500, sp = T, xy = TRUE)
x$res <-  500
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)

plot(rast.mosaic$MOCH)
plot(x, add = TRUE)






# 1000 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/grsm/", pattern = "d_1000.tif", ignore.case = TRUE, full.names = TRUE)

names(file.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(file.list)) { rast.list[i] <- stack(file.list[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)

### RENAME layers
# # this renames them
stack.names = c("IQR", "VCI", "Entropy", "FHD", "VDR", "Top_Rug", "Mean_ht", "Median_ht"," MOCH",  
                "CC", "CRR", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")

names(rast.mosaic) <- stack.names
# create a regular saple of points
# 
x <- sampleRegular(rast.mosaic, size = nFactor1000, sp = T, xy = TRUE)
x$res <-  1000
x$siteID <- "grsm"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)

plot(rast.mosaic$MOCH)
plot(x, add = TRUE)


#####
write.csv(grsm.metrics, "grsm_processed_metrics.csv")


