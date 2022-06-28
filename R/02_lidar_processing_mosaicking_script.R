# BART

library(lidR)
library(viridis)
library(raster)
# importing data
las.files <- list.files(path = "./data/", pattern = "deju", ignore.case = TRUE, full.names = TRUE)

# plot data
# gis <- read.csv("./data/neon_plot_data.csv")
# 
# ### plot centers in 
# gis %>%
#     select(siteID, plotID, plotType, easting, northing) %>%
#     filter(siteID == "BART" & plotType == "tower") %>%
#     data.frame() -> grsm.pts



# z = Z, height, rn = ReturnNumber, i = Intensity, g = gpstime, c = Classification
myMetrics <- function(z, rn, i, a, g, c){
    first  = rn == 1L
    zfirst = z[first]
    nfirst = length(zfirst)
    
    ground = c == 2
    zground = z[ground]
    nground = length(zground)
    
    firstground = rn == 1L & c == 2
    zfirstground = z[firstground]
    nfirstground = length(zfirstground)
    nz = length(z)
    
    metrics = list(
        # MeanAngle = mean(a),
        # MedAngle = median(a),
        # MaxAngle = max(a),
        # NoPoints = length(z),
        # NoGround = nground,
        # NoFirst = nfirst,
        # NoFirstGround =  nfirstground,
        iqr =  IQR(z), # inter-quartile range
        vci = VCI(z, zmax = max(z)), # vertical complexity index
        entropy = entropy(z, zmax = max(z)),
        fhd =   (entropy(z, zmax = max(z)) * log(max(z))),  #foliar height diversity
        vdr = ((max(z) - median(z)) / max(z)),
        top_rug = sd(zfirst),
        meanht = mean(z),
        medht = median(z),
        moch = mean(zfirst),
        # skew = (3 * (mean(z) - median(z)) / sd(z)), # Skew = 3 * (Mean – Median) / Standard Deviation
        # firstbelow1 = (sum(zfirst < 1) / nfirst) * 100,
        # firstbelow2 = (sum(zfirst < 2) / nfirst) * 100,
        # firstbelow5 = (sum(zfirst < 5) / nfirst) * 100,
        # total returns
        # below1 = (sum(z < 1) / nz) * 100,
        # below2 = (sum(z < 2) / nz) * 100,
        # below5 = (sum(z < 5) / nz) * 100,
        cc = 100 - ((sum(z < 2) / nz) * 100),
        crr = (mean(z) - min(z)) / (max(z) - min(z)),
        p5 = quantile(z, probs = 0.05),
        p10 = quantile(z, probs = 0.1),
        p25 = quantile(z, probs = 0.25),
        p50 = quantile(z, probs = 0.5),
        p75 = quantile(z, probs = 0.75),
        p90 = quantile(z, probs = 0.9),
        p95 = quantile(z, probs = 0.95),
        p99 = quantile(z, probs = 0.99))
    
    # return all them boys
    return(metrics)
}

#file.name <- PA4.5[221]
# function taking one file name as argument.
# reads the file and calculates grid metrics then
# saves each raster stack as a .tif file
process_lidar_file <- function(file.name, out_dir, res) {
    
    las <- lidR::readLAS(file.name)
    # imports files and then cleans duplicates
    las <- lidR::filter_duplicates(las)
    
    # normalize heights
    las.norm <- lidR::normalize_height(las, algorithm = tin(), na.rm = TRUE, method = "bilinear")
    
    las2 <- filter_poi(las.norm, Z >= 0 & Z < 75)
    # calculate the chm with the pitfree algorithm
    # Local maximum algorithm with a resolution of 1 meter replacing each
    # point by a 10 cm radius circle of 8 points and interpolating the empty
    # pixels using the 3-nearest neighbours and an inverse-distance weighting.
    #chm <- lidR::grid_canopy(las.norm, 1, algorithm = pitfree())
    
    
    #metrics = lasmetrics(lidar, myMetrics(Z, Intensity))
    
    # # remove big ol' files to clear memory
    rm(las)
    xstart = -(res/2)
    ystart = -(res/2)
    # custom metrics function, note input variables and resolution, which is set at 10 as in 10 meters
    metrics <- lidR::grid_metrics(las.norm, ~myMetrics(z = Z, rn=ReturnNumber, i = Intensity, g = gpstime, c = Classification), 
                                  start = c(xstart, ystart), res = res, filter = ~Z >= 0 & Z <= 50 )    
    
    # # this renames them
    # stack.names = c("MeanAngle", "MedAngle", "MaxAngle", "NoPoints", "IQR", "VCI", "Entropy", "FHD", "VDR", "TopRugosity", "MeanHeight", "MedianHeight", 
    #                 "MOCH", "Skewness", "FirstBelow1m", "FirstBelow2m", "FirstBelow5m", 
    #                 "Below1m", "Below2m", "Below5m", "CC", "CRR", "p10", "p25", "p50", "p75", "p90", "p95", "p99")
    # 
    # 
    # names(metrics) <- stack.names
    
    
    file.label = paste("_", res, ".tif", sep = "")
    # Before we save the files, we need to name them
    # make sure to edit the value in there. also, overwrite is set to TRUE
    # so if you don't chnage that, you will write over your data
    # out file name for SGL 316
    out.file.name <- file.path(out_dir, paste(substr(basename(file.name), 1, nchar(basename(file.name)) - 4), file.label, sep = ""))
    
    # writes metrics file raster stack to disk
    writeRaster(metrics, out.file.name, format = "GTiff", overwrite = TRUE)
    
    # # this snippet let's you know how many more .laz files are left to process and what time it is
    # message("how many las files left")
    # print(length(file.names) - i)
    # print(Sys.time())
    
}

#nested for loop
res.list = c(5, 10, 25, 30, 50, 60, 90, 100, 150, 250, 500, 1000)
# for loop
for (i in 1:length(grsm)) {
    for (j in seq_along(res.list))
    process_lidar_file(las.files[i], "./results/", res.list[j])
}

# 
# for (i in 1:length(grsm)) {
#         process_lidar_file(grsm[i], "./results/", 5)
# }
# 
# for (i in 1:length(grsm)) {
#     process_lidar_file(grsm[i], "./results/", 500)
# }
# 
# for (i in 1:length(grsm)) {
#     process_lidar_file(grsm[i], "./results/", 1000)
# }
# 
# for (j in seq_along(res.list)){
#     res = res.list[j]
#         for (i in 1:length(grsm)){
#         process_lidar_file(grsm[i], "./results/", res)
#         }
# }


# 
# x11()
# plot(chm, col = viridis(32))
# 
# # 
# i = 15
# xleft = grsm.pts$northing[i] - 20
# ybottom =  grsm.pts$easting[i] - 20
# xright =  grsm.pts$northing[i] + 20
# ytop =  grsm.pts$easting[i] + 20
# 
# plot <- clip_rectangle(las.norm, xleft, ybottom, xright, ytop)
# 
# 

# 5 meters
#### creatiing mosaics
grsm.10.list <- list.files(path = "./results/", pattern = "_5.tif", ignore.case = TRUE, full.names = TRUE)

names(grsm.10.list)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(grsm.10.list)) { rast.list[i] <- stack(grsm.10.list[i]) }

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
grsm.10 <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
grsm.10$res = 5
grsm.10$siteID = "BART"

x11()
plot(grsm.10)
#### 
grsm.metrics <- grsm.10[, c(23, 1:2, 22, 3:21)]






# 10 meters
#### creatiing mosaics
grsm.10.list <- list.files(path = "./results/", pattern = "_10.tif", ignore.case = TRUE, full.names = TRUE)

names(grsm.10)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(grsm.10)) { rast.list[i] <- stack(grsm.10[i]) }

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
grsm.10 <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
grsm.10$res = 10
grsm.10$siteID = "BART"

#### 
xx <- grsm.10[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)






# 25 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_25.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
x$res <-  25
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)







# 30 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_30.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
x$res <-  30
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)







# 50 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_50.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
x$res <-  50
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)






##################


# 60 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_60.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
x$res <-  60
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)








# 90 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_90.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
x$res <-  90
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)








# 100 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_100.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
x$res <-  100
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)







# 150 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_150.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
x$res <-  150
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)







# 250 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_250.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 144, sp = T, xy = TRUE)
x$res <-  250
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)

plot(rast.mosaic$MOCH)
plot(x, add = TRUE)



# 500 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_500.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 36, sp = T, xy = TRUE)
x$res <-  500
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)

plot(rast.mosaic$MOCH)
plot(x, add = TRUE)






# 1000 meters
#### creatiing mosaics
file.list <- list.files(path = "./results/", pattern = "_1000.tif", ignore.case = TRUE, full.names = TRUE)

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
x <- sampleRegular(rast.mosaic, size = 9, sp = T, xy = TRUE)
x$res <-  1000
x$siteID <- "BART"

xx <- x[, c(23, 1:2, 22, 3:21)]

grsm.metrics <- rbind(grsm.metrics, xx)

plot(rast.mosaic$MOCH)
plot(x, add = TRUE)


#####
write.csv(grsm.metrics, "bart_processed_metrics.csv")


