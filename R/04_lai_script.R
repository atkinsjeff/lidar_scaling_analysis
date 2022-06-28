# LAI Script
# tall

library(lidR)
library(viridis)
library(raster)
library(canopyLazR)
# importing data
las.files <- list.files(path = "./data/source_data", pattern = "tall", ignore.case = TRUE, full.names = TRUE)



#file.name <- PA4.5[221]
# function taking one file name as argument.
# reads the file and calculates grid metrics then
# saves each raster stack as a .tif file
process_lai <- function(file.name, out_dir, res) {
    print(i)
    print(paste0("Processing: ", file.name))
    
    las <- lidR::readLAS(file.name)
    # iconve
    
    # Convert .laz or .las file into a voxelized lidar array
    laz.data <- laz.to.array(file.name, 
                             voxel.resolution = res, 
                             z.resolution = 1,
                             use.classified.returns = TRUE)
    
    
    
    # Level the voxelized array to mimic a canopy height model
    level.canopy <- canopy.height.levelr(lidar.array = laz.data)
    
    # Estimate LAD for each voxel in leveled array
    lad.estimates <- machorn.lad(leveld.lidar.array = level.canopy, 
                                 voxel.height = 1, 
                                 beer.lambert.constant = 0.5)
    
    # Convert the LAD array into a single raster stack
    lad.raster <- lad.array.to.raster.stack(lad.array = lad.estimates, 
                                            laz.array = laz.data, 
                                            epsg.code = epsg(las))
    # We should remove the bottom 5 meters of LAD data to match everything else before calculating LAI
    lad.raster.5 <- subset(lad.raster, 5:nlayers(lad.raster))
    
    # Create a single LAI raster from the LAD raster stack
    lai.raster <- raster::calc(lad.raster.5, fun = sum, na.rm = TRUE)
    

    # calculate the chm with the pitfree algorithm
    # Local maximum algorithm with a resolution of 1 meter replacing each
    # point by a 10 cm radius circle of 8 points and interpolating the empty
    # pixels using the 3-nearest neighbours and an inverse-distance weighting.
    #chm <- lidR::grid_canopy(las.norm, 1, algorithm = pitfree())
    
    
    #metrics = lasmetrics(lidar, myMetrics(Z, Intensity))
    
    # # remove big ol' files to clear memory
    rm(las)

    file.label = paste("_LAI_", res, ".tif", sep = "")
    # Before we save the files, we need to name them
    # make sure to edit the value in there. also, overwrite is set to TRUE
    # so if you don't chnage that, you will write over your data
    # out file name for SGL 316
    out.file.name <- file.path(out_dir, paste(substr(basename(file.name), 1, nchar(basename(file.name)) - 4), file.label, sep = ""))
    
    # writes metrics file raster stack to disk
    writeRaster(lai.raster, out.file.name, format = "GTiff", overwrite = TRUE)
    
    # # this snippet let's you know how many more .laz files are left to process and what time it is
    # message("how many las files left")
    # print(length(file.names) - i)
    # print(Sys.time())
    
}

#nested for loop
res.list = c(5, 10, 25, 30, 50, 60, 90, 100, 150, 250, 500)
# for loop
for (i in 1:length(las.files)) {
    for (j in seq_along(res.list))
        process_lai(las.files[i], "./results/tall", res.list[j])
}



######################### MOSAICKING SCRIPT
# 5 meters
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall/", pattern = "LAI_5.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')
### SCALING
nFactor <- 16 * length(lai.files)
nFactor500 <- 4 * length(lai.files)
nFactor1000 <- length(lai.files)
# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box = FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
df.lai <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
df.lai$res = 5
df.lai$siteID = "tall"






# 10 meters
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall", pattern = "LAI_10.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
xx$res = 10
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)






# 25 meters
lai.files <- list.files(path = "./results/tall", pattern = "LAI_25.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
xx$res = 25
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)







# 30 meters
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall", pattern = "LAI_30.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
xx$res = 30
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)








# 50 meters
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall", pattern = "LAI_50.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
xx$res = 50
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)






##################


# 60 meters
lai.files <- list.files(path = "./results/tall", pattern = "LAI_60.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
xx$res = 60
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)







# 90 meters
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall", pattern = "LAI_90.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
xx$res = 90
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)






# 100 meters
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall", pattern = "LAI_100.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
xx$res = 100
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)








# 150 meters
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall", pattern = "LAI_150.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
xx$res = 150
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)





# 250 meters
#### creatiing mosaics
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall", pattern = "LAI_250.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor, sp = T, xy = TRUE)
xx$res = 250
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)



# 500 meters
#### creatiing mosaics
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall", pattern = "LAI_500.tif", ignore.case = TRUE, full.names = TRUE)

#names(lai)[1:2] <- c('x', 'y')

# make raster list
rast.list <- list()
for(i in 1:length(lai.files)) { rast.list[i] <- raster(lai.files[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- mean
rast.list$na.rm <- TRUE
#rast.list$tol <- 0.1
rast.mosaic <- do.call(mosaic, rast.list)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
### RENAME layers
# # this renames them



# create a regular saple of points
# 
xx <- sampleRegular(rast.mosaic, size = nFactor500, sp = T, xy = TRUE)
xx$res = 500
xx$siteID = "tall"

df.lai <- rbind(df.lai, xx)






# 1000 meters
#### creatiing mosaics
lai.files <- list.files(path = "./results/tall", pattern = "LAI_1000m.tif", ignore.case = TRUE, full.names = TRUE)
# 
# names(file.list)[1:2] <- c('x', 'y')

# make raster list
#rast.list$tol <- 0.1
rast.mosaic <- raster::raster(lai.files)
names(rast.mosaic) <- "LAI"
x11()
plot(rast.mosaic, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)


xx <- sampleRegular(rast.mosaic, size = nFactor1000, sp = T, xy = TRUE)
xx$res <-  1000
xx$siteID <- "tall"

df.lai <- rbind(df.lai, xx)


#####
write.csv(df.lai, "tall_processed_lai.csv")


