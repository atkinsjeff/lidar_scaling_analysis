# 1km lai try number two
# 
# 
library(lidR)
library(viridis)
library(raster)
library(canopyLazR)
library(sf)
# importing data
# wref <- list.files(path = "./data/source_data/", pattern = "wref", ignore.case = TRUE, full.names = TRUE)
# wref
# # a <- readLAS(wref[1])
# b <- readLAS(wref[2])
# c <- readLAS(wref[3])
# d <- readLAS(wref[4])
# e <- readLAS(wref[5])
# f <- readLAS(wref[6])
# g <- readLAS(wref[7])
# h <- readLAS(wref[8])
# i <- readLAS(wref[9])
# j <- readLAS(wref[10])
# # k <- readLAS(wref[11])
# # l <- readLAS(wref[12])
# 
# b <- st_transform(b, crs(c))
# las <- rbind(b, c, d, e, f, g, h, i, j)
# 
# writeLAS(las, "./data/big_wref.las")


    # Convert .laz or .las file into a voxelized lidar array
    laz.data <- laz.to.array("./data/big_wref.las", 
                             voxel.resolution = 1000, 
                             z.resolution = 1,
                             use.classified.returns = TRUE)
    
    las <- readLAS("./data/big_wref.las")
    epsg.las <- epsg(las)
    rm(las)
    
    # Level the voxelized array to mimic a canopy height model
    level.canopy <- canopy.height.levelr(lidar.array = laz.data)
    
    # Estimate LAD for each voxel in leveled array
    lad.estimates <- machorn.lad(leveld.lidar.array = level.canopy, 
                                 voxel.height = 1, 
                                 beer.lambert.constant = 0.5)
    
    # Convert the LAD array into a single raster stack
    lad.raster <- lad.array.to.raster.stack(lad.array = lad.estimates, 
                                            laz.array = laz.data, 
                                            epsg.code = epsg.las)
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

    #file.label = paste("_LAI_", 1000, ".tif", sep = "")
    # Before we save the files, we need to name them
    # make sure to edit the value in there. also, overwrite is set to TRUE
    # so if you don't chnage that, you will write over your data
    # out file name for SGL 316
    #out.file.name <- file.path("./results", paste(substr(basename("big_wref"), 1, nchar(basename("big_wref")) - 4), file.label, sep = ""))
    
    # writes metrics file raster stack to disk
    writeRaster(lai.raster, "./results/wref_lai_1000m.tif", format = "GTiff", overwrite = TRUE)
    
    # # this snippet let's you know how many more .laz files are left to process and what time it is
    # message("how many las files left")
    # print(length(file.names) - i)
    # print(Sys.time())
    

x11()
plot(lai.raster, col = viridis(64), axes = FALSE, legend=FALSE, box=FALSE)
