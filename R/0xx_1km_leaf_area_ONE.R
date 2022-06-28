#' Convert a .laz or .las file to an array
#'
#' This function reads in a list of .laz or .las files from a given file path and converts
#' each one into a voxelized array based on the x,y,z resolutions you specify.
#'
#' @param laz.file.path Path to the .laz or .las file
#' @param voxel.resolution The spatial resolution (x,y) you want the output voxel to be - this is a single
#' number where both sides of the cell will be the same
#' @param z.resolution the vertical resolution of the voxel
#' @param use.classified.returns TRUE or FALSE statement to determine if the user wants to use preclassified ground returns or the lowest point for "ground"
#' @return A list of voxelized arrays
#' @export

las.1km <- list.files(path = "./data/", pattern = "big", ignore.case = TRUE, full.names = TRUE)


#laz.to.array <- function(laz.file.path, voxel.resolution, z.resolution, use.classified.returns) {
    
    voxel.resolution = 1000
    z.resolution = 1
    use.classified.returns = TRUE
    
    #read in laz files
    laz.data <- rlas::read.las(las.1km[2])
    las <- readLAS(las.1km[2])
    epsg.las <- epsg(las)
    rm(las)
    #convert into a x,y,z, class table for easy reading and analysis
    laz.xyz.table <- as.data.frame(cbind(x=laz.data$X, y=laz.data$Y,
                                         z=laz.data$Z, class=laz.data$Classification))
    
    #memory stuff
    gc()
    laz.data
    gc()
    
    #lets remove the points that are deemed outliers - this will remove the noise from the dataset. We can use John Tukey's method here for outlier removal
    #where we can use 1.5 IQR since we are just working on preprocessing the data. More extreme data removal can happen later on and should be left up to
    #the individual user
    
    #lets get a summary of the stats for the z column (height)
    stat.q <- summary(laz.xyz.table[,3])
    
    #lets get the lower range for removal
    t.lower <- stat.q[2] - 1.5*(stat.q[5] - stat.q[2])
    t.upper <- stat.q[5] + 1.5*(stat.q[5] - stat.q[2])
    
    #lets remove these outliers now
    laz.xyz <- laz.xyz.table[(laz.xyz.table[,3] >= t.lower) & (laz.xyz.table[,3] <= t.upper),]
    
    # memory stuff
    gc()
    rm(laz.xyz.table)
    rm(stat.q)
    rm(t.lower)
    rm(t.upper)
    gc()
    
    #lets create some boundary information for the x,y,z columns
    x.range.raw <- range(laz.xyz[,1], na.rm=T)
    y.range.raw <- range(laz.xyz[,2], na.rm=T)
    z.range.raw <- range(laz.xyz[,3], na.rm=T)
    
    #lets set how big we want each pixel to be
    x.y.grain <- voxel.resolution
    z.grain <- z.resolution
    
    #this is kind of repetitive, but makes sure that there are no rounding issues, thus any number
    #with a decimal is rounded down (floor) or up (ceiling), this helps elimate edge weirdness and cell size issues
    x.range <- c(floor(x.range.raw[1] / x.y.grain) * x.y.grain, ceiling(x.range.raw[2] / x.y.grain) * x.y.grain)
    y.range <- c(floor(y.range.raw[1] / x.y.grain) * x.y.grain, ceiling(y.range.raw[2] / x.y.grain) * x.y.grain)
    z.range <- c(floor(z.range.raw[1] / z.grain) * z.grain, ceiling(z.range.raw[2] / z.grain) * z.grain)
    
    #lets create the bins that will be used as cells later on
    x.bin <- seq(x.range[1], x.range[2], x.y.grain)
    y.bin <- seq(y.range[1], y.range[2], x.y.grain)
    z.bin <- seq(z.range[1], z.range[2], z.grain)
    
    #find the index number for each lidar pulse so that that pulse can be placed in the appropriate bin
    #rounding to the middle point of the pixel, this assigns a value to each lidar point
    x.cuts <- round((laz.xyz[,1] - x.bin[1] + x.y.grain / 2) / x.y.grain)
    y.cuts <- round((laz.xyz[,2] - y.bin[1] + x.y.grain / 2) / x.y.grain)
    z.cuts <- round((laz.xyz[,3] - z.bin[1] + z.grain / 2) / z.grain)
    
    #here we turn the y value index numbers into a decimal place and then add that to the x value,
    #this will give us a number of unique values equal to the number of pixels in the empty raster
    #we can then order these, and only have to do indexing one time, rather than twice (x and y),
    #thus improving performance time and computing efficiency
    #integer is x index, float is y index
    y.cuts.dec <- y.cuts / (length(y.bin))
    x.y.cuts <- x.cuts + y.cuts.dec
    
    # memory stuff
    gc()
    rm(x.cuts)
    rm(y.cuts)
    gc()
    
    #lets create a data frame that has two columns, one with the x index values and another
    #with the y index values in decimal form
    x.y.levels <- as.data.frame(expand.grid(1:(length(x.bin) - 1), (1:(length(y.bin) - 1)) / length(y.bin)))
    colnames(x.y.levels) <- c("x.level", "y.level")
    
    #lets reorder the x.y.levels data frame so that they are in numerical order
    #first order = x lev, second order = y level
    x.y.levels <- x.y.levels[order(x.y.levels[,"x.level"], x.y.levels[,"y.level"]),]
    
    #now that the variables are ordered, lets break the data frame apart and store the
    #data as a string of values
    x.y.levels.char <- as.character(x.y.levels[,"x.level"] + x.y.levels[,"y.level"])
    
    #assign pixel values (index values) to the 250,000 pixels in the newly formed raster
    x.y.cuts.factor <- factor(x.y.cuts, levels = x.y.levels.char)
    
    #lets create an empty matrix to store all this in, fill it with NA values for now so that it has shape
    xyz.matrix <- as.data.frame(matrix(NA, nrow = length(x.y.levels.char), ncol = 4, dimnames = list(NULL, c("x","y","z", "class"))))
    
    #lets bind this empty matrix with the lidar data frame
    xyz.table <- rbind(laz.xyz, xyz.matrix)
    
    #memory stuff
    gc()
    rm(laz.xyz)
    rm(xyz.matrix)
    gc()
    
    #this determines the index number the z value of all lidar pulses,
    #so that each lidar pulse is assigned a vertical voxel to live in
    z.index <- c(z.cuts, rep(NA, length(x.y.levels.char)))
    
    #this determines the index number for the x,y cell, so that each lidar pulse is assigned a raster cell to live in
    x.y.index <- c(x.y.cuts.factor, as.factor(x.y.levels.char))
    
    #lets combine the lidar table, the x,y index values, and the z index values into a data frame
    lidar.table <- data.frame(xyz.table, x.y.index = x.y.index, z.index = z.index)
    
    #memory stuff
    gc()
    rm(xyz.table)
    gc()
    
    #lets create a function that populates the empty array that we will create later
    #this function will (for each factor in the data frame) seperate the pre-classified ground points
    #from the non-ground points, store the lowest ground value in the first row of the array, store the
    #highest non-ground point in the second row, and the number of lidar pulses in each z voxel being stored
    #in each subsequent row with the lowest voxel being the 3rd row and the highest voxel being the last row
    lidar.array.populator.classified <- function(x){
        ground.pts <- x[x[,4] == 2,]
        as.numeric(c(
            quantile(ground.pts[,3], prob = 0, na.rm = TRUE),
            quantile(x[,3], prob = 1, na.rm = TRUE),
            (table(c(x[,6], 1:length(z.bin) - 1)) - 1)
        ))
    }
    
    #lets make another function, but this one will not use the classified point cloud - instead it will take the lowest point in the
    #column of voxels and assign that to the "ground" - this way the user can use their own classification scheme to determine what points represent
    #the ground, rather than relying on a pre-determined classification
    lidar.array.populator.lowest <- function(x){
        ground.pts <- x[x[,4] == 2,]
        as.numeric(c(
            quantile(x[,3], prob = 0, na.rm = TRUE),
            quantile(x[,3], prob = 1, na.rm = TRUE),
            (table(c(x[,6], 1:length(z.bin) - 1)) - 1)
        ))
    }
    
    #lets look at if the user wants to use the classified points or take the lowest point in each column - then we can construct our
    #final LiDAR arrays
    if (use.classified.returns == TRUE) {
        
        print("Using classified LiDAR returns!")
        #lets see how many rows the empty array will have, this will be equal to the number of z voxels
        #this will also test to make sure that there are no initial errors when applying the above function
        #to the whole data set
        z.vox.test <- length(dlply(lidar.table[1:2,], "x.y.index", lidar.array.populator.classified)[[1]])
        
        #generate the final array using the lidar table, the x.y.index values as the factor, and the lidar.array.populator.classified
        #as the function.  set the dimensions equal to the number of z voxels, the length of y.bin and the length of x.bin.
        #we need to subtract 1 from each of these values, because there is a 0 y.bin and x.bin, due to data creation
        #and edge effect mitigation -- there is no data stored here, but makes counting and rounding easier
        lidar.array <- array(as.vector(unlist(dlply(lidar.table, "x.y.index", lidar.array.populator.classified, .progress = "text"))),
                             dim = c(z.vox.test, (length(y.bin) - 1), (length(x.bin) - 1)))
        
        #lets return the array and the x.bin and y.bin so that we can have spatial information for later
        return.data <- base::list("array" = lidar.array, "x.bin" = x.bin, "y.bin" = y.bin, "z.bin" = z.bin)
    }
    
    if (use.classified.returns == FALSE) {
        
        print("Using unclassified LiDAR returns!")
        #lets see how many rows the empty array will have, this will be equal to the number of z voxels
        #this will also test to make sure that there are no initial errors when applying the above function
        #to the whole data set
        z.vox.test <- length(dlply(lidar.table[1:2,], "x.y.index", lidar.array.populator.lowest)[[1]])
        
        #generate the final array using the lidar table, the x.y.index values as the factor, and the lidar.array.populator.classified
        #as the function.  set the dimensions equal to the number of z voxels, the length of y.bin and the length of x.bin.
        #we need to subtract 1 from each of these values, because there is a 0 y.bin and x.bin, due to data creation
        #and edge effect mitigation -- there is no data stored here, but makes counting and rounding easier
        lidar.array <- array(as.vector(unlist(dlply(lidar.table, "x.y.index", lidar.array.populator.lowest, .progress = "text"))),
                             dim = c(z.vox.test, (length(y.bin) - 1), (length(x.bin) - 1)))
        
        #lets return the array and the x.bin and y.bin so that we can have spatial information for later
        return.data <- base::list("array" = lidar.array, "x.bin" = x.bin, "y.bin" = y.bin, "z.bin" = z.bin)
    }
    
    #return the list
   # return(return.data)
    
    #lets remove all the stuff we don't need anymore so that R doesn't have a melt down due to memory usage
    gc()
    remove(lidar.table)
    remove(lidar.array)
    gc()
    



####################################################
# Level the voxelized array to mimic a canopy height model
level.canopy <- canopy.height.levelr(return.data)

leveld.lidar.array <- return.data 
voxel.height = 1
beer.lambert.constant = 0.5

machorn.lad <- function(leveld.lidar.array, voxel.height, beer.lambert.constant = NULL){
    
    #first lets just use the pulse totals in each voxel, we do not need the ground or canopy elevations at this point
    voxel.N.pulse <- leveld.lidar.array$array[3:dim(leveld.lidar.array$array)[1],,]
    
    #lets create an empty array to store the pulse accumulations in
    pulse.accum <- array(0, dim = dim(voxel.N.pulse))
    pulse.accum <- array(0, 1)
    #since a NA return is the same as 0 in this case, lets set all NA values to zero for easier calculations
    voxel.N.pulse[is.na(voxel.N.pulse)] <- 0
    
    #lets write a loop that goes through all the rows of the array. In this situation the rows are the vertical bins (z.bins)
    #of the lidar array. This loops cycles through these starting at the top and working its way to the bottom.
    for(i in (253):1) {
        
        #if we are looking at the top of the canopy (the last row in the array or the largest z.bin) then lets populate
        #our empty array with the same values that are in the voxel.N.pulse array (total pulses in each voxel)
        if(i == (dim(voxel.N.pulse)[1])) {
            pulse.accum[i,,] <- voxel.N.pulse[i,,]
        }
        
        #if we are looking at any other slice of the array or voxel that is not the top of the canopy (last row in the array)
        #then lets set that matrix slice (one below the previous voxel) in our empty array to the number of pulses in that canopy position
        #(from the voxel.N.pulse) plus the number of pulses in the voxel above. this produces an accumulation of pulses throughout the canopy.
        else {
            pulse.accum[i,,] <- pulse.accum[i + 1,,] + voxel.N.pulse[i,,]
        }
    }
    
    #now lets create a new array that has the number of ground returns for each column at every slice of the matrix (thus the ground points are available
    #at each vertical voxel). basically take the ground accumulation and stack it in an array equal to the number of bins in the z.bin or previously
    #generated array
    pulse.all <- array(rep(pulse.accum[1,,], each = dim(voxel.N.pulse)[1]),
                       dim = dim(voxel.N.pulse))
    
    #now we can figure out how many pulses went through each voxel (thus if there were 1000 ground hits and 100 hits at the top of the canopy, then
    #900 pulses would have went through that first voxel)
    shots.through <- pulse.all - pulse.accum
    
    #lets create another empty array this time to adjust for the pulses going in rather than the pulses going out
    shots.in <- array(dim = dim(voxel.N.pulse))
    
    #lets fill in the empty array we just created. first we need to shift all the shots out values down one voxel. this is because in the shots out
    #array the ground is a value of 0 because no pulses traveled through this column. but the accumulation of pulses above it are the number of pulses
    #that traveled into the voxel
    shots.in[1:(dim(voxel.N.pulse)[1] - 1),,] <- shots.through[2:(dim(voxel.N.pulse)[1]),,]
    
    #finally, lets add the number of pulses accumulated at the ground to the top. this is how many pulses entered the top of the canopy.
    shots.in[(dim(voxel.N.pulse)[1]),,] <- pulse.accum[1,,]
    
    ## Add the usually unadjusted LAD calculation
    # unadjLAD = ln(si-1/si)*(1/(dz)) or LAD = ln(si-1/si)*(1/(k*dz)) depending on whether k is NULL or not.
    # rLAD stands for raw LAD ... but can be unadjusted or adjusted depending on whether k is set.
    
    #now lets apply the LAD calculation based on the MacArthur Horn Method and Beer-Lambert Law.
    #if k = NULL: unadjusted LAD = ln(Si - 1 / Si) * (1 / dz)
    #if k = a number: LAD = ln(Si - 1 / Si) * (1 / (k * dz))
    #where, Si = number of pulses in each voxel, K = Beer-Lambert constant, dz = voxel.height
    
    #lets set the Beer-Lambert constant <- k for easier writing
    k <- beer.lambert.constant
    dz <- voxel.height
    
    #if no beer.lambert.constant is given then lets do this equation
    if(is.null(k)){
        
        #unadjusted lad = number of shots that went into each voxel divided by the number of shots that went through each voxel
        #then we multiple it by a theoretical beer lambert constant of 1 divded by the voxel height
        #we then need to assign all NaN and infinite values to NA
        rLAD <- log(shots.in/shots.through) * (1 / dz)
        rLAD[is.infinite(rLAD) | is.nan(rLAD)] <- NA
    }
    
    #if a beer.lambert.constant is given then lets do this equation
    else {
        
        #lets let the user know that they set the constant and what the constant is
        print("MacArthur-Horn constant is set! k = ")
        print(k)
        
        #now we are going to do the same thing as above but with the beer lambert constant put in
        rLAD <- log(shots.in/shots.through) * (1 / (k * dz))
        rLAD[is.infinite(rLAD) | is.nan(rLAD)] <- NA
    }
    
    #lets replace the zero's that we had to add into the array to make the previous code work into NA values
    #so that we can more easily calculate attributes in later steps
    for (r in 1:dim(rLAD)[2]) {
        for (c in 1:dim(rLAD)[3]) {
            
            #we need to add 2 to this because the value in the first slot is NA to initiate the array and we want to be
            #in the next voxel above the one with the last value, thus we need to add 2
            na.cut <- ceiling(leveld.lidar.array$array[2,r,c]) + 2
            
            #we need to add a failsafe if na.cut returns a NA value
            if (is.na(na.cut) == FALSE) {
                
                if (na.cut > dim(rLAD)[1]) {
                    
                } else {
                    
                    rLAD[na.cut:dim(rLAD)[1],r,c] <- NA
                    
                }
                
            } else {
                
            }
        }
    }
    
    #lets make a list to store the data we need to complete the process later on
    out <- list();
    out$rLAD <- rLAD
    out$shots.in <- shots.in
    
    #memory managment
    gc()
    remove(leveld.lidar.array)
    remove(pulse.accum)
    remove(voxel.N.pulse)
    gc()
    
    #return the final list
    return(out)
    
}

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

x11()
plot(lai.raster)
writeRaster(lai.raster, "grsm_lai_1000m.tif", overwrite = TRUE)
