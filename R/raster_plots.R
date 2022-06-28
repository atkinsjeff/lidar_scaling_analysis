# comparison of canopy cover
library(raster)
library(viridis)
# 5 meters
#### creatiing mosaics
grsm.10.list <- list.files(path = "./results/wref", pattern = "_5.tif", ignore.case = TRUE, full.names = TRUE)

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

x11()
plot(rast.mosaic$CC, col = rev(viridis(64)))