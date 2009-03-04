# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3



addFiles <- function(rstack, rasterfiles, bands=rep(1, length(rasterfiles))) {
	if (length(bands) == 1) {
		bands=rep(bands, length(rasterfiles))
	} 
	rasters <- list()
	for (i in 1:length(rasterfiles)) { 
		if (bands[[i]] < 1) {
			r <- rasterFromFile(rasterfiles[[i]], band=1)
			rasters <- c(rasters, r)
			if (nbands(r) > 1) {
				for (j in 2:nbands(r)) {
					r <- rasterFromFile(rasterfiles[[i]], band=j)
					rasters <- c(rasters, r)
				}
			}
		} else {
			rasters <- c(rasters, rasterFromFile(rasterfiles[[i]], FALSE, band=bands[[i]]))
		}
	}	
	rstack <- addRasters(rstack, rasters) 
	return(rstack)
}



addRasters <- function(rstack, rasters) {
#rasters is a list of raster objects
	if (class(rstack) != "RasterStack") { 
		stop("rstack should be a RasterStack object") 
	}
	if (length(rasters) == 1 & class(rasters) == 'RasterLayer') {
		rasters <- list(rasters)
	}

	for (i in 1 : length(rasters)) { 
		raster <- rasters[[i]]

		if (dataContent(raster) != 'all' & dataSource(raster) == 'ram') {
			stop("Cannot add a memory based RasterLayer object without values to a Rasterstack object")
		}

		if (nlayers(rstack) == 0) {
			if (class(raster) == 'RasterStack') {
				rstack <- raster
			} else {
				rstack <- setRowCol(rstack, nrow(raster), ncol(raster))
				rstack <- setBbox(rstack, raster, snap=FALSE)
				rstack <- setProjection(rstack, projection(raster))

				nl <- rstack@data@nlayers + nlayers(raster)
				rstack@data@nlayers <- as.integer(nl)
				rstack@layers[nl] <- raster 
				rstack@data@min[nl] <- raster@data@min
				rstack@data@max[nl] <- raster@data@max		

				if (trim(raster@file@shortname) != "") {
					cname <- trim(raster@file@shortname)
				} else {
					cname <- "layer1"
				}
				rstack@data@colnames[1] <- cname
				if (dataContent(raster) == 'all') {
					rstack@data@values <- as.matrix(values(raster))
					rstack@data@content <- 'all'
					raster <- clearValues(raster)
				} else {
					if (dataSource(raster) == 'ram' & dataContent(raster) != "all") {
						stop("Cannot add a memory based RasterLayer object without values to a Rasterstack object")
					}
				}

			}
			
		} else {

			if (class(raster) == 'RasterStack') {
				rasterlist <- unstack(raster)
			} else {
				rasterlist <- list(raster)
			}
			
			for (k in 1:length(rasterlist)) {
				nl <- as.integer( rstack@data@nlayers + nlayers(raster) )
				rstack@data@nlayers <- nl
				rstack@layers[nl] <- raster 
				rstack@data@min[nl] <- raster@data@min
				rstack@data@max[nl] <- raster@data@max		

				raster <- rasterlist[[k]]
				if (!compare(c(rstack, raster))) { 
					stop(paste("could not add raster:", filename(raster))) 
				}
				count <- 1
				cname <- trim(raster@file@shortname)
				if (cname == "") {
					cname <- paste("layer", nl, sep="")
				}
				cn <- cname
				for (j in 1:(nl-1)) {
					if ( cn == rstack@data@colnames[j] ) { 
						count <- count + 1 
						cn <- paste(cname, "_", count, sep="")
					}
				}	
				rstack@data@colnames[nl] <- cn
				if (dataContent(rstack)=='all') {
					if (dataContent(raster) != 'all') {
						raster <- readAll(raster)
					} 
					rstack@data@values <- cbind(rstack@data@values, values(raster))
					raster <- clearValues(raster)
				} else {
					if (dataSource(raster)=='disk') {
						raster <- clearValues(raster)
					}
				}

			}	
		}
	}	
	return(rstack)
}	


