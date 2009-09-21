# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3



addFiles <- function(rstack, rasterfiles, bands=rep(1, length(rasterfiles))) {
	if (length(bands) == 1) {
		bands=rep(bands, length(rasterfiles))
	} 
	rasters <- list()
	for (i in 1:length(rasterfiles)) { 
		if (bands[[i]] < 1) {
			r <- raster(rasterfiles[[i]], band=1)
			rasters <- c(rasters, r)
			if (nbands(r) > 1) {
				for (j in 2:nbands(r)) {
					r <- raster(rasterfiles[[i]], band=j)
					rasters <- c(rasters, r)
				}
			}
		} else {
			rasters <- c(rasters, raster(rasterfiles[[i]], FALSE, band=bands[[i]]))
		}
	}	
	rstack <- addLayer(rstack, rasters) 
	return(rstack)
}



addLayer <- function(rstack, rasters) {
#rasters is a list of raster objects
	if (class(rstack) != "RasterStack") { 
		stop("rstack should be a RasterStack object") 
	}
	if (class(rasters) == 'RasterLayer') {
		rasters <- list(rasters)
	}

	
	for (i in 1:length(rasters)) { 
		raster <- rasters[[i]]

		if (nlayers(rstack) == 0) {
			if (class(raster) == 'RasterStack') {
				rstack <- raster
			} else {
				rowcol(rstack) <- c(nrow(raster), ncol(raster))
				rstack <- setExtent(rstack, raster, snap=FALSE)
				projection(rstack) <- projection(raster)

				if (dataSource(raster) == 'ram' & dataContent(raster) != "all") {
					nl <- 0
				} else {
					nl <- 1
					rstack@data@nlayers <- as.integer(nl)
					rstack@data@min[nl] <- raster@data@min
					rstack@data@max[nl] <- raster@data@max		

					if (trim(raster@file@shortname) != "") {
						cname <- trim(raster@file@shortname)
					} else {
						cname <- "layer1"
					}
					rstack@data@colnames[1] <- cname
				#	if (dataContent(raster) == 'all') {
				#		rstack@data@values <- as.matrix(values(raster))
				#		rstack@data@content <- 'all'
				#		raster <- clearValues(raster)
				#	}
					rstack@layers[nl] <- raster 
				} 
			}
			
		} else {
			rstack <- clearValues(rstack)

			if (class(raster) == 'RasterStack') {
				rasterlist <- unstack(raster)
			} else {
				rasterlist <- list(raster)
			}
			
			for (k in 1:length(rasterlist)) {
				raster <- rasterlist[[k]]
				if (!compare(c(rstack, raster))) { 
					stop(paste("could not add raster:", filename(raster))) 
				}
				
				if (dataSource(raster) == 'ram') {
					if (dataContent(raster) != 'all') { 
						stop("Cannot add a memory based RasterLayer object without values to a Rasterstack object")
					}
				#	if (dataContent(rstack) != 'all') {
				#		rstack <- readAll(rstack)
				#	}
				}
				
				nl <- as.integer( rstack@data@nlayers + 1 )
				rstack@data@nlayers <- nl
				rstack@data@min[nl] <- raster@data@min
				rstack@data@max[nl] <- raster@data@max		

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
				#if (dataContent(rstack)=='all') {
					#if (dataContent(raster) != 'all') {
					#	raster <- readAll(raster)
					#} 
					#rstack@data@values <- cbind(rstack@data@values, values(raster))
					#raster <- clearValues(raster)
				#}
				if (dataSource(raster) == 'disk') {
						raster <- clearValues(raster)
				}
				rstack@layers[nl] <- raster 
			}	
		}
	}	
	return(rstack)
}	

