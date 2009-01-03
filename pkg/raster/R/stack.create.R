# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0,1
# Licence GPL v3

stackFromFile <- function(stackfile) {
	st <- read.table(stackfile,  as.is=FALSE, strip.white=TRUE)
	rasterfiles <- list()
	bands <- list()
	for (i in 1:length(st[,1])) {
		rasterfiles[i] <- as.character(st[i,1])
		bands[i] <- as.integer(st[i,2])
	}
	rst <- stackFromRasterfiles(rasterfiles, bands)
	rst <- setFilename(stackfile)
	return(rst)
}


stackFromRasterfiles <- function(rasterfiles, bands= rep(1, length(rasterfiles))) {
	return(stackAddFiles(NA, rasterfiles, bands))
}


stackFromRasters <- function(rasters) {
	return(stackAddRasters(NA, rasters))
}


stackAddFiles <- function(rstack, rasterfiles, bands= rep(1, length(rasterfiles))) {
	if (class(rstack) != "RasterStack") { rstack <- new("RasterStack") }
	
	if (is.list(rasterfiles)) {rasterfiles <- unlist(rasterfiles)}
	if (is.list(bands)) {bands <- unlist(bands)}
	
	for (i in 1 : length(rasterfiles)) { 
		if (length(rasterfiles)==1) { 
			fn <- rasterfiles
			band <- bands 
		} else {
			fn <- rasterfiles[i]
			band <- bands[i]
		}	
		filename <- trim(fn)
		if (!(file.exists(filename))) { 
			stop(paste(filename, "does not exist")) 
		}
		raster <- rasterFromFile(filename, band)
		rstack <- stackAddRasters(rstack, raster) 
	}
	return(rstack)
}



stackAddRasters <- function(rstack, rasters) {
#rasters is a list of raster objects
	if (class(rstack) != "RasterStack") { rstack <- new("RasterStack") }

	for (i in 1 : length(rasters)) { 
		if (length(rasters) == 1) { raster <- rasters 
		} else { raster <- rasters[[i]] }
		
		addraster <- TRUE
		i <- nlayers(rstack) + 1
		if (i == 1) {
			rstack <- setRowCol(rstack, nrow(raster), ncol(raster))
			rstack <- setBbox(rstack, raster)
		} else {
			if (length(attr(rstack@proj4string, "projection")) != 0)
				{
				if ( length(attr(raster@proj4string, "projection")) == 0 ) { warning("raster with unknown projection added") 
				} else if (rstack@proj4string != raster@proj4string) { warning("different projections used") }	
			}
			if (!compare(c(rstack, raster))) { stop (paste("could not add raster:", filename(raster))) }
			count <- 1
			for (j in 1:(i-1)) {
				if (filename(raster) == rstack@rasters[[j]]@file@shortname) { 
					count <- count + 1 
				}
			}	
			if (count > 1) { 
				raster@file@shortname <- paste(raster@file@shortname, "_", count, sep="") }
		}	
		rstack@rasters[i] <- raster 
		rstack@data@nlayers <- as.integer(rstack@data@nlayers + 1)
	}	
	return(rstack)
}	


stackRemoveRasters <- function(rstack, indices) {
	indices <- sort(indices, decreasing=TRUE)
	for (i in 1:length(indices)) {
		index <- -1 * indices[i]
		rstack@rasters <- rstack@rasters[index]
		rstack@data@nlayers <- as.integer(rstack@data@nlayers - 1)
	}	
	return(rstack)
}


stackSave <- function(rstack) {
	stackfile <- rstack@filename
	if (stackfile == "") { stop('RasterStack does not have a filename.') }
	thefile <- file(stackfile, "w")
	for (i in 1:length(rstack@rasters)) {
		cat(rstack@rasters[[i]]@file@name, "\t", rstack@rasters[[i]]@band,"\n", file=thefile)
		}
	close(thefile)
	return(rstack)
}


