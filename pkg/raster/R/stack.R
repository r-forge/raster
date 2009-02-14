# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.8
# Licence GPL v3

stackOpen <- function(stackfile) {
	st <- read.table(stackfile, as.is=FALSE, strip.white=TRUE)
	if (dim(st)[2] > 1) {
		rst <- stackFromFiles(st[,1], st[,2])
	} else {
		rst <- stackFromFiles(st[,1])
	}
	rst <- setFilename(rst, stackfile)
	return(rst)
}

stackSave <- function(rstack) {
	stackfile <- trim(rstack@filename)
	if (stackfile == "") { stop('RasterStack does not have a filename.') }
	thefile <- file(stackfile, "w")
	for (i in 1:length(rstack@layers)) {
		fname <- trim(filename(rstack@layers[[i]]))
		if (trim(fname) == "") {
			stop("cannot save a RasterStack that has Layers without filenames. Use writeStack instead.")
		}	
		cat(fname, "\t", band(rstack@layers[[i]]),"\n", file=thefile)
	}
	close(thefile)
	return(rstack)
}



stackFromFiles <- function(rasterfiles, bands= rep(1, length(rasterfiles))) {
#	stop("this function is depracated. Use makeStack() instead.")
	rstack <- new("RasterStack") 
	return(addFiles(rstack, rasterfiles, bands))
}




if (!isGeneric("stack")) {
	setGeneric("stack", function(x, ...)
		standardGeneric("stack"))
}	

setMethod("stack", signature(x='RasterLayer'), 
function(x, ...) {
	rlist <- c(x, list(...))
	return(stack(rlist))	
} )


setMethod("stack", signature(x='character'), 
function(x, ...) {
	rlist <- c(x, list(...))
	return(stack(rlist))
} )


setMethod("stack", signature(x='list'), 
function(x) {
	for (i in 1:length(x)) {
		if (is.character(x[[i]])) {
			x[i] <- rasterFromFile(x[[i]])
		} else {
			if (class(x[[i]]) != "RasterLayer") {
				stop("Arguments should be RasterLayer objects or filenames")
			}
		}	
	}
	return(addRasters(new("RasterStack"), x))
} )



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
		nl <- rstack@data@nlayers + 1
		rstack@data@nlayers <- as.integer(nl)
		if (nlayers(rstack) == 1) {
			rstack <- setRowCol(rstack, nrow(raster), ncol(raster))
			rstack <- setBbox(rstack, raster, snap=FALSE)
			rstack <- setProjection(rstack, projection(raster))
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
		} else {
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
		rstack@layers[nl] <- raster 
		rstack@data@min[nl] <- raster@data@min
		rstack@data@max[nl] <- raster@data@max		
	}	
	return(rstack)
}	


dropLayer <- function(rstack, indices) {
	indices <- sort(indices, decreasing=TRUE)
	for (i in 1:length(indices)) {
		index <- -1 * indices[i]
		rstack@layers <- rstack@layers[index]
		rstack@data@nlayers <- as.integer(rstack@data@nlayers - 1)
	}	
	return(rstack)
}


