# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,6
# Licence GPL v3

.addHistory <- function(raster, message) {
	if (is.character(message) & message != "") {
		raster@history <- c(message, raster@history)
	}	
}

setRowCol <- function(raster, nrows=nrow(raster), ncols=ncol(raster)) {
	raster@ncols <- as.integer(ncols)
	raster@nrows <- as.integer(nrows)
	return(raster)
}

setRaster <- function(raster, filename="") {
	if (class(raster) == 'RasterStack') { raster <- raster@rasters[[1]] }
	if (class(raster) != 'RasterLayer') { stop('the first argument should be a RasterLayer or a RasterStack object') }
	raster <- clearValues(raster)
	if (filename != "" & filename == filename(raster)) {
		stop("is not allowed to set the filename of the output RasterLayer to that of the input RasterLayer")
	}
	raster <- setFilename(raster, filename)
	raster <- setDatatype(raster, 'numeric')
	return(raster)
}

setFilename <- function(object, filename) {
	if (is.na(filename)) {filename <- ""}
	filename <- trim(filename)
	if (class(object)=='RasterStack') {
		object@filename <- filename
	} else {
		object@file@name <- filename
	}	
	if (class(object)=='RasterLayer') {
		if (filename != "") {
			object@file@name <- .setFileExtensionHeader(filename(object)) 
		}
		shortname <- fileName(filename)
		shortname <- setFileExtension(shortname, "")
		shortname <- gsub(" ", "_", shortname)
		if (object@file@nbands > 1) { shortname <- paste(shortname, "_", object@file@band) } 
		object@file@shortname <- shortname
		object@file@gdalhandle <- list()
	}	
	return(object)	
}


setProjection <- function(object, projstring) {
	if (class(projstring)=="CRS") {
		object@proj4string <- projstring
	} else {	
		object@proj4string <- newCRS(projstring)
	}	
	return(object)
}


clearValues <- function(raster) {
	raster@data@content <- 'nodata'
	raster@data@indices <- ""
	raster@data@values <- ""
	return(raster)
}




newCRS <- function(projstring) {
	projstring <- trim(projstring)
#	if (nchar(projstring) < 3) { 
#		projs <- (CRS(as.character(NA)))
#	} else {
		projs <- try(CRS(projstring), silent = T)
		if (class(projs) == "try-error") { 
			warning(paste(projstring, 'is not a valid proj4 CRS string')) 
			projs <- CRS(as.character(NA))
		}
#	}
	return(projs)
}


changeBbox <- function(object, xmn=xmin(object), xmx=xmax(object), ymn=ymin(object), ymx = ymax(object), projstring=projection(object), keepres=FALSE) {
	bb <- newBbox(xmn, xmx, ymn, ymx) 
	object <- setBbox(object, bb, projstring, keepres=keepres) 
	return(object)
}


newBbox <- function(xmn, xmx, ymn, ymx, projstring="") {
	if (xmn > xmx) {
		x <- xmn
		xmn <- xmx
		xmx <- x
	}
	if (ymn > ymx) {
		y <- ymn
		ymn <- ymx
		ymx <- y
	}
	projs <- newCRS(projstring)
	bb <- new("Spatial")
	bb@bbox[1,1] <- xmn
	bb@bbox[1,2] <- xmx
	bb@bbox[2,1] <- ymn
	bb@bbox[2,2] <- ymx
	bb@bbox[3,1] <- 0
	bb@bbox[3,2] <- 1
	bb@proj4string <- projs
	return(bb)
}


setBbox <- function(object, bndbox, projstring=projection(object), keepres=FALSE) {
	xrs <- xres(object)
	yrs <- yres(object)
	object@bbox[1,1] <- bndbox@bbox[1,1]
	object@bbox[1,2] <- bndbox@bbox[1,2]
	object@bbox[2,1] <- bndbox@bbox[2,1]
	object@bbox[2,2] <- bndbox@bbox[2,2]
	if (keepres) {
		object@ncols <- as.integer(round( (xmax(object) - xmin(object)) / xrs ))
		object@nrows <- as.integer(round( (ymax(object) - ymin(object)) / xrs ))
		object@bbox[1,2] <- object@bbox[1,1] + object@ncols * xrs
		object@bbox[2,2] <- object@bbox[2,1] + object@nrows * yrs
	}
	object <- setProjection(object, projstring)
	return(object)
}


makeSparse <- function(raster) {
	if ( dataContent(raster) == 'sparse') {return(raster)
	} else {
		if ( dataContent(raster) == 'all') {
			vals <- seq(1:ncells(raster))
			vals <- cbind(vals, values(raster))
			vals <- as.vector(na.omit(vals))
			raster <- setValuesSparse(raster, sparsevalues=vals[,2], indices=vals[,1])
			return(raster)
		} else { 
			# as above, but by reading data from disk, row by row
			stop('not implemented yet, use readAll() first' )
		}	
	}
}

setValuesSparse <- function(raster, sparsevalues, indices) {
	raster@data@content <- 'sparse'
	raster@data@values <- sparsevalues
	raster@data@indices <- indices
	raster@data@source <- 'ram'
	raster <- setMinmax(raster)
	return(raster)
}

setValuesBlock <- function(raster, blockvalues, firstcell, lastcell) {
	if (!is.vector(blockvalues)) {	stop('values must be a vector') }
	if (length(blockvalues) == 0) {	stop('length(blockvalues==0). If this is intended use raster.data.clear(raster)') }
	if (!(is.numeric(blockvalues) | is.integer(blockvalues) | is.logical(blockvalues))) { stop('values must be numeric, integer or logical') }
	
	firstcol <- colFromCell(raster, firstcell)
	lastcol <- colFromCell(raster, lastcell)
	firstrow <- rowFromCell(raster, firstcell)
	lastrow <- rowFromCell(raster, lastcell)
	ncells <- (lastcol - firstcol + 1) * (lastrow - firstrow + 1)
	
	if (ncells != length(blockvalues)) { 
		stop( paste("length(blockdata):", length(blockvalues), "does not match the number implied by firstcell and lastcell:", ncells)) 
	}
	raster@data@values <- blockvalues
	raster@data@content <- 'block' 
	raster@data@indices <- c(firstcell, lastcell)
	return(raster)
}


setValues <- function(raster, values, rownr=-1) {
	if (!is.vector(values)) {stop('values must be a vector')}
	if (length(values) == 0) {	stop('length(values==0). If this is intended then use clearValues(raster)') }
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {stop('data must be values')}
	rownr <- round(rownr)
	if (length(values) == ncells(raster)) { 
		if (rownr > 0) {
			stop("if setting all values, rownr must be < 1")
		}
		raster@data@values <- values
		raster@data@content <- 'all'
		raster@data@source <- 'ram'
		raster@data@indices <- c(1, ncells(raster))
		raster <- setMinmax(raster)
		return(raster)	
	} else if (length(values) == ncol(raster)) {
		if (rownr < 1 | rownr > nrow(raster)) {
			stop(paste("rownumber out of bounds:", rownr))
		}
		raster@data@values <- values
		raster@data@content <- 'row' 
		firstcell <- cellFromRowcol(raster, rownr=rownr, colnr=1)
		lastcell <- cellFromRowcol(raster, rownr=rownr, colnr=ncol(raster))
		raster@data@indices <- c(firstcell, lastcell)
		return(raster)
	} else {
		stop("length(values) is not equal to ncells(raster) or ncol(raster)") 
	}
}	
	
setMinmax <- function(raster) {
	if (raster@data@content == 'nodata') {
		stop('no data in memory')
	}
	vals <- na.omit(values(raster)) # min and max values
	if (length(vals) > 0) {
		raster@data@min <-  min(vals)
		raster@data@max <- max(vals)
	} else {
		raster@data@min <- NA
		raster@data@max <- NA
	}
	raster@data@haveminmax <- TRUE
	return(raster)
}


setDatatype <- function(raster, datatype, datasize=4) {
#  signed"  should become variable
	signed <- TRUE 
	if (datatype == "numeric") {
		raster@file@datatype <- datatype 
		if (dataContent(raster) != 'nodata') { 
			raster@data@values <- as.numeric(values(raster))
		}
		if (datasize == 4) {
			raster@file@datasize <- as.integer(4)
			raster@file@nodatavalue <- -3.4E38
			raster@file@datanotation <- "FLT4S"
		} else if (datasize == 8) {
			raster@file@datasize <- as.integer(8)
			raster@file@nodatavalue <-  -1.7E308
			raster@file@datanotation <- "FLT8S"
		} else { 
			stop("invalid datasize for this datatype") 
		}
	} else if (datatype == "integer") {
		raster@file@datatype <- datatype 
		raster@data@min <- round(minValue(raster))
		raster@data@max <- round(maxValue(raster))
		if (dataContent(raster) != 'nodata') { 
			raster@data@values <- as.integer(round(values(raster)))
		}
		if (datasize == 4) {
			raster@file@datasize <- as.integer(4)
			raster@file@nodatavalue <- -2147483647
			raster@file@datanotation <- "INT4S"
		} else if (datasize == 2) {
			raster@file@datasize <- as.integer(2)
			raster@file@nodatavalue <- -32768
			raster@file@datanotation <- "INT2S"
		} else if (datasize == 1) {
			raster@file@datasize <- as.integer(1)
			raster@file@nodatavalue <- -1
			raster@file@datanotation <- "INT1U"
			warning("binary files of single byte do not have NA values on disk")
		} else if (datasize == 8) {
			raster@file@datasize <- as.integer(8)
			raster@file@nodatavalue <- -2^63
			raster@file@datanotation <- "INT8S"
		} else {
			stop("invalid datasize for this datatype") 
		}
	} else {
		stop("unknown datatype")
	} 
	return(raster)
}

