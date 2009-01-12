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
	raster <- clearValues(raster)
	raster@ncols <- as.integer(ncols)
	raster@nrows <- as.integer(nrows)
	return(raster)
}

setRaster <- function(object, filename="", values=NA) {
	if (class(object) == 'RasterStack') { object <- asRasterLayer(object, 1) }
	if (class(object) == 'RasterBrick') { object <- asRasterLayer(object, 1) }
	if (class(object) != 'RasterLayer') { stop('the first argument should be a RasterLayer or a RasterStack object') }

	filename <- trim(filename)
	if (filename != "" & filename == filename(object)) {
		stop("it is not allowed to set the filename of the output RasterLayer to that of the input RasterLayer")
	}

	raster <- clearValues(object)
	raster@data@min <- NA
	raster@data@max <- NA

	raster <- setFilename(raster, filename)
	raster <- setDatatype(raster, 'numeric')
	if ( length(values) != 1 | ( length(values) == 1 & ncells(raster) == 1) ) {
		raster <- setValues(raster, values)
	}
	return(raster)
}

setFilename <- function(object, filename) {
	if (is.na(filename)) {filename <- ""}
	filename <- trim(filename)
	if (class(object)=='RasterStack') {
		object@filename <- setFileExtension(filename, ".stk")
	} else {
		object@file@name <- filename
	}	
	if (class(object)=='RasterLayer') {
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
		object@crs <- projstring
	} else {	
		object@crs <- newCRS(projstring)
	}	
	return(object)
}


clearValues <- function(object) {
	object@data@content <- 'nodata'
	object@data@indices <- ""
	if (class(object) == 'RasterLayer') {
		object@data@values <- vector()
	} else {
		object@data@values <- matrix(NA,0,0)
	}
	return(object)
}


roundCoords <- function(object, digits=0) {
	digits <- max(0, digits)
	b <- getBbox(object)
	b@xmin <- round(b@xmin, digits)
	b@xmax <- round(b@xmax, digits)
	b@ymin <- round(b@ymin, digits)
	b@ymax <- round(b@ymax, digits)
	if (class(object) == 'BoundingBox') {
		return(b)
	}
	object <- setBbox(object, b)
	return(object)
}



newCRS <- function(projstring) {
	projstring <- trim(projstring)
	if (is.na(projstring) | nchar(projstring) < 3) { 
		projs <- (CRS(as.character(NA)))
	} else {
		projs <- try(CRS(projstring), silent = T)
		if (class(projs) == "try-error") { 
			warning(paste(projstring, 'is not a valid proj4 CRS string')) 
			projs <- CRS(as.character(NA))
		}
	}
	return(projs)
}


changeBbox <- function(object, xmn=xmin(object), xmx=xmax(object), ymn=ymin(object), ymx = ymax(object), keepres=FALSE) {
	bb <- newBbox(xmn, xmx, ymn, ymx) 
	object <- setBbox(object, bb, keepres=keepres) 
	return(object)
}


newBbox <- function(xmn, xmx, ymn, ymx) {
	bb <- new('BoundingBox')
	bb@xmin <- xmn
	bb@xmax <- xmx
	bb@ymin <- ymn
	bb@ymax <- ymx
	return(bb)
}

getBbox <- function(object) {
	if ( class(object) == 'BoundingBox' ) { 
		bb <- object 
	} else if ( class(object) == 'RasterLayer' | class(object) == 'RasterStack' | class(object) == 'RasterBrick' ) {
		bb <- object@bbox
	} else if (class(object) == "matrix") {
		bb <- new('BoundingBox')
		bb@xmin <- object[1,1]
		bb@xmax <- object[1,2]
		bb@ymin <- object[2,1]
		bb@ymax <- object[2,2]
	} else if (class(object) == "vector") {
		bb <- new('BoundingBox')
		bb@xmin <- object[1]
		bb@xmax <- object[2]
		bb@ymin <- object[3]
		bb@ymax <- object[4]
	} else {
		bndbox <- bbox(object)
		bb <- new('BoundingBox')
		bb@xmin <- bndbox[1,1]
		bb@xmax <- bndbox[1,2]
		bb@ymin <- bndbox[2,1]
		bb@ymax <- bndbox[2,2]
	}
	return(bb)
}


setBbox <- function(object, bndbox, keepres=FALSE) {
	xrs <- xres(object)
	yrs <- yres(object)
	object@bbox <- getBbox(bndbox)
	if (keepres) {
		nc <- as.integer(round( (xmax(object) - xmin(object)) / xrs ))
		if (nc < 1) { stop( "xmin and xmax are less than one cell apart" ) 
		} else { object@ncols <- nc }
		nr <- as.integer(round( (ymax(object) - ymin(object)) / xrs ) )
		if (nr < 1) { stop( "ymin and ymax are less than one cell apart" )
		} else { object@nrows <- nr }
		object@bbox@xmax <- object@bbox@xmin + ncol(object) * xrs
		object@bbox@ymax <- object@bbox@ymin + nrow(object) * yrs
	}
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

setValuesSparse <- function(raster, sparsevalues, cellnumbers) {
	if (!(isTRUE(length(cellnumbers) == (length(sparsevalues))))) {
		stop()
	}
	raster@data@content <- 'sparse'
	raster@data@values <- sparsevalues
	raster@data@indices <- cellnumbers
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
	if (dataContent(raster) == 'nodata') {
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

