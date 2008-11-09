# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


set.rowcol <- function(raster, nrows=nrow(raster), ncols=ncol(raster)) {
	raster@ncols <- as.integer(ncols)
	raster@nrows <- as.integer(nrows)
	return(raster)
}

set.raster <- function(raster, filename=NA) {
	if (class(raster) == 'RasterStack') { raster <- raster@rasters[[1]] }
	if (class(raster) != 'RasterLayer') { stop('the first argument should be a RasterLayer or a RasterStack object') }
	raster <- clear.values(raster)
	raster <- set.filename(raster, filename)
#	if (INT) { raster <- set.datatype(raster, "integer")  }
#	else { raster <- set.datatype(raster, "numeric") }
	return(raster)
}

set.filename <- function(object, filename) {
	if (is.na(filename)) {filename <- ""}
	object@file@name <- filename
	if (class(object)=='RasterLayer') {
		shortname <- file.get.name(filename)
		shortname <- file.change.extension(shortname, "")
		shortname <- gsub(" ", "_", shortname)
		if (object@file@nbands > 1) { shortname <- paste(shortname, "_", object@file@band) } 
		object@file@shortname <- shortname
		object@file@gdalhandle <- list()
	}	
	return(object)
}


set.projection <- function(object, projection) {
	if (class(projection)=="CRS") {
		object@proj4string <- projection
	} else {	
		object@proj4string <- new.CRS(projection)
	}	
	return(object)
}


clear.values <- function(raster) {
	raster@data@content <- 'nodata'
	raster@data@indices <- ""
	raster@data@values <- ""
	return(raster)
}


set.bbox <- function(raster, xmn=xmin(raster), xmx=xmax(raster), ymn=ymin(raster), ymx=ymax(raster), keepres=FALSE) {
	xrs <- xres(raster)
	yrs <- yres(raster)
	if (xmn > xmx) {
		stop('xmn > xmx')
	}
	if (ymn > ymx) {
		stop('ymn > ymx')
	}
	raster@bbox[1,1] <- xmn
	raster@bbox[1,2] <- xmx
	raster@bbox[2,1] <- ymn
	raster@bbox[2,2] <- ymx
	if (keepres) {
		raster@ncols <- as.integer(round( (xmax(raster) - xmin(raster)) / xrs ))
		raster@nrows <- as.integer(round( (ymax(raster) - ymin(raster)) / xrs ))
		raster@bbox[1,2] <- raster@bbox[1,1] + raster@ncols * xrs
		raster@bbox[2,2] <- raster@bbox[2,1] + raster@nrows * yrs
	}
	return(raster)
}


new.CRS <- function(projection) {
	if (nchar(projection) < 6) { projs <- (CRS(as.character(NA)))
	} else {
		projs <- try(CRS(projection), silent = T)
		if (class(projs) == "try-error") { 
			warning(paste(projection, 'is not a valid proj4 CRS string')) 
			projs <- (CRS(as.character(NA)))
		}
	}
	return(projs)
}

new.boundingbox <- function(xmn, xmx, ymn, ymx, projection="") {
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
	projs <- new.CRS(projection)
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


make.sparse <- function(raster) {
	if ( data.content(raster) == 'sparse') {return(raster)
	} else {
		if ( data.content(raster) == 'all') {
			vals <- seq(1:ncells(raster))
			vals <- cbind(vals, values(raster))
			vals <- as.vector(na.omit(vals))
			raster <- set.values.sparse(raster, sparsevalues=vals[,2], indices=vals[,1])
			return(raster)
		} else { 
			# as above, but by reading data from disk, row by row
			stop('not implemented yet, use read.all() first' )
		}	
	}
}

set.values.sparse <- function(raster, sparsevalues, indices) {
	raster@data@content <- 'sparse'
	raster@data@values <- sparsevalues
	raster@data@indices <- indices
	raster@data@source <- 'ram'
	raster <- set.minmax(raster)
	return(raster)
}

set.values.block <- function(raster, blockvalues, firstcell, lastcell) {
	if (!is.vector(blockvalues)) {	stop('values must be a vector') }
	if (length(blockvalues) == 0) {	stop('length(blockvalues==0). If this is intended use raster.data.clear(raster)') }
	if (!(is.numeric(blockvalues) | is.integer(blockvalues) | is.logical(blockvalues))) { stop('values must be numeric, integer or logical') }
	
	firstcol <- get.col.from.cell(raster, firstcell)
	lastcol <- get.col.from.cell(raster, lastcell)
	firstrow <- get.row.from.cell(raster, firstcell)
	lastrow <- get.row.from.cell(raster, lastcell)
	ncells <- (lastcol - firstcol + 1) * (lastrow - firstrow + 1)
	
	if (ncells != length(blockvalues)) { 
		stop( paste("length(blockdata):", length(blockvalues), "does not match the number implied by firstcell and lastcell:", ncells)) 
	}
	raster@data@values <- blockvalues
	raster@data@content <- 'block' 
	raster@data@indices <- c(firstcell, lastcell)
	return(raster)
}


set.values.row <- function(raster, rowvalues, rownr) {
	if (!is.vector(rowvalues)) {	stop('data must be a vector') }
	if (length(rowvalues) == 0) {	stop('length(rowdata==0). If this is intended then use raster.data.clear(raster)') }
	if (!(is.numeric(rowvalues) | is.integer(rowvalues) | is.logical(rowvalues))) { stop(paste('data must be values, but class =',class(rowvalues))) }
	if (length(rowvalues) != ncol(raster)) { stop('length(rowdata) != ncol(raster)') 
	} else {	
		raster@data@values <- rowvalues
		raster@data@content <- 'row' 
		firstcell <- get.cell.from.rowcol(raster, rownr=rownr, colnr=1)
		lastcell <- get.cell.from.rowcol(raster, rownr=rownr, colnr=ncol(raster))
		raster@data@indices <- c(firstcell, lastcell)
		return(raster)
	}	
}	


set.values <- function(raster, values) {
	if (!is.vector(values)) {stop('values must be a vector')}
	if (length(values) == 0) {	stop('length(values==0). If this is intended then use raster.data.clear(raster)') }
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {stop('data must be values')}
	if (length(values) != ncells(raster) ) { stop('length(values) != ncells(raster)') 
	} else {	
		raster@data@values <- values
		raster@data@content <- 'all'
		raster@data@source <- 'ram'
		raster@data@indices <- c(1, ncells(raster))
		raster <- set.minmax(raster)
		return(raster)	
	}	
}


set.minmax <- function(raster) {
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


set.datatype <- function(raster, datatype, datasize=4) {
#  signed"  should become variable
	signed <- TRUE 
	if (datatype == "numeric") {
		raster@file@datatype <- datatype 
		if (datasize == 4) {
			raster@file@datasize <- as.integer(4)
			raster@file@nodatavalue <- -3.4E38
			raster@file@datanotation <- "FLT4S"
		} else if (datasize == 8) {
			raster@file@datasize <- as.integer(8)
			raster@file@nodatavalue <-  -1.7E308
			raster@file@datanotation <- "FLT8S"
		} else { 	stop("invalid datasize for this datatype") }
	} else if (datatype == "integer") {
		raster@file@datatype <- datatype 
		if (datasize == 4) {
			raster@file@datasize <- as.integer(4)
			raster@file@nodatavalue <- -2147483645
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
		} else { stop("invalid datasize for this datatype") }
	} else {stop("unknown datatype")} 
	return(raster)
}

