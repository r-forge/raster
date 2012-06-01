# Author: Robert J. Hijmans
# Date :  May 2012
# Version 1.0
# Licence GPL v3


.hasRAT <- function(x) {
	if (.hasSlot(x, 'data')) {
		if (.hasSlot(x@data, 'hasRAT')) {
			x@data@hasRAT
		} else {
			FALSE
		}
	} else {
		FALSE
	}
}


rats <- function(x) {
	if(! .hasRAT(x) ) {
		stop('This object has no Raster Attribute Table')
	}
	if (nlayers(x) > 1) {
		x@data@attributes
	} else {
		x@data@attributes[[1]]	
	}
}

'rats<-' <- function(x, value) {
	stopifnot(is.data.frame(value))
	stopifnot(ncol(value) > 2)
	stopifnot(colnames(value)[1:2] == c('VALUE', 'COUNT'))
	x@data@hasRAT <- TRUE
	x@data@isfactor <- TRUE
	x@data@attributes <- list(value)
	x
}


ratToLayer <- function(x, att=NULL, filename='', ...) {
	RAT <- rats(x)
	if (is.null(att)) {
		att <- 3:ncol(RAT)
	}
	ratvs <- RAT[ , att, drop=FALSE]
	if (is.null(ratvs)) {
		stop("'att' is not a valid column in the Raster Attribute Table")
	}
	ratvs <- data.frame(VALUE=RAT$VALUE, ratvs)
	w <- unique(2:ncol(ratvs))
	subs(x, ratvs, by=1, which=w, subsWithNA=TRUE, filename=filename, ...)	
}
#b = ratToLayer(x, 'WHRSIZE')


ratify <- function(x, filename='', ...) {
	stopifnot(nlayers(x) == 1)
	f <- freq(x, useNA='no')
	f <- data.frame(f)
	colnames(f) <- toupper(colnames(f))
	x@data@hasRAT <- TRUE
	x@data@isfactor <- TRUE
	x@data@attributes <- list(f)
	if (filename != '') {
		x <- writeRaster(x, filename, ...)
		# only native format stores this...
		x@data@hasRAT <- TRUE
		x@data@isfactor <- TRUE
		x@data@attributes <- list(f)	
	}
	return(x)
}

