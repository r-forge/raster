# Author: Robert J. Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.9
# Licence GPL v3

# helper functions


.showOptions <- function() {
	ft <- .filename()
	if (ft == '') {	
		cat('rasterFilenamee : ', '""', '\n')
	} else {
		cat('rasterFilename : ', ft, '\n')
	}
	cat('filetype :', .filetype(), '\n' )
	cat('datatype : ', .datatype(), '\n')
	cat('overwrite: ', .overwrite(), '\n')
	cat('tmpdir   : ', .tmpdir(), '\n')
	cat('track    : ', .track(), '\n')
	inmem <- .inMemory()
	if (!inmem) {
		cat('inMemory : FALSE\n')
	}
}



.tmpdir <- function() {
	d <- getOption('rasterTmpDir')
	if (is.null(d)) {
		d <- paste(dirname(tempdir()), '/R_raster_tmp/', sep="")
	}
	return(d)
}


.filename <- function(filename, ...) {
	if (missing(filename)) { 
		filename <- getOption('rasterFilename')
		if (is.null(filename)) {
			return('') 
		} else {
			return(filename)
		}
	} else { 
		return(filename)
	}
}

.overwrite <- function(overwrite, ...) {
	if (missing(overwrite)) { 
		overwrite <- getOption('rasterOverwrite')
		if (is.null(overwrite)) {
			return(FALSE)
		} else {
			if (is.logical(overwrite)) {
				return(overwrite)
			} else {
				return(FALSE)
			}
		}
	} else { 
		if (is.logical(overwrite)) {
			return(overwrite)
		} else {
			return(FALSE)
		}
	}
}



.datatype <- function(datatype, ...) {
	if (missing(datatype)) { 
		datatype <- getOption('rasterDatatype')
		if (is.null(datatype)) {
			return('FLT4S') 
		} else {
			return(datatype)
		}
	} else { 
		return(datatype)
	}
}

.filetype <- function(filetype, ...) {
	if (missing(filetype)) { 
	filetype <- getOption('rasterFiletype')
		if (is.null(filetype)) {
			return('raster') 
		} else {
			return(filetype)
		}
	} else { 
		return(filetype)
	}
}


.track <- function(track, ...) {
	if (missing(track)) { 
	track <- getOption('rasterTrack')
		if (is.null(track)) {
			return(-1) 
		} else {
			return(track)
		}
	} else { 
		return(track)
	}
}



.inMemory <- function(inMemory, ...) {
	if (missing(inMemory)) { 
		inMemory <- getOption('rasterProcessInMemory')
		if (is.null(inMemory)) {
			return(TRUE)
		} else {
			if (is.logical(inMemory)) {
				return(inMemory)
			} else {
				return(TRUE)
			}
		}
	} else { 
		if (is.logical(inMemory)) {
			return(inMemory)
		} else {
			return(TRUE)
		}
	}
}

