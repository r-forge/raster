# Author: Robert J. Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.9
# Licence GPL v3


showOptions <- function() {
	cat('filetype :', .filetype(), '\n' )
	cat('datatype :', .datatype(), '\n')
	cat('overwrite:', .overwrite(), '\n')
	cat('tmpdir   :', .tmpdir(), '\n')
	cat('progress :', .progress(), '\n')
	if (!.inMemory()) {
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

.progress <- function(progress, ...) {
	if (missing(progress)) { 
		progress <- getOption('rasterProgress')
		if (is.null(progress)) {
			return('none') 
		} else {
			if (is.character(progress)) {
				if (progress[1] %in% c('text', 'tcltk', 'windows')) {
					return(progress[1])
				} else {
					return('none')
				}
			} else {
				return('none')
			}
		}
	} else { 
		if (is.character(progress)) {
			if (progress[1] %in% c('text', 'tcltk', 'windows')) {
				return(progress[1])
			} else {
				return('none')
			}
		} else {
			return('none')
		}
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

