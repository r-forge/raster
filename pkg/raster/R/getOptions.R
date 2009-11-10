# Author: Robert J. Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.9
# Licence GPL v3


showOptions <- function() {
	cat('format :', .filetype(), '\n' )
	cat('datatype :', .datatype(), '\n')
	cat('overwrite:', .overwrite(), '\n')
	cat('tmpdir   :', .tmpdir(), '\n')
	cat('progress :', .progress(), '\n')
	if (.toDisk()) {
		cat('toDisk   : TRUE\n')
	}
}



.tmpdir <- function() {
	d <- getOption('rasterTmpDir')
	if (is.null(d)) {
		d <- paste(dirname(tempdir()), '/R_raster_tmp/', sep="")
	}
	if (!file.exists(d)) {
		d <- paste(dirname(tempdir()), '/R_raster_tmp/', sep="")
		dir.create(d, showWarnings=FALSE )
	}
	return(d)
}


.overwrite <- function(..., overwrite) {
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


.datatype <- function(..., datatype) {
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

.filetype <- function(..., format) {
	if (missing(format)) { 
		format <- getOption('rasterFiletype')
		if (is.null(format)) {
			return('raster') 
		} else {
			return(format)
		}
	} else { 
		return(format)
	}
}

.progress <- function(..., progress) {
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


.toDisk <- function(..., todisk) {
	if (missing(todisk)) { 
		todisk <- getOption('rasterToDisk')
		if (is.null(todisk)) {
			return(FALSE)  # the default
		} else {
			if (is.logical(todisk)) {
				return(todisk)
			} else {
				return(FALSE)
			}
		}
	} else { 
		if (is.logical(todisk)) {
			return(todisk)
		} else {
			return(FALSE)
		}
	}
}

