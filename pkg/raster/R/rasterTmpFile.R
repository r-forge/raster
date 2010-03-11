# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  May 2009
# Version 0.9
# Licence GPL v3


.defaultExtention <- function(format=.filetype()) {
	if (format == 'raster') { return('.grd') 
	} else if (format == 'GTiff') { return('.tif') 
	} else if (format == 'HFA') { return( '.img') 
	} else if (format == 'ENVI') { return('.envi')
	} else if (format == 'ERS') { return('.ers') 
	} else if (format == 'RST') { return('.rst') 
	} else if (format == 'EHdr') { return('.bil')
	} else if (format == 'ascii') { return('.asc')
	} else if (format == 'netcdf') { return('.nc')
	} else { return('') }
}



rasterTmpFile <- function()  {
	extension <- .defaultExtention(.filetype())
	d <- .tmpdir()
	dir.create(d,  showWarnings = FALSE)
	f <- paste(round(runif(10)*10), collapse="")
	d <- paste(d, 'raster_', f, extension, sep="")
	if (getOption('verbose')) { cat('writing raster to:', d) }
	return(d)
}


.removeTrailingSlash <- function(d) {
		if (substr(d, nchar(d), nchar(d)) == '/') { d <- substr(d, 1, nchar(d)-1) }
		if (substr(d, nchar(d), nchar(d)) == '\\') { d <- substr(d, 1, nchar(d)-1) }
		return(d)
}


removeTmpFiles <- function() {
	d <- .removeTrailingSlash(.tmpdir())
	if (file.exists(d)) {
		unlink(paste(d, "/*", sep=""), recursive = FALSE)
	}
}

showTmpFiles <- function() {
	d <- .removeTrailingSlash(.tmpdir())
	if (file.exists(d)) {
		f <- c(list.files(d, '.grd'), list.files(d, '.gri'))
		if (length(f) == 0) {
			cat('--- none ---\n')
		} else {
			ext(f) <- ''
			f <- unique(f)
			cat(f, "\n")
		}
	} else {
		cat('--- none ---\n')
	}
}
