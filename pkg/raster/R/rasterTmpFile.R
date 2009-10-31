# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  May 2009
# Version 0.9
# Licence GPL v3


rasterTmpFile <- function()  {
	d <- .tmpdir()
	dir.create(d,  showWarnings = FALSE)
	f <- paste(round(runif(10)*10), collapse="")
	d <- paste(d, 'raster_', f, '.grd', sep="")
	return(d)
}

removeTmpFiles <- function() {
	d <- .tmpdir()
	f <- c(list.files(d, pattern="^raster_.*gri$", full.names=TRUE), list.files(d, pattern="^raster_.*grd$", full.names=TRUE))
	if (length(f) > 0) {
		r <- file.remove(f)
	}
}

showTmpFiles <- function() {
	removeTrailingSlash <- function(d) {
		if (substr(d, nchar(d), nchar(d)) == '/') { d <- substr(d, 1, nchar(d)-1) }
		if (substr(d, nchar(d), nchar(d)) == '\\') { d <- substr(d, 1, nchar(d)-1) }
		return(d)
	}
	d <- .tmpdir()
	d <- removeTrailingSlash(d)
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
