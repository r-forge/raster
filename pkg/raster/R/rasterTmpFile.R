# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  May 2009
# Version 0.9
# Licence GPL v3


setTmpDir <- function(d=NULL) {
	if (!is.null(d)) {
		res <- file.exists(d)
		if (!res) {
			res <- dir.create(d,  showWarnings = FALSE)
		}
		if (res) { options(rasterTmpDir = d) }
	}
}


rasterTmpDir <- function() {
	d <- getOption('rasterTmpDir')
	if (is.null(d)) {
		d <- paste(dirname(tempdir()), '/R_raster_tmp/', sep="")
	}
	return(d)
}

rasterTmpFile <- function()  {
	d <- rasterTmpDir()
	dir.create(d,  showWarnings = FALSE)
	f <- paste(round(runif(10)*10), collapse="")
	d <- paste(d, f, '.grd', sep="")
	return(d)
}

removeTmpFiles <- function() {
	d <- rasterTmpDir()
#	unlink(paste(d,'/*', sep=""))
	gri <- Sys.glob(paste(d, '*.gri'))
	file.remove(gri)
	grd <- Sys.glob(paste(d, '*.grd'))
	file.remove(grd)
}

showTmpFiles <- function() {
	d <- rasterTmpDir()
	if (file.exists(d)) {
		list.files(d, '.grd')
	} else {
		cat('--- none ---\n')
	}
}
