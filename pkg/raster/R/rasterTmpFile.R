# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  May 2009
# Version 0.9
# Licence GPL v3



rasterTmpFile <- function()  {
	d <- .tmpdir()
	dir.create(d,  showWarnings = FALSE)
	f <- paste(round(runif(10)*10), collapse="")
	d <- paste(d, f, '.grd', sep="")
	return(d)
}

removeTmpFiles <- function() {
	d <- .tmpdir()
#	unlink(paste(d,'/*', sep=""))
	gri <- Sys.glob(paste(d, '*.gri'))
	file.remove(gri)
	grd <- Sys.glob(paste(d, '*.grd'))
	file.remove(grd)
}

showTmpFiles <- function() {
	d <- .tmpdir()
	if (file.exists(d)) {
		list.files(d, '.grd')
	} else {
		cat('--- none ---\n')
	}
}
