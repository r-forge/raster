# Robert J. Hijmans
# May 2010
# Version 1.0
# Licence GPL v3

rotated <- function(x) {
	r <- try(x@rotated, silent=TRUE)
	if (class(r) == "try-error") {
		r <- FALSE
	}
	r
}


rectify <- function(x, ext, res, method='ngb', filename='', ...) {
	stopifnot(rotated(x))
	if ( missing(ext)) {
		ext <- extent(x)
	} else {
		ext <- extent(ext)
	}
	out <- raster(ext)
	if ( missing(res)) {
		res(out) <- raster::res(x)
	} else {
		res(out) <- res
	}
	resample(x, out, method=method, filename=filename, ...)
}

