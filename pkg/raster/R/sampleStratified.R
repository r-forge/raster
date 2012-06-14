# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("sampleStratified")) {
	setGeneric("sampleStratified", function(x, size, ...)
		standardGeneric("sampleStratified"))
}	


setMethod('sampleStratified', signature(x='RasterLayer'), 
function(x, size, exp=10, na.rm=TRUE, xy=FALSE, ext=NULL, ...) {

	if (!hasValues(x)) {
		stop('No values associated with the Raster object')
	}	
	
	size <- round(size)
	stopifnot(size <= ncell(x))
	stopifnot(size > 0)
		
	if (!is.null(ext)) {
		oldx <- raster(x)
		x <- crop(x, ext)
	}
	f <- freq(x)
	if (na.rm) {
		na <- which(is.na(f[,1]))
		if (length(na) > 0) {
			f <- f[-na, ,drop=FALSE]
		}
	}

	exp <- max(1, exp)
	ss <- exp * size * nrow(f)
	if (ss < 1000) {
		ss <- 1000
	}
	if (ss > ncell(x)) {
		ss <- ncell(x)
	}
		
	sr <- sampleRandom(x, ss, na.rm=na.rm, ext=NULL, cells=TRUE, rowcol=FALSE, sp=FALSE)
	
	ys <- list()
	for (i in f[,1]) {
		y <- subset(sr, sr[, 2] == i)
		if (nrow(y) == 0) {
			warning("no samples found for value: ", i)
		} else {
			if (nrow(y) > size) {
				y <- y[sample(nrow(y), size),  ,drop=FALSE]
			}
			ys[[i]] <- y
		}
	}

	res <- do.call(rbind, ys)
	if (!is.null(ext)) {
		xy <- xyFromCell(x, res[,1])
		res[,1] <- cellFromXY(oldx, xy)
		if (xy) {
			res <- cbind(xy, res)
		} 
	} else if (xy) {
		xy <- xyFromCell(x, res[,1])
		res <- cbind(xy, res)
	} 
	
	ta <- tapply(res[,1], res[,2], length) 
	tanm <- names(ta)[which(ta < size)]
	
	if (length(tanm)== 1) {
		warning('fewer samples than requested found for stratum: ', tanm)
	} else if (length(tanm) > 1) {
		warning('fewer samples than requested found for strata: ', paste(tanm, collapse=', '))
	}
	return(res)
}
)




