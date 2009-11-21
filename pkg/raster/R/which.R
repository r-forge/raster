# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: November 2009
# Version 0.9
# Licence GPL v3


setMethod('as.logical', signature(x='RasterLayer'), 
function(x, filename='', ...) {
	if (canProcessInMemory(x, 2)){
		if (dataContent(x) != 'all') { x <- readAll(x) }
		x <- setValues(x, as.logical(values(x)))
		if (filename != '') {
			x <- writeRaster(x, filename, datatype='INT2S', ...)
		}
		return(x)
	} else {
		if (filename == '') {
			filename <- rasterTmpFile()
			if (getOption('verbose')) { cat('writing raster to:', filename)	}						
			out <- raster(x)
			for (r in 1:nrow(out)) {
				out <- setValues(out, as.logical(getValues(x, r)), r)
				out <- writeRaster(out, filename, datatype='INT2S', ...)
			}
		}
	}
}
)


if (!isGeneric("which")) {
	setGeneric("which", function(x, arr.ind=FALSE)
		standardGeneric("which"))
}	


setMethod('which', signature(x='RasterLayer'), 
function(x, arr.ind=FALSE) {
	if (canProcessInMemory(x, 2)){
		if (dataContent(x) != 'all') { x <- readAll(x) }
		x <- as.logical(x)
		if (arr.ind) {
			return(which(values(x)))
		} else {
			x[is.na(x)] <- FALSE
			return(x)
		}
	} else {
		filename <- rasterTmpFile()
		if (getOption('verbose')) { cat('writing raster to:', filename)	}
		out <- raster(x)
		vv <- vector()
		for (r in 1:nrow(out)) {
			v <- as.logical(getValues(x, r))
			if (arr.ind) {
				vv <- c(vv, which(values(x)))
			} else {
				v[is.na(v)] <- FALSE
				out <- setValues(out, v, r)
				out <- writeRaster(out, filename, datatype='INT2S')
			}
		}
		if (arr.ind) { return(vv) 
		} else { return(x) }
	}
}
)

