# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("mask")) {
	setGeneric("mask", function(x, mask, ...)
		standardGeneric("mask"))
}	


setMethod('mask', signature(x='RasterLayer', mask='RasterLayer'), 
function(x, mask, filename="", ...){ 
	compare(x, mask)
	if (dataContent(x) == 'all' & dataContent(mask)=='all') {
		x[is.na(mask)] <- NA
		return(x)
	} else if (canProcessInMemory(x, 3)) {
		if (dataContent(x) != 'all') { x <- readAll(x) }
		if (dataContent(mask) != 'all') { x <- readAll(mask) }
		x[is.na(mask)] <- NA
		return(x)
	} else {
		out <- raster(x)
		vv <- matrix(ncol=nrow(out), nrow=ncol(out))
		filename <- trim(filename)
		if (!canProcessInMemory(out, 1) & filename=='') {
			filename <- rasterTmpFile()
		}
		for (r in 1:nrow(out)) {
			v <- getValues(x, r)
			m <- getValues(mask, r)
			v[is.na(m)] <- NA
			out <- setValues(out, v, r)
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			} else {
				vv[,r] <- v
			}
		}
		if (filename == '') {
			out <- setValues(out, vv)
		}
		return(out)
	}
}
)


