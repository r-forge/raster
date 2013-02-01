# Author: Robert J. Hijmans
# Date : January 2009
# Version 1.0
# Licence GPL v3

'extent<-' <- function(x, value) {
	return(setExtent(x, value))
}


setExtent <- function(x, ext, keepres=FALSE, snap=FALSE) {
	
	oldbb <- extent(x)
	bb <- extent(ext)
	if (snap) {
		bb <- alignExtent(bb, x)
	}

	if (inherits(x, 'RasterStack')) {
		if (keepres) {
			stop('you cannot use keepres=TRUE with a RasterStack')
		}
		x@extent <- bb
		if (nlayers(x) > 0) {
			for (i in 1:nlayers(x)) {
				x@layers[[i]]@extent <- bb
			}
		} 
		return(x)
	}

	newobj <- clearValues(x)
	newobj@extent <- bb
	
	if (keepres) {
		xrs <- xres(x)
		yrs <- yres(x)
		nc <- as.integer(round( (xmax(newobj) - xmin(newobj)) / xrs ))
		if (nc < 1) {
			stop( "xmin and xmax are less than one cell apart" ) 
		} else { 
			newobj@ncols <- nc 
		}
		nr <- as.integer(round( (ymax(newobj) - ymin(newobj)) / yrs ) )
		if (nr < 1) { 
			stop( "ymin and ymax are less than one cell apart" )
		} else { 
			newobj@nrows <- nr 
		}
		newobj@extent@xmax <- newobj@extent@xmin + ncol(newobj) * xrs
		newobj@extent@ymax <- newobj@extent@ymin + nrow(newobj) * yrs
		
		if (ncol(x) == ncol(newobj) & nrow(x) == nrow(newobj)) {
			if ( canProcessInMemory(x) ) {
				newobj <- setValues(newobj, getValues(x))
			} else {
				x@extent <- extent(newobj)
				newobj <- writeRaster(x, rasterTmpFile())
			}
		}
		
		return(newobj)
		
	} else if (class(x) != "BasicRaster") {
		x@extent <- bb
		return(x)
	}
	
}

