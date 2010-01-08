# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2009
# Version 0.9
# Licence GPL v3


.stackCDF <- function(nc, type, r, xvar, yvar, zvar, time, add_offset, scale_factor, missing_value, long_name, prj) {
# to be improved for large files (i.e. do not read all data from file...)

	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
	dd <- var.get.nc(nc, zvar)
    close.nc(nc)
	
	dims <- dim(dd)
	getRaster <- FALSE
	if (length(dims)== 3) { 
		if (is.numeric(time)) { 
			tsteps <- time	
		} else { 
			tsteps <- 1:dims[3] 
		}
		if (length(tsteps) < 2) { stop('cannot make a RasterStack or Brick from a single time step, use raster() instead, and then make a stack or brick from that') } 
		
	} else if (length(dims)== 2) { 
		stop('cannot make a RasterStack or Brick from a data that has only two dimensions (no time step), use raster() instead, and then make a stack or brick from that')	
	} else { 
		stop(paste('data has an unexpected number of dimensions', dims)) 
	}
	
	
#	for (i in tsteps) {

	d <- dd[,,tsteps]
	dims <- dim(d)
	if (!is.na(missing_value)) {
		d[d==missing_value] <- NA
	}
	d <- add_offset + d * scale_factor

	stk <- new('RasterStack')

	for (i in 1:dims[3]) {
		x <- t(d[,,i])
		x <- x[nrow(x):1, ]
		r[] <- as.vector(t(x))
		stk <- addLayer(stk, r)
	}
	if (type=="RasterStack") {	
		attr(stk, 'prj') <- prj
		return(stk) 
	} else {
		b <- brick(stk)
		b@title <- long_name
		attr(b, 'prj') <- prj
		return(b)
	}
}

