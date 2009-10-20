# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2009
# Version 0.9
# Licence GPL v3


.stackCDF <- function(filename, xvar='', yvar='', zvar='', time='') {
# to be improved for large files (i.e. do not read all data from file...)
	if (!require(RNetCDF)) { stop() }
	nc <- open.nc(filename)

	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
     
	r <- .getraster(nc, vars, xvar, yvar) 
	stk <- stack( r )
	
	zvar <- .getzvar(zvar, vars) 
	
	dd <- var.get.nc(nc, zvar)
    close.nc(nc)
	
	dims <- dim(dd)
	if (length(dims)== 3) { 
		if (is.numeric(time)) { 
			tsteps <- time	
		} else { 
			tsteps <- 1:dims[3] 
		}
	} else if (length(dims)== 3) { tsteps <- 1
	} else if (length(dims)== 2) { 
		return(stack(.rasterFromCDF(filename, xvar, yvar, zvar)))
	} else { stop(paste('data has an unexpected number of dimensions', dims)) }
	

#	for (i in tsteps) {
	d <- dd[,,tsteps]
	dims <- dim(d)
	for (i in 1:dims[3]) {
		x <- t(d[,,i])
		x <- x[nrow(x):1, ]
		r[] <- as.vector(t(x))
		stk <- addLayer(stk, r)
	}

	shortname <- gsub(" ", "_", ext(basename(filename), ""))
	x <- .enforceGoodLayerNames(x, shortname)
	
	return(stk)
}

