# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.9
# Licence GPL v3

.getxvar <- function(xvar, vars) {
	if (xvar == '') {
		if ('x' %in% vars) { xvar <- 'x'
		} else if ('lon' %in% vars) { xvar <- 'lon' 
		} else if ('long' %in% vars) { xvar <- 'long' 
		} else if ('longitude' %in% vars) { xvar <- 'longitude' 
		} else { stop('Cannot find an obvious xvar in file. Select one from:\n', paste(vars, collapse=", "))  
		}
	} else if (!(xvar %in% vars)) { stop( paste('Cannot find xvar in file. Select one from:\n', paste(vars, collapse=", "))) }	
	return(xvar)
}

.getyvar <- function(yvar, vars) {
	if (yvar == '') { if ('y' %in% vars){ yvar <- 'y'
		} else if ('lat' %in% vars) { yvar <- 'lat' 
		} else if ('latitude' %in% vars) { yvar <- 'latitude' 
		} else { stop('Cannot find an obvious yvar in file. Select one from:\n', paste(vars, collapse=", "))  
		}
	} else if (!(yvar %in% vars)) { stop( paste('Cannot find yvar in file. Select one from:\n', paste(vars, collapse=", "))) }	
	return(yvar)
}

.getzvar <- function(zvar, vars) {
	if (zvar == '') { zvar <- 'z' }
	if (!(zvar %in% vars)) { stop ( 'Cannot find an obvious zvar in file. Select one from:\n', paste(vars, collapse=", ") ) }
	return(zvar)
}

.getraster <- function(nc, xvar, yvar, zvar) {
	ncols <- dim.inq.nc(nc, xvar)$length
	nrows <- dim.inq.nc(nc, yvar)$length
	xx <- as.vector(var.get.nc(nc, xvar))
	xrange <- c(min(xx), max(xx))
	rm(xx)
	yy <- as.vector(var.get.nc(nc, yvar))
	yrange <- c(min(yy), max(yy))
	rm(yy)
    r <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)
    return(r)
}


.rasterCDF <- function(filename, xvar='', yvar='', zvar='', time=1) {
# to be improved for large files (i.e. do not read all data from file...)
	if (!require(RNetCDF)) { stop() }
	nc <- open.nc(filename)
	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
	xvar <- .getxvar(xvar, vars) 
	yvar <- .getyvar(yvar, vars) 
	zvar <- .getzvar(zvar, vars) 
	r <- .getraster(nc, xvar, yvar, zvar)
	d <- var.get.nc(nc, zvar)
    close.nc(nc)
    
	dims <- dim(d)

	if (length(dims)== 2) { 
		d <- as.vector(d)
	} else if (length(dims)== 3) { 
		d <- as.vector(d[,,time])
	} else { stop(paste('data has an unexpected number of dimensions', dims)) }

# y needs to go from big to small
	d <- matrix(d, ncol=ncol(r), nrow=nrow(r), byrow=TRUE)
	d <- as.vector( t( d[nrow(r):1,] ) )	
	r <- setValues(r, d)
	return(r)
}



.stackCDF <- function(filename, xvar='', yvar='', zvar='', time='') {
# to be improved for large files (i.e. do not read all data from file...)
	if (!require(RNetCDF)) { stop() }
	nc <- open.nc(filename)

	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
     
	xvar <- .getxvar(xvar, vars) 
	yvar <- .getyvar(yvar, vars) 
	zvar <- .getzvar(zvar, vars) 
	r <- .getraster(nc, xvar, yvar, zvar)
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
		return(stack(.rasterCDF(filename, xvar, yvar, zvar)))
	} else { stop(paste('data has an unexpected number of dimensions', dims)) }
	

	for (i in tsteps) {
		d <- dd[,,i]
# y needs to go from big to small
		d <- matrix(d, ncol=ncol(r), nrow=nrow(r), byrow=TRUE)
		d <- as.vector( t( d[nrow(r):1,] ) )
		r <- setValues(r, d)
		if (i == 1) { stk <- stack(r) 
		} else { stk <- addLayer(stk, r) }
	}
	return(stk)
}


