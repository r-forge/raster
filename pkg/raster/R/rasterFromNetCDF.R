# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.8
# Licence GPL v3


.getxvar <- function(xvar, vars) {
	if (xvar == '') {
		if ('x' %in% vars) { xvar <- 'x'
		} else if ('lon' %in% vars) { xvar <- 'lon' 
		} else if ('long' %in% vars) { xvar <- 'long' 
		} else if ('longitude' %in% vars) { xvar <- 'longitude' 
		} else { stop('cannot find xvar in file') 
		}
	} else if (!(xvar %in% vars)) { stop('cannot find xvar in file') }	
	return(xvar)
}

.getyvar <- function(yvar, vars) {
	if (yvar == '') { if ('y' %in% vars){ yvar <- 'y'
		} else if ('lat' %in% vars) { yvar <- 'lat' 
		} else if ('latitude' %in% vars) { yvar <- 'latitude' 
		} else { stop('cannot find yvar in file') 
		}
	} else if (!(yvar %in% vars)) { stop('cannot find yvar in file') }	
	return(yvar)
}

.getzvar <- function(zvar, vars) {
	if (zvar == '') { zvar <- 'z' }
	if (!(zvar %in% vars)) { stop('cannot find zvar in file') }
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

rasterCDF <- function(filename, xvar='', yvar='', zvar='', time=1) {
# to be improved for large files (i.e. do not read all data from file...)
	if (!require(RNetCDF)) { stop() }
	nc <- open.nc(filename)
# should do some checks if x, y and z are present. Or perhaps lat and lon in stead of x	

	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
     
	xvar <- .getxvar(xvar, vars) 
	yvar <- .getyvar(yvar, vars) 
	zvar <- .getzvar(zvar, vars) 
	r <- .getraster(nc, xvar, yvar, zvar)
	
	d <- var.get.nc(nc, zvar)
	d <- d[,,time]
	
    close.nc(nc)
# y needs to go from big to small
	d <- matrix(d, ncol=ncol(r), nrow=nrow(r), byrow=TRUE)
	d <- as.vector( t( d[nrow(r):1,] ) )
	
	r <- setValues(r, d)
	return(r)
}




stackCDF <- function(filename, xvar='', yvar='', zvar='', time=1) {
# to be improved for large files (i.e. do not read all data from file...)
	if (!require(RNetCDF)) { stop() }
	nc <- open.nc(filename)
# should do some checks if x, y and z are present. Or perhaps lat and lon in stead of x	

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
	if (length(dims)== 3) { tsteps <- dims[3] 
	} else if (length(dims)== 3) { tsteps <- 1
	} else if (length(dims)== 2) { 
		return(stack(rasterCDF(filename, xvar, yvar, zvar)))
	} else { stop('data has unexpected dimensions') }
	

	for (i in 1:tsteps) {
		d <- d[,,i]
# y needs to go from big to small
		d <- matrix(d, ncol=ncol(r), nrow=nrow(r), byrow=TRUE)
		d <- as.vector( t( d[nrow(r):1,] ) )
		r <- setValues(r, d)
		if (i == 1) { stk <- stack(r) 
		} else { stk <- addLayer(stk, r) }
	}
	return(stk)
}
