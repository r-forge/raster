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

.getraster <- function(nc, vars, xvar, yvar) {
	xvar <- .getxvar(xvar, vars) 
	yvar <- .getyvar(yvar, vars) 
	# to do: also consider "lat_bnds" and "lat_bnds"
	
	ncols <- dim.inq.nc(nc, xvar)$length
	nrows <- dim.inq.nc(nc, yvar)$length
	xx <- as.vector(var.get.nc(nc, xvar))
	xrange <- c(min(xx), max(xx))
	rm(xx)
	yy <- as.vector(var.get.nc(nc, yvar))
	yrange <- c(min(yy), max(yy))
	rm(yy)
	resx <- (xrange[2] - xrange[1]) / (ncols-1)
	resy <- (yrange[2] - yrange[1]) / (nrows-1)
	xrange[1] <- xrange[1] - 0.5 * resx
	xrange[2] <- xrange[2] + 0.5 * resx
	yrange[1] <- yrange[1] - 0.5 * resy
	yrange[2] <- yrange[2] + 0.5 * resy
    r <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)
    return(r)
}


.rasterCDF <- function(filename, type, xvar='', yvar='', zvar='', time=NA) {
# to be improved for large files (i.e. do not read all data from file...)
	if (!require(RNetCDF)) { stop() }


	if (type == 'RasterBrick') {
		b <- .stackCDF(filename, type, xvar, yvar, zvar)
		b <- brick(b)
		b@file@driver <- "netcdf"
		return(b)
	}


	nc <- open.nc(filename)
	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
	zvar <- .getzvar(zvar, vars) 
	r <- .getraster(nc, vars, xvar, yvar)
	
	if (is.na(time) ) {
		d <- var.get.nc(nc, variable=zvar)
		dims <- dim(d)
		if (length(dims)== 1) { 
			stop('zvar only has a single dimension')
		} else if (length(dims)== 2) { 
			d <- as.vector(d)
		} else if (length(dims)== 3) { 
			stop('zvar has three dimensions, provide a value for "time"')
		} else if (length(dims)== 4) { 
			stop('zvar has four dimensions, I cannot process that')
		}
		
	} else {
		start <- c(1, 1, time)
		count <- c(ncol(r), nrow(r), 1)
		d <- as.vector ( var.get.nc(nc, variable=zvar, start=start, count=count) )
	} 
	close.nc(nc)

# y needs to go from big to small
	d <- matrix(d, ncol=ncol(r), nrow=nrow(r), byrow=TRUE)
	d <- as.vector( t( d[nrow(r):1,] ) )	
	r <- setValues(r, d)
	
	r@file@driver <- "netcdf"
	shortname <- gsub(" ", "_", ext(basename(filename), ""))
	x <- .enforceGoodLayerNames(x, shortname)

	return(r)
}


