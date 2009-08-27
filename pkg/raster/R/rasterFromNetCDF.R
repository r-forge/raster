# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.8
# Licence GPL v3


.rasterFromNetCDF <- function(x, band, xvar='', yvar='', zvar='') {
# to be improved for large files (i.e. do not read all data from file...)
	if (!require(RNetCDF)) { stop() }
	nc <- open.nc(x)
# should do some checks if x, y and z are present. Or perhaps lat and lon in stead of x	

	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
     
	if (xvar == '') {
		if ('x' %in% vars) { 
			xvar <- 'x'
		} else if ('lon' %in% vars) { 
			xvar <- 'lon' 
		} else {
			stop('cannot find xvar in file')
		}
	} else {
		if (!(xvar %in% vars)) { 
			stop('cannot find xvar in file')
		}
	}
	if (yvar == '') {
		if ('y' %in% vars){ 
			yvar <- 'y'
		} else if ('lat' %in% vars) { 
			yvar <- 'lat' 
		} else {
			stop('cannot find yvar in file')
		}
	} else {
		if (!(yvar %in% vars)) { 
			stop('cannot find yvar in file')
		}
	}
	
	if (zvar == '') { zvar <- 'z' }
	if (!(zvar %in% vars)) { 
		stop('cannot find zvar in file')
	}
	
	ncols <- dim.inq.nc(nc, xvar)$length
	nrows <- dim.inq.nc(nc, yvar)$length
	xrange <- att.get.nc(nc, xvar, 1)
	yrange <- att.get.nc(nc, yvar, 1)
	d <- as.vector(var.get.nc(nc, zvar))
	
     close.nc(nc)
    r <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)
# y needs to go from big to small
	d <- matrix(d, ncol=ncols, nrow=nrows, byrow=TRUE)
	d <- as.vector( t( d[nrows:1,] ) )
	
	r <- setValues(r, d)
	return(r)
}

