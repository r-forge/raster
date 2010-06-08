# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.rasterGetValuesAllCDF <- function(r) {	
	nc <- open.nc(r@file@name)
	zvar = r@data@zvar
	if (r@file@nbands == 1) {
		d <- as.vector(var.get.nc(nc, variable=zvar))
		d <- as.vector(d)
	} else {
		time <- r@data@band
		start <- c(1, 1, time)
		r@file@nbands <- as.integer(dim.inq.nc(nc, var.inq.nc(nc, zvar)$dimids[3])$length)
		r@data@band <- as.integer(time)
		count <- c(ncol(r), nrow(r), 1)
		d <- as.vector ( var.get.nc(nc, variable=zvar, start=start, count=count) )
	} 
	close.nc(nc)		

	if (!is.na(r@file@nodatavalue)) {
		d[d==r@file@nodatavalue] <- NA
	}
	d <- r@data@add_offset + d * r@data@scale_factor

	d <- matrix(d, ncol=ncol(r), nrow=nrow(r), byrow=TRUE)
	
	if ( r@file@toptobottom ) { 
		d <- as.vector( t( d ) )	
	} else {
		d <- as.vector( t( d[nrow(r):1,] ) )	
	}
	return( d )
}


.rasterGetValuesRowCDF <- function(x, row, nrows=1) {
	nc <- open.nc(x@file@name)
	zvar = x@data@zvar
	start = c(1, row, x@data@band)
	count = c(x@ncols, nrows, 1)
	d <- as.vector(var.get.nc(nc, variable=zvar, start=start, count=count))
	close.nc(nc)	
	return(d)
}
	
	
.rasterGetValuesBlockCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1)) {
	nc <- open.nc(x@file@name)
	zvar = x@data@zvar
	start = c(col, row, x@data@band)
	count = c(ncols, nrows, 1)
	d <- as.vector(var.get.nc(nc, variable=zvar, start=start, count=count))
	close.nc(nc)	
	return(d)
}
	

	
	

.brickGetValuesAllCDF <- function(x, time=1, ntimes=nlayers(x)-time+1) {	
	nc <- open.nc(x@file@name)
	d <- var.get.nc(nc, x@data@zvar)
    close.nc(nc)

	if (!is.na(x@file@nodatavalue)) {
		d[d==x@file@nodatavalue] <- NA
	}
	if (x@data@add_offset != 0 | x@data@scale_factor != 1) {
		d <- x@data@add_offset + d * x@data@scale_factor
	}
	
	values <- matrix(nrow=ncell(x), ncol=nlayers(x))
	for (i in 1:nlayers(x)) {
		x <- t(d[,,i])
		x <- x[nrow(x):1, ]
		values[,i] <- as.vector(t(x))
	}
	return(values)
}	


.brickGetValuesRowCDF <- function(x, row, nrows=1, time=1, ntimes=nlayers(x)-time+1) {
	
	nc <- open.nc(x@file@name)
	zvar = x@data@zvar
	start = c(1, row, time)
	count = c(x@ncols, nrows, ntimes)
	d <- var.get.nc(nc, variable=zvar, start=start, count=count)
	close.nc(nc)
	
	dim(d) = c(x@ncols, nrows, x@data@nlayers)

	values <- matrix(nrow=nrows*ncol(x), ncol=nlayers(x))
	if (nrows > 1) {
		for (i in 1:nlayers(x)) {
			values[,i] <- as.vector(d[,,i])
		}
	} else {
		for (i in 1:nlayers(x)) {
			values[,i] <- as.vector(d[,i])
		}
	}
	return(values)
}
	
	
.brickGetValuesBlockCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), time=1, ntimes=nlayers(x)-time+1) {
	
	nc <- open.nc(x@file@name)
	zvar = x@data@zvar
	start = c(col, row, time)
	count = c(ncols, nrows, ntimes)
	d <- var.get.nc(nc, variable=zvar, start=start, count=count)
	close.nc(nc)
	
	dim(d) = c(ncols, nrows, x@data@nlayers)

	values <- matrix(nrow=nrows*ncol(x), ncol=nlayers(x))
	if (nrows > 1) {
		for (i in 1:nlayers(x)) {
			values[,i] <- as.vector(d[,,i])
		}
	} else {
		for (i in 1:nlayers(x)) {
			values[,i] <- as.vector(d[,i])
		}
	}
	return(values)
}
	
	
	

#f = "G:/cmip/ipcc/20c3m/atm/mo/pr/bccr_bcm2_0/run1/pr_A1_1.nc"
#r = .rasterFromCDF(f, zvar='pr', type='RasterLayer', time=1)
#q = .rasterReadAllCDF(r)

#b = .rasterFromCDF(f, zvar='pr', type='RasterBr', time=10)
#bd = .brickReadAllCDF(b)

	
