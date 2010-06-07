# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.rasterReadAllCDF <- function(r) {	
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
	return( setValues(r, d) )
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
	

	
	

.brickReadAllCDF <- function(b) {	
	nc <- open.nc(b@file@name)
	d <- var.get.nc(nc, b@data@zvar)
    close.nc(nc)

	if (!is.na(b@file@nodatavalue)) {
		d[d==b@file@nodatavalue] <- NA
	}
	if (b@data@add_offset != 0 | b@data@scale_factor != 1) {
		d <- b@data@add_offset + d * b@data@scale_factor
	}
	
	b@data@values <- matrix(nrow=ncell(b), ncol=nlayers(b))
	for (i in 1:nlayers(b)) {
		x <- t(d[,,i])
		x <- x[nrow(x):1, ]
		b@data@values[,i] <- as.vector(t(x))
	}
	return(b)
}	


.brickGetValuesRowCDF <- function(b, row, nrows=1) {
	nc <- open.nc(b@file@name)
	zvar = b@data@zvar
	start = c(1, row, 1)
	count = c(b@ncols, nrows, b@data@nlayers)
	d <- var.get.nc(nc, variable=zvar, start=start, count=count)
	close.nc(nc)	
	dim(d) = c(b@ncols, nrows, b@data@nlayers)

	values <- matrix(nrow=nrows*ncol(b), ncol=nlayers(b))
	if (nrows > 1) {
		for (i in 1:nlayers(b)) {
			values[,i] <- as.vector(d[,,i])
		}
	} else {
		for (i in 1:nlayers(b)) {
			values[,i] <- as.vector(d[,i])
		}
	}
	return(values)
}
	
	
.brickGetValuesBlockCDF <- function(b, row, nrows=1, col=1, ncols=(ncol(b)-col+1)) {
	nc <- open.nc(b@file@name)
	zvar = b@data@zvar
	start = c(col, row, 1)
	count = c(ncols, nrows, b@data@nlayers)
	d <- var.get.nc(nc, variable=zvar, start=start, count=count)
	close.nc(nc)	
	dim(d) = c(ncols, nrows, b@data@nlayers)

	values <- matrix(nrow=nrows*ncol(b), ncol=nlayers(b))
	if (nrows > 1) {
		for (i in 1:nlayers(b)) {
			values[,i] <- as.vector(d[,,i])
		}
	} else {
		for (i in 1:nlayers(b)) {
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

	
