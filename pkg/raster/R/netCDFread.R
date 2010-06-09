# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.rasterGetValuesAllCDF <- function(x) {	

	nc <- open.nc(x@file@name)
	zvar = x@data@zvar
	
	if (x@file@nbands == 1) {
		d <- var.get.nc(nc, variable=zvar)
	} else {
		time <- x@data@band
		start <- c(1, 1, time)
		x@file@nbands <- as.integer(dim.inq.nc(nc, var.inq.nc(nc, zvar)$dimids[3])$length)
		x@data@band <- as.integer(time)
		count <- c(ncol(x), nrow(x), 1)
		d <- var.get.nc(nc, variable=zvar, start=start, count=count) 
	} 
	close.nc(nc)		

	if (!is.na(x@file@nodatavalue)) { 
		d[d==x@file@nodatavalue] <- NA
	}
	d <- x@data@add_offset + d * x@data@scale_factor

	if ( x@file@toptobottom ) { 
		# in d, cols and rows are reversed, hence chaning the col order
		d <- d[, ncol(d):1] 
	}
	
	return( as.vector(d) )
}



	
.readRowsNetCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1)) {

	if ( x@file@toptobottom ) { row <- x@nrows - row - nrows + 2	}
	
	nc <- open.nc(x@file@name)
	zvar = x@data@zvar

	if (x@file@nbands == 1) {
		start = c(col, row)
		count = c(ncols, nrows)
		d <- var.get.nc(nc, variable=zvar, start=start, count=count)

	} else {
		start = c(col, row, x@data@band)
		count = c(ncols, nrows, 1)
	}
	d <- var.get.nc(nc, variable=zvar, start=start, count=count)
	close.nc(nc)	

	if (!is.na(x@file@nodatavalue)) { 
		d[d==x@file@nodatavalue] <- NA
	}
	d <- x@data@add_offset + d * x@data@scale_factor
	
	if (length(dim(d)) > 1) {
		if ( x@file@toptobottom ) { 
			d <- d[, ncol(d):1] 	
		}
	}
	return( as.vector(d) )
	
}
	
	
	


.brickGetValuesAllCDF <- function(x, time=1, ntimes=nlayers(x)-time+1) {	

	time   =  min( max( round(time), 1), nlayers(x))
	ntimes =  min( max( round(ntimes), 1), nlayers(x)-time+1 )
	
	nc <- open.nc(x@file@name)

	start = c(1, 1, time)
	count = c(x@ncols, x@nrows, ntimes)
	d <- var.get.nc(nc, variable=x@data@zvar, start=start, count=count)
    close.nc(nc)

	if (!is.na(x@file@nodatavalue)) {
		d[d==x@file@nodatavalue] <- NA
	}
	if (x@data@add_offset != 0 | x@data@scale_factor != 1) {
		d <- x@data@add_offset + d * x@data@scale_factor
	}
	
	values <- matrix(nrow=ncell(x), ncol=ntimes)
	colnames(values) = time:(time+ntimes-1)
	
	if ( x@file@toptobottom ) { 
		for (i in 1:ntimes) {
			x <- (d[,,i])
			values[,i] <- as.vector( x[, ncol(x):1] )
		}
	} else {
		for (i in 1:ntimes) {
			values[,i] <- as.vector(x)
		}
	}
	
	return(values)
}	


	
.readRowsBrickNetCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), time=1, ntimes=nlayers(x)-time+1) {
	
	
	if ( x@file@toptobottom ) { row <- x@nrows - row - nrows + 2	}
	
	
	time   =  min( max( round(time), 1), nlayers(x))
	ntimes =  min( max( round(ntimes), 1), nlayers(x)-time+1 )
	
	nc <- open.nc(x@file@name)
	zvar = x@data@zvar
	start = c(col, row, time)
	count = c(ncols, nrows, ntimes)
	d <- var.get.nc(nc, variable=zvar, start=start, count=count)
	close.nc(nc)
	
	if (!is.na(x@file@nodatavalue)) { 
		d[d==x@file@nodatavalue] <- NA
	}
	d <- x@data@add_offset + d * x@data@scale_factor
	
	values <- matrix(nrow=nrows*ncols, ncol=ntimes)
	
	# top to bottom ??
	
	if (length(dim(d)) == 3) {
		for (i in 1:ntimes) {
			values[,i] <- as.vector(d[,,i])
		}
		
	} else {
		for (i in 1:ntimes) {
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
