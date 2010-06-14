# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2009 / revised June 2010
# Version 1.0
# Licence GPL v3


.stackCDF <- function(filename, x, y, varname, time) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package first') }

	nc <- open.nc(filename)

	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
	zvar <- .getVarname(varname, vars) 
	xvar <- .getxvar(x, vars) 
	yvar <- .getyvar(y, vars) 

	varinfo <- try(var.inq.nc(nc, zvar))
	dims <- varinfo$ndims
	close.nc(nc)
	
	if (dims== 1) { 
		stop('zvar only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims > 3) { 
		stop('zvar has ', length(dims), ' dimensions, I do not know how to process these data')
	} else if (dims == 2) {
		return( stack ( raster(filename, x=xvar, y=yvar, varname=zvar, layer=time )  )  )
	} else {
		if (time == '') {
			time = 1:(as.integer(dim.inq.nc(nc, var.inq.nc(nc, zvar)$dimids[3])$length))
		}
		st = stack( raster(filename, x=xvar, y=yvar, varname=zvar, layer=time[1]) )
		if (length(time) > 1) {
			for (i in 2:length(time)) {
				st <- addLayer(st, raster(filename, xvar=x, yvar=y, zvar=varname, layer=time[i]) )
			}
		}
		return( st )
	}
}
	
