# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2009 / revised June 2010
# Version 1.0
# Licence GPL v3


.stackCDF <- function(filename, x='', y='', varname='', band='') {

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
	
	if (dims== 1) { 
		close.nc(nc)
		stop('zvar only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims > 3) { 
		close.nc(nc)
		stop(zvar, 'has ', length(dims), ' dimensions, I do not know how to process these data')
	} else if (dims == 2) {
		close.nc(nc)
		return( stack ( raster(filename, x=xvar, y=yvar, varname=zvar )  )  )
	} else {
		if (band[1] == '') {
			band = 1:(as.integer(dim.inq.nc(nc, var.inq.nc(nc, zvar)$dimids[3])$length))
		}
		close.nc(nc)
		st = stack( raster(filename, xvar=x, yvar=y, varname=zvar, band=band[1]) )
		if (length(band) > 1) {
			for (i in 2:length(band)) {
				st <- addLayer(st, raster(filename, xvar=x, yvar=y, varname=zvar, band=band[i]) )
			}
		}
		return( st )
	}
}
	
 #s = .stackCDF(f, varname='uwnd')
