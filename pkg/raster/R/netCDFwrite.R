# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.saveAsNetCDF <- function(r, filename) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package first') }

	varname = layerNames(r)
	if (trim(varname) == '') {
		varname <- 'value'
	}
	if (.couldBeLonLat(r)) {
		xvar = 'longitude'
		yvar = 'latitude'
	} else {
		xvar = 'northing'
		yvar = 'easting'	
	}
	
	nc <- create.nc(filename)	
	
	dim.def.nc(nc, xvar, ncol(r) )
	dim.def.nc(nc, yvar, nrow(r) ) 
#	t <- dim.def.nc(nc, "time", "days since 1900-01-01", 2, unlim=FALSE)

	var.def.nc(nc, xvar, "NC_DOUBLE", xvar)
	var.def.nc(nc, yvar, "NC_DOUBLE", yvar)
	var.def.nc(nc, varname, "NC_DOUBLE", c(xvar, yvar))

	var.put.nc(nc, xvar, xFromCol(r, 1:ncol(r)), start=NA, count=NA, na.mode=0)
	var.put.nc(nc, yvar, yFromRow(r, 1:nrow(r)), start=NA, count=NA, na.mode=0)
	var.put.nc(nc, varname, t(values(r, format='matrix')), start=NA, count=NA, na.mode=0)

	close.nc(nc)
	
	r <- raster(filename, zvar=varname)
	return(r)
}

#r = raster(ncol=10, nrow=10)
#r[] = 1:100
#a = saveAsNetCDF(r, 'test.nc')
#plot(a)


