# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.getNetCDFDType <- function(dtype, format='') {
	if (!(dtype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S'))) {
		stop('not a valid data type')
	}
	type <- .shortDataType(dtype)
	size <- dataSize(dtype) * 8
	signed <- dataSigned(dtype)
	#NC_CHAR	 8-bit characters intended for representing text.
	if (size == 8) { return("NC_BYTE")
	} else if (type == 'INT') {
		if (!signed) {
			warning('netcdf only stores signed integers')
		}
		if (size == 16) { return( "NC_SHORT" ) 
		} else { return( "NC_INT" ) }
	} else {
		if (size == 32) { return( "NC_FLOAT" ) 
		} else {  return ( "NC_DOUBLE" )  }
	}
}


.saveAsNetCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package first') }

	filename = trim(filename)
	if (filename == '') { stop('provide a filename') }
	ext(filename) <- .defaultExtension(format='netcdf')
	if (file.exists(filename) & !overwrite) {
		stop('file exists, use overwrite=TRUE to overwrite it')
	}
	
	datatype = .getNetCDFDType(datatype)
	
	varname = layerNames(x)
	if (trim(varname) == '') {
		varname <- 'value'
	}
	if (.couldBeLonLat(x)) {
		xvar = 'longitude'
		yvar = 'latitude'
	} else {
		xvar = 'northing'
		yvar = 'easting'	
	}
	tvar = 'layer'
	
	nc <- create.nc(filename)	
	
	dim.def.nc(nc, xvar, ncol(x) )
	var.def.nc(nc, xvar, "NC_DOUBLE", xvar)
	dim.def.nc(nc, yvar, nrow(x) ) 
	var.def.nc(nc, yvar, "NC_DOUBLE", yvar)

#	dim.def.nc(nc, tvar, nlayers(x), unlim=TRUE)
#	var.def.nc(nc, tvar, "NC_INT", tvar)
#	var.def.nc(nc, varname, datatype, c(xvar, yvar, tvar))
	var.def.nc(nc, varname, datatype, c(xvar, yvar))

	var.put.nc(nc, xvar, xFromCol(x, 1:ncol(x)), start=NA, count=NA, na.mode=0)
	var.put.nc(nc, yvar, yFromRow(x, 1:nrow(x)), start=NA, count=NA, na.mode=0)
	var.put.nc(nc, varname, t(values(x, format='matrix')), start=NA, count=NA, na.mode=0)
#	var.put.nc(nc, tvar, 1:nlayers(x), start=NA, count=NA, na.mode=0)
#	var.put.nc(nc, varname, t(values(x, format='matrix')), start=c(1,1,1), count=c(ncol(x),nrow(x),nlayers(x)), na.mode=0)

	close.nc(nc)
	
	r <- raster(filename, zvar=varname)
	return(r)
}

#library(raster)
#r = raster(ncol=10, nrow=10)
#r[] = 1:100
#a = raster:::.saveAsNetCDF(r, 'test.nc', overwrite=TRUE)
#plot(a)


