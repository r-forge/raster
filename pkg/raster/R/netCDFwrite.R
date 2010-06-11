# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.getRasterDTypeFromCDF <- function(type) { 
	if (type == "NC_CHAR" )  { return("INT1U") 
	} else if (type == "NC_BYTE" ) { return("INT1S")
	} else if (type == "NC_SHORT" ) { return("INT2S")
	} else if (type == "NC_INT" ) { return("INT4S")
	} else if (type == "NC_FLOAT" ) { return("FLT4S")
	} else if (type =="NC_DOUBLE" ) { return("FLT8S") 
	} else { return("FLT4S") }
}


.getNetCDFDType <- function(dtype) {
	if (!(dtype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S'))) {
		stop('not a valid data type')
	}
	type <- .shortDataType(dtype)
	size <- dataSize(dtype) * 8
	signed <- dataSigned(dtype)
	
	if (size == 8) {
		if (!signed) {
			return("NC_CHAR") #8-bit characters intended for representing text.
		} else {
			return("NC_BYTE")
		}
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


.brickSaveAsNetCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE) {
	stop('not yet implemented')
}


.rasterSaveAsNetCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE) {
	x <- .startWriteCDF(x, filename=filename, datatype=datatype, overwrite=overwrite)
	x <- .writeValuesCDF(x, getValues(x))
	return( .stopWriteCDF(x) )
}


.startWriteCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package') }

	filename = trim(filename)
	if (filename == '') { stop('provide a filename') }
	ext(filename) <- .defaultExtension(format='netcdf')
	if (file.exists(filename) & !overwrite) {
		stop('file exists, use overwrite=TRUE to overwrite it')
	}
	
	dataType(x) <- datatype
	
	datatype = .getNetCDFDType(datatype)
	
	if (.couldBeLonLat(x)) {
		xname = 'longitude'
		yname = 'latitude'
		unit = 'degrees'
	} else {
		xname = 'northing'
		yname = 'easting'	
		unit = 'meter' # probably
	}
	
	nc <- create.nc(filename)	
	
	dim.def.nc(nc, 'x', ncol(x) )
	var.def.nc(nc, 'x', "NC_DOUBLE", 0)
	att.put.nc(nc, 'x', 'long_name', 'NC_CHAR', xname)
	att.put.nc(nc, 'x', 'standard_name', 'NC_CHAR', xname)
	att.put.nc(nc, 'x', 'axis', 'NC_CHAR', 'X')
	att.put.nc(nc, 'x', 'units', 'NC_CHAR', unit)

	
	dim.def.nc(nc, 'y', nrow(x) ) 
	var.def.nc(nc, 'y', "NC_DOUBLE", 1)
	att.put.nc(nc, 'y', 'long_name', 'NC_CHAR', yname)
	att.put.nc(nc, 'y', 'standard_name', 'NC_CHAR', yname)
	att.put.nc(nc, 'y', 'axis', 'NC_CHAR', 'Y')
	att.put.nc(nc, 'y', 'units', 'NC_CHAR', unit)

	var.def.nc(nc, 'value', datatype, c(0,1))
	att.put.nc(nc, 'value', 'missing_value', datatype, x@file@nodatavalue)
	
#	dim.def.nc(nc, 'z', nlayers(x), unlim=TRUE)
#	var.def.nc(nc, 'z', "NC_INT", 'z')
#	var.def.nc(nc, 'value', datatype, c('x', 'y', 'z'))

	var.put.nc(nc, 'x', xFromCol(x, 1:ncol(x)), start=NA, count=NA, na.mode=0)
	var.put.nc(nc, 'y', yFromRow(x, 1:nrow(x)), start=NA, count=NA, na.mode=0)
#	var.put.nc(nc, 'z', 1:nlayers(x), start=NA, count=NA, na.mode=0)
	att.put.nc(nc, 'value', 'long_name', 'NC_CHAR', layerNames(x))

	pkgversion = drop(read.dcf(file=system.file("DESCRIPTION", package='raster'), fields=c("Version")))
	att.put.nc(nc, "NC_GLOBAL", 'created_by', 'NC_CHAR', paste('R, raster package, version', pkgversion))
	att.put.nc(nc, "NC_GLOBAL", 'date', 'NC_CHAR', format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

	close.nc(nc)
	
	x@data@min <- rep(Inf, nlayers(x))
	x@data@max <- rep(-Inf, nlayers(x))
	x@data@haveminmax <- FALSE
	x@file@driver <- 'netcdf'
	x@file@name <- filename
	
	return(x)
}


.stopWriteCDF <-  function(x) {
	nc <- open.nc(x@file@name, write=TRUE)
	att.put.nc(nc, 'value', 'min', 'NC_DOUBLE', as.numeric(x@data@min))
	att.put.nc(nc, 'value', 'max', 'NC_DOUBLE', as.numeric(x@data@max))
	close.nc(nc)
	r <- raster(x@file@name, zvar='value')
	return(r)
}




.writeValuesCDF <- function(x, v, start=1) {

	nc <- open.nc(x@file@name, write=TRUE)
	
	v[is.na(v)] = x@file@nodatavalue
	v <- matrix(v, ncol=x@nrows)
	
	var.put.nc(nc, 'value', v, start=c(1, start), count=NA, na.mode=0)
	close.nc(nc)

#	var.put.nc(nc, 'value', v, start=c(1,1,1), count=c(ncol(x),nrow(x),nlayers(x)), na.mode=0)
	
	rsd <- na.omit(x@data@values) # min and max values
	if (length(rsd) > 0) {
		x@data@min <- min(x@data@min, rsd)
		x@data@max <- max(x@data@max, rsd)
	}	
	
	return(x)
}


#library(raster)
#r = raster(ncol=10, nrow=5)
#r[] = c(1:49, NA)
#layerNames(r) = 'hello world'
#a = .rasterSaveAsNetCDF(r, 'test.nc', overwrite=TRUE)
#plot(a)
#print(a)

