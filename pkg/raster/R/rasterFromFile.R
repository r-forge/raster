# R raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3



.rasterObjectFromFile <- function(x, band=1, objecttype='RasterLayer', native=FALSE, ...) {
	x <- trim(x)
	if (x=='' | x=='.') { # etc? 
		stop('provide a valid filename')
	}
	if (!(require(rgdal))) { native <- TRUE }  
	
	fileext <- toupper(ext(x)) 
	if (native) {
		if ( fileext == ".ASC" ) {
			return ( .rasterFromASCIIFile(x) )
		}
		if ( fileext %in% c(".BIL", ".BIP", ".BSQ")) {
			return ( .rasterFromGenericFile(x, ...) )
		}
		if ( fileext %in% c(".RST", ".RDC") ) {
#  not tested
			return ( .rasterFromIDRISIFile(x) )
		}
		if ( fileext %in% c(".SGRD", ".SDAT") ) {
# barely tested
			return ( .rasterFromSAGAFile(x) )
		}
	}
	
	if ( fileext %in% c(".NC", ".NCDF", ".NETCDF")) {
		return ( .rasterFromCDF(x, objecttype, ...) )
	}
	if ( (! fileext %in% c(".GRD", ".GRI")) & file.exists(x)) {
		return ( .rasterFromGDAL(x, band, objecttype) )
	}
	grifile <- .setFileExtensionValues(x, 'raster')
	grdfile <- .setFileExtensionHeader(x, 'raster')
	if (file.exists( grdfile) ) {
		if (file.exists( grifile)) {
			return ( .rasterFromRasterFile(grdfile, band, objecttype) )
		} else {
			if (.isNetCDF(x)) {
				return ( .rasterFromCDF(x, objecttype, ...) )
			} else  {
				test <- try ( r <- .rasterFromGDAL(x, band, objecttype), silent=TRUE )
				if (class(test) == "try-error") {
					stop("Cannot create RasterLayer object. There is a '.grd' file but no '.gri' file. It does not seem to be a netcdf or surfer6/7 file. What is it?")
				} else {
					return(r)
				}
			}
		}
	} else if (file.exists( grifile)) {
		test <- try ( r <- .rasterFromGDAL(x, band, objecttype), silent=TRUE )
			if (class(test) == "try-error") {
				stop("Cannot create RasterLayer object. There is a '.gri' file but no '.grd' file. What is it?")
			} else {
				return(r)
		}
	}
	stop(paste('file', x, 'does not exist'))
}

