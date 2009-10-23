# R raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3


.isNetCDF <- function(x) {
	fcon <- file(x, "rb")
	w <- readBin(fcon, what='character', n=1)
	close(fcon)
	if (substr(w, 1, 3) == "CDF") {
		return(TRUE) 
	} else {
		return(FALSE)
	}
}


.rasterObjectFromFile <- function(x, band=1, objecttype='RasterLayer', forcegdal=FALSE, ...) {
	x <- trim(x)
	if (x=='' | x=='.') { # etc? 
		stop('provide a valid filename')
	}
	fileext <- toupper(ext(x)) 
	if (forcegdal) {
		return( .rasterFromGDAL(x, band, objecttype) )
	} 
	if ( fileext %in% c(".NC", ".NCDF", ".NETCDF")) {
		return ( .rasterFromCDF(x, band, objecttype) )
	}
	if ( fileext %in% c(".RST", ".RDC") ) {
		return ( .rasterFromIDRISIFile(x) )
	}
	if ( fileext %in% c(".SGRD", ".SDAT") ) {
		return ( .rasterFromSAGAFile(x) )
	}
	if ( (! fileext %in% c(".GRD", ".GRI")) & file.exists(x)) {
		return ( .rasterFromGDAL(x, band, objecttype) )
	}
	if ( fileext %in% c(".GRD", ".GRI", "") ) {
		grifile <- .setFileExtensionValues(x, 'raster')
		grdfile <- .setFileExtensionHeader(x, 'raster')
		if (file.exists( grdfile) ) {
			if (file.exists( grifile)) {
				return ( .rasterFromRasterFile(grdfile, band, objecttype) )
			} else {
				if (.isNetCDF(x)) {
					return ( .rasterFromCDF(x, objecttype, ...) )
				} else if ( isTRUE(.isSurferFile(x)) ) {
					return ( .rasterFromSurferFile(x) )
				} else {
					stop("Cannot create RasterLayer object. There is a '.grd' file but no '.gri' file. It is not a netcdf or surfer6 file. You can try again with 'forcegdal=TRUE'")
				} 
			}
		} else {
			stop("Cannot create RasterLayer object. There is a '.gri' file but no '.grd' file. You can try again with 'forcegdal=TRUE'")
		}
	} 
	stop(paste('file', x, 'does not exist'))
}

