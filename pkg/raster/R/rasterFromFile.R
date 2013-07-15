# R raster package
# Date : September 2009
# Version 1.0
# Licence GPL v3


.rasterObjectFromFile <- function(x, band=1, objecttype='RasterLayer', native=FALSE, silent=TRUE, offset=NULL, ncdf=FALSE, crs=NULL, ...) {
	x <- trim(x)
	if (x=='' | x=='.') { # etc? 
		stop('provide a valid filename')
	}

	# fix for opendap https://r-forge.r-project.org/forum/message.php?msg_id=5015
	start <- tolower(substr(x, 1, 4))
	if (start != 'http' & start != 'ftp') {	
		y <- NULL
		try( y <- normalizePath( x, mustWork=TRUE), silent=TRUE )
		if (! is.null(y)) {
			x <- y
		}
	}
	
	fileext <- toupper(extension(x)) 

	if (fileext %in% c(".GRD", ".GRI")) {
		grifile <- .setFileExtensionValues(x, 'raster')
		grdfile <- .setFileExtensionHeader(x, 'raster')
		if ( file.exists( grdfile) & file.exists( grifile)) {
			return ( .rasterFromRasterFile(grdfile, band=band, objecttype, crs) )
		} 
	}
	
	
	if (! file.exists(x) ) {
		if (extension(x) == '') {
			grifile <- .setFileExtensionValues(x, 'raster')
			grdfile <- .setFileExtensionHeader(x, 'raster')
			if ( file.exists( grdfile) & file.exists( grifile)) {
				return ( .rasterFromRasterFile(grdfile, band=band, objecttype, crs) )
			} else {
				# stop('file: ', x, ' does not exist')
			}
		}
	}

	#if (isTRUE(GMT)) {
	#	return(.rasterObjectFromCDF_GMT(x))
	#}
	if (( fileext %in% c(".NC", ".NCF", ".NC4", ".CDF", ".NCDF", ".NETCDF")) | (isTRUE(ncdf))) {
		return( .rasterObjectFromCDF(x, type=objecttype, band=band, crs, ...) )
	}
	if ( fileext == ".GRD") {
		if (require(ncdf)) {
			if (.isNetCDF(x)) {
				return( .rasterObjectFromCDF(x, type=objecttype, band=band, crs, ...)  )
			}
		}
	}

	if ( fileext == ".BIG" | fileext == ".BRD") {
		return( .rasterFromRasterFile(x, band=band, objecttype, driver='big.matrix', crs) )
	}
	
	if (!is.null(offset)) {
		return( .rasterFromASCIIFile(x, offset, crs) )
	}
	
	if(!native) {
		if (! .requireRgdal(FALSE) )  { 
			native <- TRUE 
		}  
	}
	if (native) {
		if ( fileext == ".ASC" ) {
			return ( .rasterFromASCIIFile(x, crs=crs)  )
		} else if ( fileext %in% c(".BIL", ".BIP", ".BSQ")) {
			return ( .rasterFromGenericFile(x, type=objecttype, crs=crs, ...) )
		} else {
			if ( fileext %in% c(".RST", ".RDC") ) {
#  not tested much
				return( .rasterFromIDRISIFile(x, crs=crs))
			}
			if ( fileext %in% c(".SGRD", ".SDAT") ) {
# barely tested
				return( .rasterFromSAGAFile(x, crs=crs))
			}
		} 
	
	}
	
	if ( fileext %in% c(".SGRD", ".SDAT") ) {
		r <-  .rasterFromSAGAFile(x, crs=crs) 
		if (r@file@toptobottom | r@data@gain != 1) {
			return(r)
		} # else use gdal
	}

	if (! .requireRgdal(FALSE) ) {
		stop("Cannot create RasterLayer object from this file; perhaps you need to install rgdal first")
	}
	test <- try( r <- .rasterFromGDAL(x, band=band, objecttype, crs=crs, ...), silent=silent )
	if (class(test) == "try-error") {
		if (!file.exists(x)) {
			stop("Cannot create a RasterLayer object from this file. (file does not exist)")
		}
		stop("Cannot create a RasterLayer object from this file.")
	} else {
		return(r)
	}
}

