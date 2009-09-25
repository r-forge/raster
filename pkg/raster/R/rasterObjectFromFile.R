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


.rasterObjectFromFile <- function(x, band=1, proj='', objecttype='RasterLayer', ...) {
	fileext <- toupper(ext(x)) 
	if ( fileext == ".GRD" | fileext == ".GRI" | fileext == "" ) {
		if (fileext == "" & file.exists(x)) {
			r <- .rasterFromGDAL(x, band, objecttype) 
		} else {
			grifile <- .setFileExtensionValues(x)
			grdfile <- .setFileExtensionHeader(x)
			if (file.exists( grdfile) ) {
				if (file.exists( grifile)) {
					if (fileext != '.grd') { ext(x) <- '.grd' }
					r <- .rasterFromRasterFile(x, band, objecttype) 
				} else {
					if (fileext == ".GRD" ) {
						if (.isNetCDF(x)) {
							r <- .rasterCDF(x, objecttype, ...) 
						} else {
						# TODO check if this is a valid rater .grd but the problem is that the .gri is missing?
						# perhaps a surfer grid...
							r <- .rasterFromGDAL(x, band, objecttype) 
						}
					} else {
					# what would this be? A gri, but no grd. 
						r <- .rasterFromGDAL(x, band, objecttype) 
							#stop('unknown file type; .gri file found but .grd is missing')
					}
				}
			} else {
				r <- .rasterFromGDAL(x, band, objecttype) 
			}
		}
	} else if (file.exists( x )){
	    if (fileext == '.NC') {
			r <- .rasterCDF(x, objecttype, ...) 
		} else {
			r <- .rasterFromGDAL(x, band, objecttype) 
		}
	} else {
		stop(paste('file', x, 'does not exist'))
	}
	if (!is.null(proj)) {
		projection(r) <- proj
	}
	
	return(r)
}
