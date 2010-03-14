# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  May 2009
# Version 0.9
# Licence GPL v3

.defaultExtension <- function(format=.filetype()) {
	if (format == 'raster') { return('.grd') 
	} else if (format == 'ascii') { return('.asc')
	} else if (format == 'netcdf') { return('.nc')
	} else if (format == 'GTiff') { return('.tif') 
	} else if (format == 'HFA') { return( '.img') 
	} else if (format == 'ENVI') { return('.envi')
	} else if (format == 'ERS') { return('.ers') 
	} else if (format == 'RST') { return('.rst') 
	} else if (format == 'EHdr') { return('.bil')
	} else if (format == 'ILWIS') { return('.mpr')
	} else if (format == 'SAGA') { return('.sdat')
	} else if (format == 'RMF') { return('.rsw')
	} else { return('') }
}


.getExtension <- function(f, format) {
	def <- .defaultExtension(format)
	extension <- tolower(ext(f))
	if (def != '') {
		ext(f) <- def
	}
	return(f)
}

