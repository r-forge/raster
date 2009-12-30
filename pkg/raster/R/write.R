# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric('writeStart')) {
	setGeneric('writeStart', function(x, filename, ...)
		standardGeneric('writeStart')) 
}  

if (!isGeneric('writeStop')) {
	setGeneric('writeStop', function(x)
		standardGeneric('writeStop')) 
}  
	
if (!isGeneric('writeValues')) {
	setGeneric('writeValues', function(x, v)
		standardGeneric('writeValues')) 
}  
	

setMethod('writeStart', signature(x='Raster', filename='character'), 
function(x, filename, ...) {
	.startRowWriting(x, filename, ...)
})

setMethod('writeStop', signature(x='Raster'), 
function(x) {
	.stopRowWriting(x)
})


setMethod('writeValues', signature(x='Raster'), 
	function(x, v) {
		v[is.infinite(v)] <- NA
		if (x@file@dtype == "INT" || x@file@dtype =='LOG' ) { 
			v <- as.integer(round(v))  
			v[is.na(v)] <- as.integer(x@file@nodatavalue)		
		} else { 
			v  <- as.numeric( v ) 
		}
		rsd <- na.omit(v) # min and max values
		if (length(rsd) > 0) {
			x@data@min <- min(x@data@min, rsd)
			x@data@max <- max(x@data@max, rsd)
		}	
		writeBin(v, x@file@con, size=x@file@dsize )
		return(x)	
	}	
)


setMethod('writeValues', signature(x='RasterBrick'), 
	function(x, v) {
		v[is.infinite(v)] <- NA
		if (x@file@dtype == "INT" || x@file@dtype =='LOG' ) { 
			v <- as.integer(round(v))  
			v[is.na(v)] <- as.integer(x@file@nodatavalue)		
		} else { 
			v  <- as.numeric( v ) 
		}
		writeBin(v, x@file@con, size=x@file@dsize )
		return(x)	
	}	
)

