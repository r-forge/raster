

# Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3

makeSparse <- function(raster) {
	if ( dataContent(raster) == 'sparse') {return(raster)
	} else {
		if ( dataContent(raster) == 'all') {
			vals <- seq(1:ncell(raster))
			vals <- cbind(vals, values(raster))
			vals <- na.omit(vals)
			raster <- setValuesSparse(raster, sparsevalues=vals[,2], cellnumbers=vals[,1])
			return(raster)
		} else { 
			# as above, but by reading data from disk, row by row
			stop('not implemented yet for objects with no data in memory, use readAll() first' )
		}	
	}
}
