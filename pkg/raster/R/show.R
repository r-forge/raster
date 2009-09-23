# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3



setMethod ('show' , 'Extent', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('xmin        :' , xmin(object), '\n')
		cat('xmax        :' , xmax(object), '\n')
		cat('ymin        :' , ymin(object), '\n')
		cat('ymax        :' , ymax(object), '\n')
	}
)	
	

setMethod ('show' , 'BasicRaster', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('nrow        :' , nrow(object), '\n')
		cat('ncol        :' , ncol(object), '\n')
		cat('ncells      :' , ncell(object), '\n')
		cat('projection  :' , projection(object, TRUE), '\n')
		cat('xmin        :' , xmin(object), '\n')
		cat('xmax        :' , xmax(object), '\n')
		cat('ymin        :' , ymin(object), '\n')
		cat('ymax        :' , ymax(object), '\n')
		cat('xres        :' , xres(object), '\n')
		cat('yres        :' , yres(object), '\n')
	}
)	
	
setMethod ('show' , 'RasterLayer', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('filename    :' , filename(object), '\n')
		if (nbands(object) > 1) { cat('band        :' , band(object), '\n')	}	
		cat('nrow        :' , nrow(object), '\n')
		cat('ncol        :' , ncol(object), '\n')
		cat('ncells      :' , ncell(object), '\n')
#		cat('data type   :' , object@file@datanotation, '\n')
#		if (dataContent(object) == 'nodata') { cat('vals in mem : none', '\n')
#		} else { cat('vals in mem :', dataContent(object) , '\n') }
						
		if (object@data@haveminmax) {
			cat('min value   :' , minValue(object), '\n')
			cat('max value   :' , maxValue(object), '\n')
		} else { 
			if (object@data@source == 'disk')  {
				cat('min value   : ? \n')
				cat('max value   : ? \n')
			} else {
				cat('min value   :  \n')
				cat('max value   :  \n')		
			}
		}
		cat('projection  :' , projection(object, TRUE), '\n')
		cat('xmin        :' , xmin(object), '\n')
		cat('xmax        :' , xmax(object), '\n')
		cat('ymin        :' , ymin(object), '\n')
		cat('ymax        :' , ymax(object), '\n')
		cat('xres        :' , xres(object), '\n')
		cat('yres        :' , yres(object), '\n')
		cat ('\n')
	}
)


setMethod ('show' , 'RasterBrick',
	function ( object ) {
		cat ('class       :' , class ( object ) , '\n')
		cat ('filename    :' , filename(object), '\n')
		cat ('nlayers     :' , nlayers(object), '\n')
		cat ('nrow        :' , nrow(object), '\n')
		cat ('ncol        :' , ncol(object), '\n')
		cat ('ncells      :' , ncell(object), '\n')
		cat ('projection  :' , projection(object, TRUE), '\n')
#		if (dataContent(object) == 'nodata') { cat('vals in mem : none', '\n')
#		} else { cat('vals in mem :', dataContent(object) , '\n') }
		if (object@data@haveminmax) {
			cat('min value   :', paste(minValue(object, -1), collapse=' '), '\n')
			cat('max value   :', paste(maxValue(object, -1), collapse=' '), '\n')
		} else { 
			if (object@data@source == 'disk')  {
				cat('min value   : ? \n')
				cat('max value   : ? \n')
			} else {
				cat('min value   :  \n')
				cat('max value   :  \n')		
			}
		}
		cat ('xmin        :' , xmin(object), '\n')
		cat ('xmax        :' , xmax(object), '\n')
		cat ('ymin        :' , ymin(object), '\n')
		cat ('ymax        :' , ymax(object), '\n')
		cat ('xres        :' , xres(object) , '\n')
		cat ('yres        :' , yres(object) , '\n')
		cat ('\n')
	}
)



setMethod ('show' , 'RasterStack',
	function ( object ) {
		cat ('class       :' , class ( object ) , '\n')
		cat ('filename    :' , filename(object), '\n')
		cat ('nlayers     :' , nlayers(object), '\n')
		if (nlayers(object) > 0) {
			cat ('nrow        :' , nrow(object@layers[[1]]), '\n')
			cat ('ncol        :' , ncol(object@layers[[1]]), '\n')
			cat ('ncells      :' , ncell(object@layers[[1]]), '\n')
			cat ('projection  :' , projection(object@layers[[1]], TRUE), '\n')
			minv <- list()
			maxv <- list()
			for (i in 1:nlayers(object)) {
				if (object@layers[[i]]@data@haveminmax) {
					minv[i] <- minValue(object@layers[[i]])
					maxv[i] <- maxValue(object@layers[[i]])
				} else {
					minv[i] <- '?'
					maxv[i] <- '?'
				}
			}
			cat('min value   :', paste(minv, collapse=' '), '\n')
			cat('max value   :', paste(maxv, collapse=' '), '\n')
		}
		cat ('xmin        :' , xmin(object), '\n')
		cat ('xmax        :' , xmax(object), '\n')
		cat ('ymin        :' , ymin(object), '\n')
		cat ('ymax        :' , ymax(object), '\n')
		cat ('xres        :' , xres(object), '\n')
		cat ('yres        :' , yres(object), '\n')
		cat ('\n')
	}
)

