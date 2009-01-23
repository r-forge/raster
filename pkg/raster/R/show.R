# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3



setMethod ('show' , 'BoundingBox', 
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
		if (nbands(object) > 1) {
			cat('band        :' , band(object), '\n')
		}	
		cat('nrow        :' , nrow(object), '\n')
		cat('ncol        :' , ncol(object), '\n')
		cat('ncells      :' , ncell(object), '\n')
		cat('data type   :' , object@file@datanotation, '\n')
		cat('data content:' ,  dataContent(object), '\n')
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
	function ( object ){
		cat ('class     :' , class ( object ) , '\n')
		cat ('filename  :' , filename(object), '\n')
		cat ('nlayers   :' , nlayers(object), '\n')
		cat ('nrow      :' , nrow(object), '\n')
		cat ('ncol      :' , ncol(object), '\n')
		cat ('ncells    :' , ncell(object), '\n')
		cat ('projection:' , projection(object, TRUE), '\n')
		cat ('xmin      :' , xmin(object), '\n')
		cat ('xmax      :' , xmax(object), '\n')
		cat ('ymin      :' , ymin(object), '\n')
		cat ('ymax      :' , ymax(object), '\n')
		cat ('xres      :' , xres(object) , '\n')
		cat ('yres      :' , yres(object) , '\n')
		cat ('\n')
	}
)


setMethod ('show' , 'RasterStack',
	function ( object ){
		cat ('class     :' , class ( object ) , '\n')
		cat ('filename  :' , filename(object), '\n')
		cat ('nlayers   :' , nlayers(object), '\n')
		cat ('nrow      :' , nrow(object), '\n')
		cat ('ncol      :' , ncol(object), '\n')
		cat ('ncells    :' , ncell(object), '\n')
		cat ('projection:' , projection(object, TRUE), '\n')
		cat ('xmin      :' , xmin(object), '\n')
		cat ('xmax      :' , xmax(object), '\n')
		cat ('ymin      :' , ymin(object), '\n')
		cat ('ymax      :' , ymax(object), '\n')
		cat ('xres      :' , xres(object) , '\n')
		cat ('yres      :' , yres(object) , '\n')
		cat ('\n')
	}
)

