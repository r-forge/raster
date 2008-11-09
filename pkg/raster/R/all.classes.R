# R classes for spatial data (raster data specifically) 
# Authors: Robert J. Hijmans and Jacob van Etten, 
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.8
# Licence GPL v3


# the below may be necessary as the function is not imported from SP (it is internal)
# It is to check the bounds of lat/lon values, SP gives an error, I prefer a warning
.ll_sanity <- function(bb) {
	outside <- FALSE
	if (bb[1,1] < -180) {outside <- TRUE }
	if (bb[1,2] > 180) {outside <- TRUE }
	if (bb[2,1] < -90) {outside <- TRUE }
	if (bb[2,2] > 90) {outside <- TRUE }	
	if (outside) { warning('latitude/longitude values are outside their normal range') }
	return(TRUE)
}


#setMethod ('show' , 'Spatial', 
#	function(object) {
#		cat('class     :', class(object), '\n')
#		cat('projection:', projection(object), '\n')
#		boundingbox(object)
#	}
#)	


setClass ('AbstractRaster',
# importing "Spatial" (bounding box + Proj4string) from the sp package
	contains = 'Spatial',
	representation (
		ncols ='integer',
		nrows ='integer'
		),
	validity = function(object)
	{
		c1 <- (object@ncols > 0)
		c2 <- (object@nrows > 0)
		return(c1 & c2)
	}
)
	
	
setClass('RasterFile', 
	representation (
		name ='character',
		shortname ='character', # short name
		driver ='character', #gdal, raster
		gdalhandle='list',
		datatype ='character', #'numeric' or 'integer'
		datasize ='integer',
		datasigned='logical',
		datanotation='character',
		byteorder ='character',
		nodatavalue ='numeric', # on disk, in ram it is NA
		nbands ='integer',
		band = 'integer',
		bandorder ='character'
		),
	prototype (	
	    name = '',
		shortname ='',
		driver = 'raster',
		gdalhandle= list(),
		datatype = 'numeric',
		datasize = as.integer(4),
		datasigned= TRUE,
		datanotation='FLT4S',
		byteorder = .Platform$endian,
		nodatavalue = -9999,
		nbands = as.integer(1),
		band = as.integer(1),
		bandorder = 'BIL'
	),
	validity = function(object)
	{
	}
)	


setClass('SingleLayerData', 
	representation (
		values='vector', 
		content='character', #nodata, all, row, block, sparse
		indices = 'vector',
		haveminmax = 'logical',
		min ='numeric',
		max ='numeric',
		source='character' # ram, disk
		),
	prototype (	
		values=vector(),
		content='nodata', 
		indices =vector(mode='numeric'),
		haveminmax = FALSE,
		min = numeric(1),
		max = numeric(1),
		source='ram'
	),	
	validity = function(object)
	{
	}
)

	
setClass ('RasterLayer',
	contains = 'AbstractRaster',
	representation (
		title = 'character',
		file = 'RasterFile',
		data = 'SingleLayerData',
		history = 'vector'
		),
	prototype (
		history = vector(mode='character')
		)
	)
	
	
setMethod ('show' , 'RasterLayer', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('filename    :' , filename(object), '\n')
		if (object@file@nbands > 1) {
#			cat('nbands      :' , object@file@nbands, '\n')
			cat('band        :' , object@file@band, '\n')
		}	
		cat('nrow        :' , nrow(object), '\n')
		cat('ncol        :' , ncol(object), '\n')
		cat('ncells      :' , ncells(object), '\n')
		cat('data type   :' , object@file@datanotation, '\n')
		cat('data content:' ,  data.content(object), '\n')
		if (object@data@haveminmax) {
			cat('min value   :' , minvalue(object), '\n')
			cat('max value   :' , maxvalue(object), '\n')
		} else { #if (object@data@source == 'disk')  {
			cat('min value   : NA \n')
			cat('max value   : NA \n')
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


setClass('MultipleRasterData', 
	representation (
		values='matrix', 
		content='character', #nodata, all, row, block, sparse
		indices = 'vector',
		varnames = 'character',
		nlayers='integer'
		),
	prototype (	
		values=matrix(NA,0,0),
		content='nodata', 
		indices =vector(mode='numeric'),
		varnames =vector(mode='character'),
		nlayers=as.integer(0)
	),	
	validity = function(object)
	{
	}
)


setClass ('RasterBrick',
	contains = 'AbstractRaster',
	representation (
		data = 'MultipleRasterData',
		title = 'character',
		file = 'RasterFile',
		sparse = 'logical',
		history = 'vector'
		),
	prototype (
		sparse = FALSE,
		history = vector(mode='character')
		),
	validity = function(object)
	{
	}
)
	


setMethod ('show' , 'RasterBrick',
	function ( object ){
		cat ('class     :' , class ( object ) , '\n')
		cat ('filename  :' , object@filename, '\n')
		cat ('nlayers   :' , object@data@nlayers, '\n')
		cat ('nrow      :' , nrow(object), '\n')
		cat ('ncol      :' , ncol(object), '\n')
		cat ('ncells    :' , ncells(object), '\n')
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


	
setClass ('RasterStack',
	contains = 'AbstractRaster',
	representation (
	    filename ='character',
		rasters ='list',
		data = 'MultipleRasterData'	
		),
	prototype (
		filename='',
		rasters = list()
		),
	validity = function(object)
	{
		cond1 <- length(object@rasters) == object@data@nlayers
		#cond2 <- Are the rasters equal in dimensions etc.? The exact implementation will depend on the format of the raster@data slot (list, array, vector)
		cond <- cond1 #& cond2
		return(cond)
	}
)


setMethod ('show' , 'RasterStack',
	function ( object ){
		cat ('class     :' , class ( object ) , '\n')
		cat ('filename  :' , object@filename, '\n')
		cat ('nlayers   :' , object@data@nlayers, '\n')
		cat ('nrow      :' , nrow(object), '\n')
		cat ('ncol      :' , ncol(object), '\n')
		cat ('ncells    :' , ncells(object), '\n')
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

