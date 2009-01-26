# R classes for spatial data (raster data specifically) 
# Authors: Robert J. Hijmans and Jacob van Etten, 
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.8
# Licence GPL v3


setClass('BoundingBox',
	representation (
		xmin = 'numeric',
		xmax = 'numeric',
		ymin = 'numeric',
		ymax = 'numeric'
	),	
	prototype (	
		xmin = 0,
		xmax = 1,
		ymin = 0,
		ymax = 1
	),
	validity = function(object)	{
		c1 <- (object@xmin <= object@xmax)
		c2 <- (object@ymin <= object@ymax)
		return(c1 & c2)
	}
)


setClass ('BasicRaster',
	representation (
		bbox = 'BoundingBox',
		ncols ='integer',
		nrows ='integer',
		crs = 'CRS'
	),
	prototype (	
		ncols= as.integer(1),
		nrows= as.integer(1),
		crs = CRS(as.character(NA))
	),
	validity = function(object) {
		c1 <- (object@ncols > 0)
		c2 <- (object@nrows > 0)
		return(c1 & c2)
	}
)

setClass ('Raster', contains = c('BasicRaster', 'VIRTUAL') )

	
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
	validity = function(object) {
	}
)	


setClass('SingleLayerData', 
	representation (
		values='vector', 
		content='character', #nodata, all, row, block, sparse
		indices = 'vector',
		colname = 'character',
		haveminmax = 'logical',
		min = 'vector',
		max = 'vector',
		source='character' # ram, disk
		),
	prototype (	
		values=vector(),
		content='nodata', 
		indices = vector(mode='numeric'),
		colname = '',
		haveminmax = FALSE,
		min = c(Inf),
		max = c(-Inf),
		source='ram'
	),	
	validity = function(object) {
	}
)

	
setClass ('RasterLayer',
	contains = 'Raster',
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
	

setClass('MultipleRasterData', 
	representation (
		values='matrix', 
		content='character', #nodata, all, row, block, sparse
		indices = 'vector',
		colnames = 'vector',
		nlayers='integer',
		haveminmax = 'logical',
		min = 'vector',
		max = 'vector'
		),
	prototype (	
		values=matrix(NA,0,0),
		content='nodata', 
		indices =vector(mode='numeric'),
		colnames =vector(mode='character'),
		nlayers=as.integer(0),
		haveminmax = FALSE,
		min = c(Inf),
		max = c(-Inf)
	),	
	validity = function(object) {
	}
)



	
setClass ('RasterStack',
	contains = 'Raster',
	representation (
	    filename ='character',
		layers ='list',
		data = 'MultipleRasterData'	
		),
	prototype (
		filename='',
		layers = list()
		),
	validity = function(object) {
		cond1 <- length(object@layers) == object@data@nlayers
		cond <- cond1
		return(cond)
	}
)

