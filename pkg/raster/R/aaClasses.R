# R classes for raster (grid) type spatial data
# Robert J. Hijmans, r.hijmans@gmail.com
# November 2008
# Version 0.9
# Licence GPL v3


setClass('Extent',
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
		if (!c1) { stop('xmin > xmax') }
		c2 <- (object@ymin <= object@ymax)
		if (!c2) { stop('ymin > ymax') }
		v <- c(object@xmin, object@xmax, object@ymin, object@ymax)
		c3 <- all(!is.infinite(v))
		if (!c3) { stop('infinite in Extent') }		
		return(c1 & c2 & c3)
	}
)


setClass ('BasicRaster',
	representation (
		extent = 'Extent',
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
		validObject(extent(object))
		c1 <- (object@ncols > 0)
		if (!c1) { stop('ncols < 1') }
		c2 <- (object@nrows > 0)
		if (!c2) { stop('nrows < 1') }		
		return(c1 & c2)
	}
)

setClass ('Raster', contains = c('BasicRaster', 'VIRTUAL') )

	
setClass('RasterFile', 
	representation (
		name ='character',
		shortname ='character', # short name
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
		datanotation='FLT4S',
		byteorder = .Platform$endian,
		nodatavalue = -3.4E38,
		nbands = as.integer(1),
		band = as.integer(1),
		bandorder = 'BIL'
	),
	validity = function(object) {
		c1 <- datanotation %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'INT4U', 'INT8U', 'FLT4S', 'FLT8S')
		return(c1)
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



setClass ('RasterLegend',
	representation (
		type = 'character',
		begin = 'vector',
		end = 'vector',
		color = 'vector'
		),
	prototype (
		)
	)
	

	
setClass ('RasterLayer',
	contains = 'Raster',
	representation (
		title = 'character',
		file = 'RasterFile',
		data = 'SingleLayerData',
		legend = 'RasterLegend',
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


setClass ('RasterBrick',
	contains = 'Raster',
	representation (
		title = 'character',
		file = 'RasterFile',
		data = 'MultipleRasterData',
		legend = 'RasterLegend',
#		sparse = 'logical',
		history = 'vector'
		),
	prototype (
#		sparse = FALSE,
		history = vector(mode='character')
		),
	validity = function(object)
	{
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

setClassUnion("RasterStackBrick", c("RasterStack", "RasterBrick"))
