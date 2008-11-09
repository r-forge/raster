

setAs('RasterLayer', 'SpatialGridDataFrame', 
	function(from){ return(as.raster (from)) }
)

setAs('SpatialGridDataFrame', 'RasterLayer', 
	function(from){ return(as.spgrid (from)) }
)


setMethod('dim', signature(x='AbstractRaster'), 
	function(x){ return(c(nrow(x), ncol(x)))}
)

setMethod('dim', signature(x='RasterStack'), 
	function(x){ return(c(nrow(x), ncol(x), nlayers(x)))}
)

setMethod('dim', signature(x='RasterBrick'), 
	function(x){ return(c(nrow(x), ncol(x), nlayers(x)))}
)

setMethod('nrow', signature(x='AbstractRaster'), 
	function(x){ return(x@nrows)}
)

setMethod('ncol', signature(x='AbstractRaster'), 
	function(x){ return(x@ncols) }
)


if (!isGeneric("read.all")) {
	setGeneric("read.all", function(object)
		standardGeneric("read.all"))
}	
setMethod('read.all', signature(object='RasterLayer'), 
	function(object){ return(.raster.read.all(object))}
)
setMethod('read.all', signature(object='RasterStack'), 
	function(object){ return(.rasterstack.read.all(object))}
)


if (!isGeneric("read.row")) {
	setGeneric("read.row", function(object, rownr)
		standardGeneric("read.row"))
}
setMethod('read.row', signature(object='RasterLayer'), 
	function(object, rownr){ return(.raster.read.row(object, rownr))}
)
setMethod('read.row', signature(object='RasterStack'), 
	function(object, rownr){ return(.rasterstack.read.row(object, rownr))}
)

	
if (!isGeneric("read.rows")) {
	setGeneric("read.rows", function(object, startrow, nrows=3)
		standardGeneric("read.rows"))
}	

setMethod('read.rows', signature(object='RasterLayer'), 
	function(object, startrow, nrows=3) { 
		return(.raster.read.rows(object, startrow, nrows))}
)		


if (!isGeneric("read.block")) {
	setGeneric("read.block", function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1))
		standardGeneric("read.block"))
}	

setMethod('read.block', signature(object='RasterLayer'), 
	function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return(.raster.read.block(object, startrow, nrows, ncolumns))}
)

if (!isGeneric("read.part.of.row")) {
	setGeneric("read.part.of.row", function(object, rownr, startcol=1, ncolumns=(ncol(object)-startcol+1))
		standardGeneric("read.part.of.row"))
}	

setMethod('read.part.of.row', signature(object='RasterLayer'), 
	function(object, rownr, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return(.raster.read.part.of.row(object, rownr, startcol, ncolumns))}
)

setMethod('read.part.of.row', signature(object='RasterStack'), 
	function(object, rownr, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return(.rasterstack.read.part.of.row(object, rownr, startcol, ncolumns))}
)

if (!isGeneric("extract.cells")) {
	setGeneric("extract.cells", function(object, cells)
		standardGeneric("extract.cells"))
}	
	
setMethod('extract.cells', signature(object='RasterLayer'), 
	function(object, cells) { 
		return(.raster.read.cells(object, cells))}
)

setMethod('extract.cells', signature(object='RasterStack'), 
	function(object, cells) { 
		return(.rasterstack.read.cells(object, cells))}
)

if (!isGeneric("extract.xy")) {
	setGeneric("extract.xy", function(object, xy)
		standardGeneric("extract.xy"))
}	
	
setMethod('extract.xy', signature(object='RasterLayer'), 
	function(object, xy) { 
		return(.raster.read.xy(object, xy))}
)

setMethod('extract.xy', signature(object='RasterStack'), 
	function(object, xy) { 
		return(.rasterstack.read.xy(object, xy))}
)


setMethod('hist', signature(x='RasterLayer'), 
	function(x, ...){.hist.raster(x, ...)}
)

.hist.raster <- function(x, ...) {
	if (data.content(x) != 'all') {
		if (data.source(x) == 'disk') {
		# also make a function that does this by block and combines  all data into a single histogram
			x <- .read.skip(x, 1000) 
		} else { stop('cannot do; there should be data that is either on disk or in memory')}
	}
	hist(values(x), ...)
}

