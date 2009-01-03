

setAs('RasterLayer', 'SpatialGridDataFrame', 
	function(from){ return(asSpGrid (from)) }
)

setAs('SpatialGridDataFrame', 'RasterBrick',
	function(from){ return(asRasterBrick (from)) }
)

setAs('SpatialGridDataFrame', 'RasterLayer', 
	function(from){ return(asRasterLayer (from)) }
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


if (!isGeneric("readAll")) {
	setGeneric("readAll", function(object)
		standardGeneric("readAll"))
}	
setMethod('readAll', signature(object='RasterLayer'), 
	function(object){ return(.raster.read(object, -1))}
)
setMethod('readAll', signature(object='RasterStack'), 
	function(object){ return(.rasterstack.read(object, -1))}
)


if (!isGeneric("readRow")) {
	setGeneric("readRow", function(object, rownr)
		standardGeneric("readRow"))
}
setMethod('readRow', signature(object='RasterLayer'), 
	function(object, rownr){ return(.raster.read(object, rownr))}
)
setMethod('readRow', signature(object='RasterStack'), 
	function(object, rownr){ return(.rasterstack.read(object, rownr))}
)

	
if (!isGeneric("readRows")) {
	setGeneric("readRows", function(object, startrow, nrows=3)
		standardGeneric("readRows"))
}	

setMethod('readRows', signature(object='RasterLayer'), 
	function(object, startrow, nrows=3) { 
		#read multiple rows
		return(.raster.read.block(object, startrow, nrows))
	}	
)

		

if (!isGeneric("readBlock")) {
	setGeneric("readBlock", function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1))
		standardGeneric("readBlock"))
}	

setMethod('readBlock', signature(object='RasterLayer'), 
	function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return(.raster.read.block(object, startrow, nrows, ncolumns))}
)

if (!isGeneric("readPartOfRow")) {
	setGeneric("readPartOfRow", function(object, rownr, startcol=1, ncolumns=(ncol(object)-startcol+1))
		standardGeneric("readPartOfRow"))
}	

setMethod('readPartOfRow', signature(object='RasterLayer'), 
	function(object, rownr, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return(.raster.read(object, rownr, startcol, ncolumns))}
)

setMethod('readPartOfRow', signature(object='RasterStack'), 
	function(object, rownr, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return( .rasterstack.read(object, rownr, startcol, ncolumns) ) }
)

if (!isGeneric("valuesCells")) {
	setGeneric("valuesCells", function(object, cells)
		standardGeneric("valuesCells"))
}	
	
setMethod("valuesCells", signature(object='RasterLayer'), 
	function(object, cells) { 
		return(.raster.read.cells(object, cells))}
)


setMethod("valuesCells", signature(object='RasterStack'), 
	function(object, cells) { 
		return(.rasterstack.read.cells(object, cells))}
)

if (!isGeneric("valuesXY")) {
	setGeneric("valuesXY", function(object, xy)
		standardGeneric("valuesXY"))
}	
	
setMethod("valuesXY", signature(object='RasterLayer'), 
	function(object, xy) { 
		return(.raster.read.xy(object, xy))
	}
)




setMethod("valuesXY", signature(object='RasterStack'), 
	function(object, xy) { 
		return(.rasterstack.read.xy(object, xy))}
)


setMethod('hist', signature(x='RasterLayer'), 
	function(x, ...){
		if (dataContent(x) != 'all') {
			if (dataSource(x) == 'disk') {
		# TO DO: ake a function that does this by block and combines  all data into a single histogram
				x <- .read.skip(x, 1000) 
			} else { stop('cannot make a histogram; there should be data, either on disk or in memory')}
		}
		hist(values(x), ...)
	}	
)

