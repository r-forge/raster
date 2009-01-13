# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.8
# Licence GPL v3


###   readAll   ###

if (!isGeneric("readAll")) {
	setGeneric("readAll", function(object)
		standardGeneric("readAll"))
}	
setMethod('readAll', signature(object='RasterLayer'), 
	function(object){ return(.rasterRead(object, -1))}
)
setMethod('readAll', signature(object='RasterStack'), 
	function(object){ return(.stackRead(object, -1))}
)


###   readRow   ###

if (!isGeneric("readRow")) {
	setGeneric("readRow", function(object, rownr)
		standardGeneric("readRow"))
}
setMethod('readRow', signature(object='RasterLayer'), 
	function(object, rownr){ return(.rasterRead(object, rownr))}
)
setMethod('readRow', signature(object='RasterStack'), 
	function(object, rownr){ return(.stackRead(object, rownr))}
)


###   readRows   ###
	
if (!isGeneric("readRows")) {
	setGeneric("readRows", function(object, startrow, nrows=3)
		standardGeneric("readRows"))
}	

setMethod('readRows', signature(object='RasterLayer'), 
	function(object, startrow, nrows=3) { 
		#read multiple rows
		return(.rasterReadBlock(object, startrow, nrows))
	}	
)

		
###   readBlock   ###		

if (!isGeneric("readBlock")) {
	setGeneric("readBlock", function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1))
		standardGeneric("readBlock"))
}	

setMethod('readBlock', signature(object='RasterLayer'), 
	function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return(.rasterReadBlock(object, startrow, nrows, ncolumns))}
)


###   readPartOfRow   ###

if (!isGeneric("readPartOfRow")) {
	setGeneric("readPartOfRow", function(object, rownr, startcol=1, ncolumns=(ncol(object)-startcol+1))
		standardGeneric("readPartOfRow"))
}	

setMethod('readPartOfRow', signature(object='RasterLayer'), 
	function(object, rownr, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return(.rasterRead(object, rownr, startcol, ncolumns))}
)

setMethod('readPartOfRow', signature(object='RasterStack'), 
	function(object, rownr, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return( .stackRead(object, rownr, startcol, ncolumns) ) }
)


###   cellValues   ###

if (!isGeneric("cellValues")) {
	setGeneric("cellValues", function(object, cells)
		standardGeneric("cellValues"))
}	
	
setMethod("cellValues", signature(object='RasterLayer', cells='vector'), 
	function(object, cells) { 
		return(.rasterReadCells(object, cells))
	}
)


setMethod("cellValues", signature(object='RasterStack', cells='vector'), 
	function(object, cells) { 
		return(.stackReadCells(object, cells))
	}
)


###   xyValues   ###

if (!isGeneric("xyValues")) {
	setGeneric("xyValues", function(object, xyCoords)
		standardGeneric("xyValues"))
}	
	
setMethod("xyValues", signature(object='RasterLayer', xyCoords='matrix'), 
	function(object, xyCoords) { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; there should be 2 columns only' )
		}
		cells <- cellFromXY(object, xyCoords)
		return(.rasterReadCells(object, cells))
	}	
)	


setMethod("xyValues", signature(object='RasterStack', xyCoords='matrix'), 
	function(object, xyCoords) { 
		if (dim(xyCoords)[2] != 2) {
			stop('xyCoords has wrong dimensions; there should be 2 columns only' )
		}
		cells <- cellFromXY(object, xyCoords)
		return(.stackReadCells(object, cells))
	}	
)



setMethod("xyValues", signature(object='RasterStack', xyCoords='SpatialPoints'), 
	function(object, xyCoords) { 
		xyCoords <- coordinates(xyCoords)
		cells <- cellFromXY(object, xyCoords)
		return(.stackReadCells(object, cells))
	}	
)


setMethod("xyValues", signature(object='RasterStack', xyCoords='SpatialPoints'), 
	function(object, xyCoords) { 
		xyCoords <- coordinates(xyCoords)
		cells <- cellFromXY(object, xyCoords)
		return(.stackReadCells(object, cells))
	}	
)



