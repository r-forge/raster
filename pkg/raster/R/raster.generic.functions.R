
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

		

if (!isGeneric("readBlock")) {
	setGeneric("readBlock", function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1))
		standardGeneric("readBlock"))
}	

setMethod('readBlock', signature(object='RasterLayer'), 
	function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		return(.rasterReadBlock(object, startrow, nrows, ncolumns))}
)

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

if (!isGeneric("cellValues")) {
	setGeneric("cellValues", function(object, cells)
		standardGeneric("cellValues"))
}	
	
setMethod("cellValues", signature(object='RasterLayer'), 
	function(object, cells) { 
		return(.rasterReadCells(object, cells))}
)


setMethod("cellValues", signature(object='RasterStack'), 
	function(object, cells) { 
		return(.stackReadCells(object, cells))}
)

if (!isGeneric("xyValues")) {
	setGeneric("xyValues", function(object, xy)
		standardGeneric("xyValues"))
}	
	
setMethod("xyValues", signature(object='RasterLayer'), 
	function(object, xy) { 
		return(.rasterReadXY(object, xy))
	}
)

setMethod("xyValues", signature(object='RasterStack'), 
	function(object, xy) { 
		return(.stackReadXY(object, xy))}
)

