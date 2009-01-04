

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

if (!isGeneric("valuesCells")) {
	setGeneric("valuesCells", function(object, cells)
		standardGeneric("valuesCells"))
}	
	
setMethod("valuesCells", signature(object='RasterLayer'), 
	function(object, cells) { 
		return(.rasterReadCells(object, cells))}
)


setMethod("valuesCells", signature(object='RasterStack'), 
	function(object, cells) { 
		return(.stackReadCells(object, cells))}
)

if (!isGeneric("valuesXY")) {
	setGeneric("valuesXY", function(object, xy)
		standardGeneric("valuesXY"))
}	
	
setMethod("valuesXY", signature(object='RasterLayer'), 
	function(object, xy) { 
		return(.rasterReadXY(object, xy))
	}
)

setMethod("valuesXY", signature(object='RasterStack'), 
	function(object, xy) { 
		return(.stackReadXY(object, xy))}
)


setMethod("plot", signature(x='RasterLayer', y='missing'), 
	function(x, y, ...)  {
		map(x, ...)
	}
)	
	

setMethod("plot", signature(x='RasterStack', y='numeric'), 
	function(x, y, ...)  {
		ind <- as.integer(round(y))
		ind <- min(max(ind, 1), nlayers(x))
		map(x, ind, ...)
	}
)		


setMethod("plot", signature(x='RasterBrick', y='numeric'), 
	function(x, y, ...)  {
		ind <- as.integer(round(y))
		ind <- min(max(ind, 1), nlayers(x))
		map(x, ind, ...)
	}
)		


setMethod('summary', signature(object='AbstractRaster'), 
	function(object, ...) {
	# to be replaces by something more typical for summary in R, i.e. a sumary of the raster values
		cat ("Cells: " , ncells(object), '\n')
		if ( class(object) == "RasterLayer" ) {
			if ( dataContent(object) == "all") {
				cat("NAs  : ", sum(is.na(values(object))), "\n")
				summary(values(object))
			} else {
				cat("values not in memory\n")
			}
		} else if (class(object) == "RasterStack" | class(object) == "RasterBrick") {
			if (dataContent(object) == 'all') {
				for (n in 1:nlayers(object)) {
					cat("layer ", n, "\n")
					cat("NAs  : ", sum(is.na(values(object)[,n])), "\n")
					summary(values(object)[,n])
				}
			} else {
				cat("values not in memory\n")
			}
		} 
	}	
)


setMethod('hist', signature(x='RasterLayer'), 
	function(x, ...){
		maxsamp <- 1000000
		if (dataContent(x) != 'all') {
			if (dataSource(x) == 'disk') {
		# TO DO: ake a function that does this by block and combines  all data into a single histogram
				if (ncells(x) > maxsamp) {
					cells <- unique(runif(maxsamp) * ncells(x))
					cells <- cells[cells > 0]
					values <- valuesCells(x, cells)
					nas <- length(na.omit(values))
					msg <- paste(round(100 * length(cells) / ncells(x)), "% of the raster cells were used", sep="")
					if (nas < length(values)) {
						msg <- paste(msg, " (of which ", 100 - round(100 * nas / length(values)), "% were NA)", sep="")
					}
					msg <- paste(msg, ". ",nas," values used.", sep="")
					warning(msg)
				} else {
					values <- values(readAll(x))
				}	
			} else { stop('cannot make a histogram; need data on disk or in memory')}
		} else {
			values <- values(x)
		}			
		hist(values, ...)
	}	
)

