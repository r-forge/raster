# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('getValues', signature(x='RasterStack', row='numeric'), 
function(x, row) {
	if (!is.atomic(row)) {
		stop()
	}
	row <- as.integer(round(row))
	if (!(validRow(x, row))) {
		stop(paste(row,'is not a valid rownumber')) 
	}
	m <- matrix(NA, ncol=nlayers(x), nrow=ncol(x)) 
	for (i in 1:nlayers(x)) {	
		m[,i] <- getValues(x@layers[[i]], row)
	}
	return(m)
}
)




setMethod('getValues', signature(x='Raster', row='numeric'), 
function(x, row) {
	if (!is.atomic(row)) {
		stop()
	}
	row <- as.integer(round(row))

	readrow <- FALSE
	if (!(validRow(x, row))) {
		stop(paste(row, 'is not a valid rownumber')) 
	}

	if (dataContent(x) == 'nodata') {
		readrow <- TRUE
	} else if (dataContent(x) == 'all'){
		if (row < 0) {
			return(x@data@values) 
		}
		startcell <- cellFromRowCol(x, row, 1)
		endcell <- startcell+ncol(x)-1
		return(x@data@values[startcell:endcell])
	} else if (dataContent(x) == 'row') {
		startcell <- cellFromRowCol(x, row, 1)
		endcell <- startcell+ncol(x)-1
		if ( (dataIndices(x)[1] == startcell) & (dataIndices(x)[2] == endcell) ) {
			return(x@data@values)
		} else {
			readrow <- TRUE
		}
	} else if (dataContent(x) == 'block') {
		firstcol <- colFromCell(x, dataIndices(x)[1])
		lastcol <- colFromCell(x, dataIndices(x)[2])
		if (firstcol != 1 | lastcol != ncol(x)) {
			readrow <- TRUE
		} else {
			firstrow <- rowFromCell(x, dataIndices(x)[1])
			lastrow <- rowFromCell(x, dataIndices(x)[2])
			if (row < firstrow | row > lastrow) {
				readrow <- TRUE
			} else {
				startcell <- ((row - firstrow) * ncol(x) + 1) 
				endcell <- startcell + ncol(x) - 1
				return(x@data@values[startcell:endcell])
			}
		}
	} else if (dataContent(x) == 'sparse') {
		return (.values.sparse(x, row)) 
	} else {
		stop('something is wrong with the RasterLayer dataContent')
	}
	x <- readRow(x, row)
	return(x@data@values)
}
)

