# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('getValues', signature(x='RasterStack', row='numeric'), 
function(x, row, format='matrix', names=TRUE) {
	if (!is.atomic(row)) {
		stop()
	}
	row <- as.integer(round(row))
	if (!(validRow(x, row))) {
		stop(paste(row,'is not a valid rownumber')) 
	}
	res <- matrix(NA, ncol=nlayers(x), nrow=ncol(x)) 
	for (i in 1:nlayers(x)) {	
		res[,i] <- getValues(x@layers[[i]], row)
	}

	if (names) {
		colnames(res) <- layerNames(x)
	}
	if (format=='dataframe') {
		res <- as.data.frame(res)
	}
	if (format=='vector') {
		res <- as.vector(res)
	}
	
	return(res)
}
)


setMethod('getValues', signature(x='RasterLayer', row='numeric'), 
function(x, row, format='vector', names=TRUE) {
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
			res <- x@data@values
		}
		startcell <- cellFromRowCol(x, row, 1)
		endcell <- startcell+ncol(x)-1
		res <- x@data@values[startcell:endcell]
	} else if (dataContent(x) == 'row') {
		startcell <- cellFromRowCol(x, row, 1)
		endcell <- startcell+ncol(x)-1
		if ( (dataIndices(x)[1] == startcell) & (dataIndices(x)[2] == endcell) ) {
			res <- (x@data@values)
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
				res <- x@data@values[startcell:endcell]
			}
		}
	} else {
		stop('something is wrong with the RasterLayer dataContent')
	}
	
	if (readrow) {
		res <- values(readRow(x, row))
	}
	if (format=='dataframe') {
		res <- as.data.frame(res)
		if (names) {
			colnames(res) <- layerNames(x)
		}
	}
	if (format=='matrix') {
		res <- as.matrix(res)
		if (names) {
			colnames(res) <- layerNames(x)
		}
	}
	return(res)	
}
)


setMethod('getValues', signature(x='RasterBrick', row='numeric'), 
function(x, row, format='matrix', names=TRUE) {
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
			res <- x@data@values
		}
		startcell <- cellFromRowCol(x, row, 1)
		endcell <- startcell+ncol(x)-1
		res <- x@data@values[startcell:endcell,]
	} else if (dataContent(x) == 'row') {
		startcell <- cellFromRowCol(x, row, 1)
		endcell <- startcell+ncol(x)-1
		if ( (dataIndices(x)[1] == startcell) & (dataIndices(x)[2] == endcell) ) {
			res <- x@data@values
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
				res <- x@data@values[startcell:endcell,]
			}
		}
#	} else if (dataContent(x) == 'sparse') {
#		res <- .values.sparse(x, row)
	} else {
		stop('something is wrong with the RasterLayer dataContent')
	}
	if (readrow) {
		x <- readRow(x, row)
		res <- x@data@values
	}
	if (names) {
		colnames(res) <- layerNames(x)
	}
	if (format=='dataframe') {
		res <- as.data.frame(res)
	}
	if (format=='vector') {
		res <- as.vector(res)
	}
	
	return(res)
}
)