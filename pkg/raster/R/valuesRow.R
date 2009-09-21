# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("valuesRow")) {
	setGeneric("valuesRow", function(x, rownr)
		standardGeneric("valuesRow"))
}	


setMethod('valuesRow', signature(x='RasterStack'), 
function(x, rownr) {

	if (!(validRow(x, rownr))) {
		stop(paste(rownr,'is not a valid rownumber')) 
	}

	if (dataContent(x) == 'nodata') {
		return( .getStackValuesRow(x, rownr) )
	}
	
	if (dataContent(x) == 'all'){
		if (rownr < 0) {
			return(values(x))
		}
		startcell <- cellFromRowCol(x, rownr, 1)
		endcell <- startcell+ncol(x)-1
		return(values(x)[startcell:endcell,])

	} else if (dataContent(x) == 'row') {
		startcell <- cellFromRowCol(x, rownr, 1)
		endcell <- startcell+ncol(x)-1
		if ( (dataIndices(x)[1] == startcell) & (dataIndices(x)[2] == endcell) ) {
			return(values(x))
		} else {
			return(.getStackValuesRow(x, rownr))
		}
		
	} else if (dataContent(x) == 'block') {
		firstcol <- colFromCell(x, dataIndices(x)[1])
		lastcol <- colFromCell(x, dataIndices(x)[2])
		if (firstcol != 1 | lastcol != ncol(x)) {
			return(.getStackValuesRow(x, rownr))
		}
		firstrow <- rowFromCell(x, dataIndices(x)[1])
		lastrow <- rowFromCell(x, dataIndices(x)[2])
		if (rownr < firstrow | rownr > lastrow) {
			return(.getStackValuesRow(x, rownr))
		}
		startcell <- ((rownr - firstrow) * ncol(x) + 1) 
		endcell <- startcell + ncol(x) - 1
		return(values(x)[startcell:endcell,])
	} else {
		stop('something is wrong with the RasterStack dataContent')
	}
}
)


.getStackValuesRow <- function(x, rownr) {
	m <- matrix(NA, ncol=nlayers(x), nrow=ncol(x)) 
	for (i in 1:nlayers(x)) {	
		m[,i] <- valuesRow(x@layers[[i]], rownr)
	}
	return(m)
}




setMethod('valuesRow', signature(x='RasterBrick'), 
function(x, rownr) {
	if (dataContent(x) == 'nodata') {
		return(values(readRow(x, rownr)))
	}
	if (!(validRow(x, rownr))) {
		stop(paste(rownr,'is not a valid rownumber')) 
	}

	if (dataContent(x) == 'all'){
		if (rownr < 0) {
			return(values(x))
		}
		startcell <- cellFromRowCol(x, rownr, 1)
		endcell <- startcell+ncol(x)-1
		return(values(x)[startcell:endcell])
	} else if (dataContent(x) == 'row') {
		startcell <- cellFromRowCol(x, rownr, 1)
		endcell <- startcell+ncol(x)-1
		if ( (dataIndices(x)[1] == startcell) & (dataIndices(x)[2] == endcell) ) {
			return(values(x))
		} else {
			return(values(readRow(x, rownr)))
		}
	} else if (dataContent(x) == 'block') {
		firstcol <- colFromCell(x, dataIndices(x)[1])
		lastcol <- colFromCell(x, dataIndices(x)[2])
		if (firstcol != 1 | lastcol != ncol(x)) {
			return(values(readRow(x, rownr)))
		}
		firstrow <- rowFromCell(x, dataIndices(x)[1])
		lastrow <- rowFromCell(x, dataIndices(x)[2])
		if (rownr < firstrow | rownr > lastrow) {
			return(values(readRow(x, rownr)))
		}
		startcell <- ((rownr - firstrow) * ncol(x) + 1) 
		endcell <- startcell + ncol(x) - 1
		return(values(x)[startcell:endcell])
	} else if (dataContent(x) == 'sparse') {
		return (.valuesRow.sparse(x, rownr)) 
	} else {
		stop('something is wrong with the RasterLayer dataContent')
	}
}
)



setMethod('valuesRow', signature(x='RasterLayer'), 
function(x, rownr) {
	if (!(validRow(x, rownr))) {
		stop(paste(rownr,'is not a valid rownumber')) 
	}

	if (dataContent(x) == 'nodata') {
		return(values(readRow(x, rownr)))
	} else if (dataContent(x) == 'all'){
		if (rownr < 0) {
			return(values(x)) 
		}
		startcell <- cellFromRowCol(x, rownr, 1)
		endcell <- startcell+ncol(x)-1
		return(values(x)[startcell:endcell])
	} else if (dataContent(x) == 'row') {
		startcell <- cellFromRowCol(x, rownr, 1)
		endcell <- startcell+ncol(x)-1
		if ( (dataIndices(x)[1] == startcell) & (dataIndices(x)[2] == endcell) ) {
			return(values(x))
		} else {
			return(values(readRow(x, rownr)))
		}
	} else if (dataContent(x) == 'block') {
		firstcol <- colFromCell(x, dataIndices(x)[1])
		lastcol <- colFromCell(x, dataIndices(x)[2])
		if (firstcol != 1 | lastcol != ncol(x)) {
			return(values(readRow(x, rownr)))
		}
		firstrow <- rowFromCell(x, dataIndices(x)[1])
		lastrow <- rowFromCell(x, dataIndices(x)[2])
		if (rownr < firstrow | rownr > lastrow) {
			return(values(readRow(x, rownr)))
		}
		startcell <- ((rownr - firstrow) * ncol(x) + 1) 
		endcell <- startcell + ncol(x) - 1
		return(values(x)[startcell:endcell])
	} else if (dataContent(x) == 'sparse') {
		return (.valuesRow.sparse(x, rownr)) 
	} else {
		stop('something is wrong with the RasterLayer dataContent')
	}
}
)

.valuesRow.sparse <- function(raster, rownr, explode=TRUE) {
	if (dataContent(raster) != 'sparse') {stop('cannot do. Need sparse')}
	startcell <- cellFromRowCol(raster, rownr, 1)
	endcell <- startcell+ncol(raster)-1
	d <- cbind(dataIndices(raster), values(raster))
	d <- d[d[,1] >= startcell & d[,1] <= endcell, ] 
	if (explode) { 
		cells <- startcell:endcell
		cells[] <- NA
		cells[d[,1]] <- d[,2]	
		return(cells)
	} else {
		return(d)
	}	
}

