# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3




valuesRow <- function(object, rownr) {
	if (dataContent(object) == 'nodata') {
		return(values(readRow(object, rownr)))
	}
	if (rownr < 0) {
		if (dataContent(object) == 'all') {
			return(object)
		} else {
			stop('cannot get these values')
		}
	}
	if (!(validRow(object, rownr))) {
		stop(paste(rownr,'is not a valid rownumber')) 
	}
	
	
	if (dataContent(object) == 'sparse') {
		return (.valuesRow.sparse(object, rownr)) 
	} else if (dataContent(object) == 'row') {
		startcell <- cellFromRowCol(object, rownr, 1)
		endcell <- startcell+ncol(object)-1
		if (dataIndices(object) == c(startcell, endcell)) {
			return(values(object))
		} else {
			stop('this row is not in memory. First use readRow() or readAll')		
		}
	} else if (dataContent(object) == 'block') {
		firstcol <- colFromCell(object, dataIndices(object)[1])
		lastcol <- colFromCell(object, dataIndices(object)[2])
		if (firstcol != 1 | lastcol != ncol(object)) {
			stop('the block data in this object does not have complete rows')
		}
		firstrow <- rowFromCell(object, dataIndices(object)[1])
		lastrow <- rowFromCell(object, dataIndices(object)[2])
		if (rownr < firstrow | rownr > lastrow) {
			stop('this row is not in memory. First use readRow() or readAll')		
		}
		startcell <- ((rownr - firstrow) * ncol(object) + 1) 
		endcell <- startcell + ncol(object) - 1
		if (class(object) == 'objectStack') {
			return(values(object)[startcell:endcell,])
		} else {	
			return(values(object)[startcell:endcell])
		}
	} else if (dataContent(object) == 'all'){
		startcell <- cellFromRowCol(object, rownr, 1)
		endcell <- startcell+ncol(object)-1
		if (class(object) == 'RasterStack') {
			return(values(object)[startcell:endcell,])
		} else {	
			return(values(object)[startcell:endcell])
		}
	} else {
		stop('something is wrong with the RasterLayer dataContent')
	}
}


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

