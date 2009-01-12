
values <- function(object, format='vector', names=FALSE) {
	if (object@data@content=="nodata") {stop("first read some data (e.g., readAll()") }
	if (format=='matrix') { 
		return(.values.as.matrix(object, names)) 
	if (format=='dataframe') { 
		return(.values.as.dataframe(object)) 
	} else {
		return(object@data@values) 
	}
}



valuesRow <- function(raster, rownr) {
	if (!(validRows(raster, rownr))) {stop(paste(rownr,'is not a valid rownumber')) }
	if (dataContent(raster) == 'sparse') {return (.valuesRow.sparse(raster, rownr)) 
	} else if (dataContent(raster) != 'all') {stop('cannot do. Need all data')
	} else {
		startcell <- cellFromRowcol(raster, rownr, 1)
		endcell <- startcell+ncol(raster)-1
		return(values(raster)[startcell:endcell])
	}	
}


.valuesRow.sparse <- function(raster, rownr, explode=TRUE) {
	if (dataContent(raster) != 'sparse') {stop('cannot do. Need sparse')}
	startcell <- cellFromRowcol(raster, rownr, 1)
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


.values.as.dataframe <- function(raster) {
	m <- as.data.frame(values.as.matrix(raster, FALSE))
	if raster@data@colnames != '' { 
		colnames(m) <- raster@data@colnames 
	} else {
		colnames(m) <- seq(1:ncol(m))
	}	
	rownames(m) <- seq(1:nrow(m))
	return(m)
}

.values.as.matrix <- function(raster, names=FALSE) {
	if (dataContent(raster)=="nodata") {stop("first read some data (e.g., readAll() or readRow()") }
	
	if (is.matrix(values(raster))) {
		return(values(raster))
		
	} else if (dataContent(raster)=="all") {
		mdata <- matrix(values(raster), nrow=nrow(raster), ncol=ncol(raster), byrow=TRUE)
		if (names) {
			colnames(mdata) <- seq(1:ncol(raster))
			rownames(mdata) <- seq(1:nrow(raster))
		}	
		return(mdata)

	} else if (dataContent(raster)=="sparse") {
		mdata <- matrix(NA, nrow=nrow(raster), ncol=ncol(raster), byrow=TRUE)
		vals <- cbind(dataIndices(raster), values(raster))
		mdata[vals[,1]] <- vals[1,2]
		if (names) {
			colnames(mdata) <- seq(1:ncol(raster))
			rownames(mdata) <- seq(1:nrow(raster))
		}	
		return(mdata)
		
	} else if (dataContent(raster)=="row") {
		mdata <- matrix(values(raster), nrow=1, ncol=ncol(raster), byrow=TRUE)
		if (names) {
			colnames(mdata) <- seq(1:ncol(raster))
			therow <- rowFromCell(raster, dataIndices(raster)[1])
			rownames(mdata) <- therow
		}
		return(mdata)
		
	} else if (dataContent(raster)=="block") {
		startrow <- rowFromCell(raster, dataIndices(raster)[1])
		startcol <- colFromCell(raster, dataIndices(raster)[1])
		endrow <- rowFromCell(raster, dataIndices(raster)[2])
		endcol <- colFromCell(raster, dataIndices(raster)[2])
		ncols <- 1 + endcol - startcol
		nrows <- 1 + endrow - startrow
		
		mdata <- as.matrix(t(values(raster)[1:ncols]))
		
		if (nrows > 1) {
			for (i in 2:nrows) {
				arow <- values(raster)[((i-1)*ncols+1):((i-1)*ncols+ncols)]
				mdata <- rbind(mdata, t(arow))
			}
		}
		
		if (names) {
			rowlist <- list()
			for (i in 1:nrows) {
				r <- startrow + i - 1
				rowlist[i] <- paste(r, sep="")
				rownames(mdata) <- rowlist
				colnames(mdata) <- seq(1:ncols)+startcol-1
			}	
		}
		return(mdata)
	}	
}

