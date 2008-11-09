values <- function(object, format='vector', names=FALSE) {
	if (object@data@content=="nodata") {stop("first read some data (e.g., read.all()") }
	if (format=='matrix') { 
		return(.values.as.matrix(object, names)) 
	} else {
		return(object@data@values) 
	}
}



values.row <- function(raster, rownr) {
	if (!(valid.rows(raster, rownr))) {stop(paste(rownr,'is not a valid rownumber')) }
	if (data.content(raster) == 'sparse') {return (.values.row.sparse(raster, rownr)) 
	} else if (data.content(raster) != 'all') {stop('cannot do. Need all data')
	} else {
		startcell <- get.cell.from.rowcol(raster, rownr, 1)
		endcell <- startcell+ncol(raster)-1
		return(values(raster)[startcell:endcell])
	}	
}


.values.row.sparse <- function(raster, rownr, explode=TRUE) {
	if (data.content(raster) != 'sparse') {stop('cannot do. Need sparse')}
	startcell <- get.cell.from.rowcol(raster, rownr, 1)
	endcell <- startcell+ncol(raster)-1
	d <- cbind(data.indices(raster), values(raster))
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



.values.as.matrix <- function(raster, names=FALSE) {
	if (raster@data@content=="nodata") {stop("first read some data (e.g., read.all() or read.row()") }
	
	if (is.matrix(raster@data@values)) {
		return(raster@data@values)
		
	} else if (raster@data@content=="all") {
		mdata <- matrix(raster@data@values, nrow=raster@nrows, ncol=raster@ncols, byrow=TRUE)
		if (names) {
			colnames(mdata) <- seq(1:raster@ncols)
			rownames(mdata) <- seq(1:raster@nrows)
		}	
		return(mdata)

	} else if (raster@data@content=="sparse") {
		mdata <- matrix(NA, nrow=raster@nrows, ncol=raster@ncols, byrow=TRUE)
		vals <- cbind(raster@data@indices, raster@data@values)
		mdata[vals[,1]] <- vals[1,2]
		if (names) {
			colnames(mdata) <- seq(1:raster@ncols)
			rownames(mdata) <- seq(1:raster@nrows)
		}	
		return(mdata)
		
	} else if (raster@data@content=="row") {
		mdata <- matrix(raster@data@values, nrow=1, ncol=raster@ncols, byrow=TRUE)
		if (names) {
			colnames(mdata) <- seq(1:raster@ncols)
			therow <- get.row.from.cell(raster, raster@data@indices[1])
			rownames(mdata) <- therow
		}
		return(mdata)
		
	} else if (raster@data@content=="block") {
		startrow <- get.row.from.cell(raster, raster@data@indices[1])
		startcol <- get.col.from.cell(raster, raster@data@indices[1])
		endrow <- get.row.from.cell(raster, raster@data@indices[2])
		endcol <- get.col.from.cell(raster, raster@data@indices[2])
		ncols <- 1 + endcol - startcol
		nrows <- 1 + endrow - startrow
		
		mdata <- as.matrix(t(raster@data@values[1:ncols]))
		
		if (nrows > 1) {
			for (i in 2:nrows) {
				arow <- raster@data@values[((i-1)*ncols+1):((i-1)*ncols+ncols)]
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

