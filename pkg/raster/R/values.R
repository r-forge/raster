# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.8
# Licence GPL v3



values <- function(object, format='', names=FALSE) {
	format <- trim(format)
	if (class(object) == 'RasterLayer') {
		if (format=='') {
			format <- 'vector'
		}
		return(.rasterValues(object, format, names=names))
	} else if (class(object) == 'RasterBrick') {
		return(object@data@values)
	} else {
		stop('use getValues for a RasterStack')
	}
}



.rasterValues <- function(x, format='vector', names=FALSE) {
	if (dataContent(x)=="nodata") {
		stop("first read some data (e.g., readAll(), or use getValues()") 
	}
	if (format=='matrix') { 
		return(.values.as.matrix(x, names)) 
	} else if (format=='dataframe') { 
		return(.values.as.dataframe(x)) 
	} else {
		return(x@data@values) 
	}
}



.values.as.dataframe <- function(raster) {
	m <- as.data.frame(.values.as.matrix(raster, FALSE))
	if (isTRUE(length(raster@layernames) == dim(m)[2])) { 
		colnames(m) <- raster@layernames 
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

