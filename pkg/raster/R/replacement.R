# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3


'projection<-' <- function(x, value) {
	return( setProjection(x, value) )
}

'ncol<-' <- function(x, value) {
	return( setRowCol(x, ncols=value) )
}	

'nrow<-' <- function(x, value) {
	return( setRowCol(x, nrows=value) )
}	


.getColValues <- function(r, colnr) {
	firstcol <- 1:nrow(r) * ncol(r) - ncol(r) 
	cells <- colnr + firstcol 
	return(values(r)[cells])
}

setMethod("[", "RasterLayer",
	function(x,i,j,...,drop=FALSE) {
		if (!missing(j)) { stop("incorrect number of dimensions") }
# consider row, sparse....		
		if (dataContent(x) != 'all') {
			if (dataSource(x) != 'disk') {
				stop('no data associated with this RasterLayer object')
			} else {
				return(cellValues(x, i))
			}	
		} else {
			return(values(x)[i]) 
		}	
	}
)


setReplaceMethod("[", "RasterLayer",  
	function(x, i, j, value) {
		if  (!missing(j)) {	stop("incorrect number of dimensions") }
# what about data rows ?		
		if (dataContent(x) == 'nodata') {
			if (ncell(x) < 1000000) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					x <- setValues(x, vector(length=ncell(x)), v)
					x@data@values[] <- NA
				}
			} else {
				stop('Large raster with no data in memory, use readAll() first')
			}	
		}
		x@data@values[i] <- value
		x@data@source <- 'ram'
		x <- setFilename(x, "")
		return(x)
	}
)


setMethod("[[", c("RasterLayer","ANY","ANY"),
# i = row
# j = col
	function(x,i,j,...,drop=FALSE) {
		if (dataContent(x) == 'nodata') {
			if (ncell(x) < 1000000) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					stop('no data associated with this RasterLayer object')
				}
			} else {
				stop('Large raster, no data in memory, use readAll() first')
			}	
		}
		return( matrix(values(x), nrow(x), ncol(x), byrow=T)[i,j] )
	
#		if (missing(i)) { rows <- 1:nrow(x) } else { rows <- i }
#		if (missing(j)) { cols <- 1:ncol(x) } else { cols <- j }
		# ugly R code
#		cells <- cellFromRowcol(rep(rows[1], length(cols)), cols)
#		for (a in 2:rows) { cells <- c(cells, cells + (a - 1) * ncol(x)) }
#		vals <- values(x)[cells]
#		return( matrix(vals, length(rows), lenght(cols), byrow=T) )
	}
)


