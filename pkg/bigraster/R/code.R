

setClass ('BigRasterLayer',
	contains = 'RasterLayer',
	representation (
		bigtrix = "big.matrix"
		),
	prototype (
		)
	)
	

bigRaster <- function(x, ...) {
	b <- as(x, 'BigRasterLayer')
	b@bigtrix <- big.matrix(nrow(b), ncol(b))
	if (hasValues(x)) {
		if (canProcessInMemory(x)) {
			b@bigtrix[] <- as.matrix(x)
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, type=raster:::.progress())
			for (i in 1:tr$n) {
				row1 <- tr$row[i]
				row2 <- row1 + tr$nrows[i] -1
				b@bigtrix[row1:row2, ] <- getValues(x, row=row1, nrows=tr$nrows[i], format='matrix')
				pbStep(pb, i)
			}
			pbClose(pb)
		}
	}
	b
}
	
	

setMethod("getValues", signature(x='BigRasterLayer', row='missing', nrows='missing'), 
function(x, format='') {
	if (format=='matrix') {
		x@bigtrix[]
	} else {
		as.vector(t(x@bigtrix[]))
	}
}
)


setMethod('getValues', signature(x='BigRasterLayer', row='numeric', nrows='missing'), 
	function(x, row, nrows) {
		getValues(x, row=row, nrows=1)
	}
)

setMethod('getValues', signature(x='BigRasterLayer', row='numeric', nrows='numeric'), 
function(x, row, nrows, format='') {
	row <- round(row)
	nrows <- round(nrows)
	stopifnot(validRow(x, row))
	stopifnot(nrows > 0)
	row <- min(x@nrows, max(1, row))
	endrow <- max(min(x@nrows, row+nrows-1), row)
	nrows <- endrow - row + 1
	
	v <-  x@bigtrix[row:(row+nrows-1), ] 
	if (format!= 'matrix') { 
		v <- as.vector(t(v))
	} 
	return(v)
}
)



setMethod('getValuesBlock', signature(x='BigRasterLayer', row='numeric'), 
 	function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), format='') {
		
		row <- max(1, min(x@nrows, round(row[1])))
		lastrow <- min(x@nrows, row + round(nrows[1]) - 1)
		nrows <- lastrow - row + 1
		col <- max(1, min(x@ncols, round(col[1])))
		lastcol <- col + round(ncols[1]) - 1
		ncols <- lastcol - col + 1
		
		if (!(validRow(x, row))) {	stop(paste(row, 'is not a valid rownumber')) }
	
		res <- x@bigtrix[row:lastrow, col:lastcol, drop=FALSE]

		if (format == 'matrix') {
			colnames(res) <- col:lastcol
			rownames(res) <- row:lastrow
		} else {
			res <- as.vector(t(res))
		}
		res
	}
)



	
setMethod('setValues', signature(x='BigRasterLayer'), 
function(x, values) {

	if (length(values) == 1) {	
		values <- matrix(rep(values, ncell(x)), ncol=ncol(x))
	}
	if (is.matrix(values)) { 
		if (ncol(values) == x@ncols & nrow(values) == x@nrows) {
			x@bigtrix[]  <- values
		} else if (ncol(values)==1 | nrow(values)==1) {
			x@bigtrix[]  <- matrix(values, ncol=ncol(x), byrow=TRUE)
		} else {
			stop('cannot use a matrix with these dimensions')
		}
	}
	
	if (length(values) == ncell(x)) { 
		x@data@inmemory <- TRUE
		x@data@fromdisk <- FALSE
		x@file@name <- ""
		x@file@driver <- ""
		x@bigtrix[] <- values
		x@data@min <- min(values, na.rm=TRUE)
		x@data@max <- max(values, na.rm=TRUE)
		x@data@haveminmax <- TRUE
		return(x)
		
	} else {
		stop("length(values) is not equal to ncell(x), or to 1") 
	}
 }
)
	

