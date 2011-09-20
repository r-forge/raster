
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


