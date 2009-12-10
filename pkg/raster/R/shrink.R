# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("shrink")) {
	setGeneric("shrink", function(x, ...)
		standardGeneric("shrink"))
}	


setMethod('shrink', signature(x='RasterLayer'), 
function(x, padding=0, filename='', ...) {
	filename <- trim(filename)

	if (dataContent(x) != 'all' & dataSource(x) == 'disk')  {
		if (canProcessInMemory(x, 3)) {
			x <- readAll(x)
		}
	}
	
	nr <- nrow(x)
	nc <- ncol(x)

	for (r in 1:nr) {
		v <- getValues(x, r)
		if (sum(is.na(v)) < nc) { break }
	}
	if ( r == nr) { stop('only NA values found') }
	firstrow <- max(r+padding, 1)
	
	for (r in nr:1) {
		v <- getValues(x, r)
		if (sum(is.na(v)) < nc) { break }
	}
	lastrow <- min(r+padding, nr)
	
	if (lastrow < firstrow) { 
		tmp <- firstrow
		firstrow <- lastrow
		lastrow <- tmp
	}
	
	cells <- cellFromCol(x, 1)
	for (c in 1:nc) {
		v <- cellValues(x, cells)
		if (sum(is.na(v)) < nr) { break }
		cells <- cells + 1
	}
	firstcol <- max(c+padding, 1) 
	
	cells <- cellFromCol(x, nc)
	for (c in nc:1) {
		v <- cellValues(x, cells)
		if (sum(is.na(v)) < nr) { break }
		cells <- cells - 1
	}
	lastcol <- min(c+padding, nc)
	
	if (lastcol < firstcol) { 
		tmp <- firstcol
		firstcol <- lastcol
		lastcol <- tmp
	}
	
	xr <- xres(x)
	yr <- yres(x)
	e <- extent(xFromCol(x, firstcol)-0.5*xr, xFromCol(x, lastcol)+0.5*xr, yFromRow(x, lastrow)-0.5*yr, yFromRow(x, firstrow)+0.5*yr)
	
	return( crop(x, e, ...) )
}
)

