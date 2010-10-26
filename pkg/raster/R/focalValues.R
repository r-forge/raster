# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

# to do: if proj is latlon & between -180 to 180, then use cells from other side..
#	global <- .isGlobalLatLon(raster)
#	if (global) {}
	

focalValues <- function(x, ...) {
	.focalValues(x, ...)
}
	
	
.focalValues <- function(x, row, ngb=3) {

	if (missing(row)) stop('You must provide a row number "row=" argument')

	if (!(validRow(x, row))) {	stop(paste(row, 'is not a valid rownumber')) }

	ngb <- .checkngb(ngb)
	
	r1 = row - floor(ngb[1]/2)
	r2 = r1 + ngb[1] - 1
	r1 = max(1, r1)
	r2 = min(nrow(x), r2)
	nrows = r2 - r1 + 1
	
	col1 = floor(ngb[2]/2)
	col2 = ngb[2]-(col1+1)
	add1 = matrix(0, ncol=col1, nrow=nrows)
	add2 = matrix(0, ncol=col2, nrow=nrows)

	cols=x@ncols
	nrs = matrix(ncol=cols, nrow=nrows)
	nrs[] = 1:length(nrs)
	nrs = cbind(add1, nrs, add2)
	
	idx = matrix(ncol=cols, nrow=ngb[2]*nrows)
	cc = 1:ngb[2]
	for (c in 1:cols) {
		idx[,c] = nrs[, (cc+c-1)] 
	}
	id = as.vector(idx)
	id = cbind(rep(1:cols, each=nrow(idx)), id)
	id = subset(id, id[,2]>0)

	nl <- nlayers(x)
	if (nl == 1) {
		ngbdata = matrix(getValuesBlock(x, r1, nrows), ncol=ncol(x))
		v = cbind( id[,1], as.vector(ngbdata)[id[,2]] )
		colnames(v) <- c('col', 'value')
	} else {
		alldata <- getValuesBlock(x, r1, nrows)
		v <- id[,1,drop=FALSE]
		for (i in 1:nl) {
			ngbdata = matrix(alldata[,i], ncol=ncol(x))
			v <- cbind( v, as.vector(ngbdata)[id[,2]] )
		}
		colnames(v) <- c('col', layerNames(x))
	}
	return(v)
}


