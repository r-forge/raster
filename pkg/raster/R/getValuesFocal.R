# Author: Robert J. Hijmans
# Date :  March 2012
# Version 1.0
# Licence GPL v3

if (!isGeneric("getValuesFocal")) {
	setGeneric("getValuesFocal", function(x, row, nrows, ngb, ...)
		standardGeneric("getValuesFocal"))
}	

setMethod("getValuesFocal", signature(x='RasterLayer', row='missing', nrows='missing', ngb='numeric'), 
function(x, ngb, names=FALSE, ...) {
	getValuesFocal(x, 1, nrow(x), ngb, names=names, ...)
})


setMethod("getValuesFocal", signature(x='RasterLayer', row='numeric', nrows='numeric', ngb='numeric'), 
function(x, row, nrows, ngb, names=FALSE, ...) {
	xx <- raster(x)
	row <- round(row)
	nrows <- round(nrows)
	if (!validRow(xx, row)) {
		stop("Not a valid row number")
	}
	if ( (row+nrows-1) > nrow(xx) ) {
		stop("'nrows' is too high")
	}
	geo <- raster:::.couldBeLonLat(xx)
	ngb <- raster:::.checkngb(ngb, mustBeUneven=TRUE)
	
	ngbr <- floor(ngb[1]/2)
	ngbc <- floor(ngb[2]/2)
	
	r1 <- row - ngbr
	r <- max(1, r1)  # startrow
	nr1 <- nrows + ngb[1] - 1
	nr2 <- nrow(xx)-r+1
	nr <- min(nr1, nr2) # nrows
	v <- getValues(x, max(1, r), nr, format='matrix')
	if (r > r1) {
		add <- r - r1
		v <- rbind(matrix(nrow=add, ncol=ncol(v)), v)
	}
	if (nr1 > nr2) {
		add <- nr1 - nr2 - (r - r1)
		v <- rbind(v, matrix(nrow=add, ncol=ncol(v)))
	}
	if (geo) {
		nv <- ncol(v)
		if (ngbc < nv) {
			v <- cbind(v[,(nv-ngbc+1):nv], v, v[,1:ngbc])
		} else {
			stop('horizontal neighborhood is too big')
		}
	} else {
		add <- matrix(ncol=ngbc, nrow=nrow(v))
		v <- cbind(add, v, add)
	}
	
	vv <- .Call('focal_get', as.vector(t(v)), as.integer(dim(v)), as.integer(ngb), NAOK=TRUE, PACKAGE='raster')
	m <- matrix(vv, nrow=nrows*ncol(xx), byrow=TRUE)
	if (names) {
		rownames(m) <- cellFromRowCol(xx, row, 1):cellFromRowCol(xx, row+nrows-1,ncol(xx))
		colnames(m) <- paste('r', rep(1:ngb[1], each=ngb[2]), 'c', rep(1:ngb[2], ngb[1]), sep='')
	}
	m
}
)



