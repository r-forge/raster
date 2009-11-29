# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  November 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("plot3d")) {
	setGeneric("plot3d", function(x,...)
		standardGeneric("plot3d"))
}	

setMethod("plot3d", signature(x='RasterLayer'), 
function(x, background=0, maxdim=250, zfac=6, col=terrain.colors, rev=TRUE, ...) { 
# much of this code was taken from example(surface3d) in the rgl package
	if (!require(rgl)){ stop("to use this function you need to install the 'rgl' package") }
	x <- sampleSkip(x, maxdim, asRaster=TRUE)
	X <- xFromCol(x,1:ncol(x))
	Y <- yFromRow(x, nrow(x):1)
	Z <- t((values(x, format='matrix'))[nrow(x):1,])
	Z[is.na(Z)] <- background
	zlim <- range(Z, na.rm=TRUE)
	zlen <- zlim[2] - zlim[1] + 1
	xlen <- max(X) - min(X)
	ylen <- max(Y) - min(Y)
	adj <- zlen/min(ylen,xlen)
	X <- X * adj * zfac
	Y <- Y * adj * zfac
	colorlut <- col(zlen) # height color lookup table
	if (rev) { colorlut <- rev(colorlut) }
	color <- colorlut[ Z-zlim[1]+1 ] # assign colors to heights for each point
	open3d()
	surface3d(X, Y, Z, color=color, back="lines")
}
)
