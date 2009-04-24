# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  April 2009
# Version 0.8
# Licence GPL v3

if (!isGeneric("persp")) {
	setGeneric("persp", function(x,...)
		standardGeneric("persp"))
}	

setMethod("persp", signature(x='RasterLayer'), 
	function(x, maxdim=1000, ...)  {
		if (dataContent(x) != 'all') { 
#	to do: should  test if can read, else sample
			if (canProcessInMemory(x, 2)) {
				x <- readAll(x) 
			} else {
				x <- sampleSkip(x, maxdim, asRaster=TRUE)
			}
		}
		value <- t((values(x, format='matrix'))[nrow(x):1,])
		y <- yFromRow(x, nrow(x):1)
		x <- xFromCol(x,1:ncol(x))
		persp(x=x, y=y, z=value, ...)
	}
)

setMethod("persp", signature(x='RasterStack'), 
	function(x, y=1, maxdim=1000, ...)  {
		if (y < 1) { y <- 1 }
		if (y > nlayers(x)) { y <- nlayers(x) }
		persp(x=x, y=y, maxdim=maxdim, ...)
	}	
)

