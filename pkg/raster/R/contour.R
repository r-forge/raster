# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  April 2009
# Version 0.8
# Licence GPL v3


setMethod("contour", signature(x='RasterLayer'), 
	function(x, maxdim=1000, add=TRUE, ...)  {
		if (dataContent(x) != 'all') { 
#	to do: should  test if can read, else sample
			if (canProcessInMemory(x, 2)) {
				x <- readAll(x) 
			} else {
				x <- sampleSkip(x, maxdim, asRaster=TRUE)
			}
		}
		contour(x=xFromCol(x,1:ncol(x)), y=yFromRow(x, nrow(x):1), z=t((values(x, format='matrix'))[nrow(x):1,]), add=add, ...)
	}
)

