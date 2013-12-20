# Author: Robert J. Hijmans
# Date: December 2011
# Version 1.0
# Licence GPL v3

if (!isGeneric('symdif')) {
	setGeneric('symdif', function(x, y, ...)
		standardGeneric('symdif'))
}	



setMethod('symdif', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y, ...) {
	yy <- list(y, ...)
	for (y in yy) {
		if (gIntersects(x, y)) {
			part1 <- erase(x, y)
			part2 <- erase(y, x)
			x <- combine(part1, part2)
		}
	}
	x
}
)

