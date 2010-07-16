# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  July 2010
# Version 1.0
# Licence GPL v3


rasterFromXYZ <- function(xyz, crs=NA, digits=6) {
	if (inherits(xyz, 'SpatialPoints')) {
		if (inherits(xyz, 'SpatialPointsDataFrame')) {
			xyz <- cbind(coordinates(xyz), xyz@data[,1])
		} else {
			xyz <- coordinates(xyz)		
		}
	}
	x = sort(unique(xyz[,1]))
	dx <- x[-1] - x[-length(x)]
	rx <- min(dx)
	if ( sum(round(dx %% rx, digits = digits)) > 0 ) {
		stop('x cell sizes are not regular')
	}
	y = sort(unique(xyz[,2]))
	dy <- y[-1] - y[-length(x)]
	ry <- min(dy)
	if ( sum(round(dy %% ry, digits = digits)) > 0 ) {
		stop('y cell sizes are not regular')
	}
	minx <- min(x) - 0.5 * rx
	maxx <- max(x) + 0.5 * rx
	miny <- min(y) - 0.5 * ry
	maxy <- max(y) + 0.5 * ry
	r = raster(xmn=minx, xmx=maxx, ymn=miny, ymx=maxy)
	res(r) <- c(rx, ry)
	cells <- cellFromXY(r, xyz[,1:2])
	if (dim(xyz)[2] > 2) {
		r[cells] <- xyz[,3]
	} 	
	return(r)
}	
	

