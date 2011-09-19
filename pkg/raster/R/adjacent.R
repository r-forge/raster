# Author: Robert J. Hijmans
# Date :  September 2011
# Version 1.0
# Licence GPL v3


adjacent <- function(x, cells, directions=4) {
	stopifnot(directions %in% c(4,8))
	r <- res(x)
	xy <- xyFromCell(x, cells)

	if (directions==4) {
		d <- t(apply(xy, 1, function(x)c(x[1]-r[1], x[1]+r[1], x[1], x[1], x[2], x[2], x[2]+r[2], x[2]-r[2])))
	} else { #if (directions==8) {
		d <- t(apply(xy, 1, function(x)c(rep(x[1]-r[1], 3), rep(x[1]+r[1],3), x[1], x[1], rep(c(x[2]+r[2], x[2], x[2]-r[2]), 2),  x[2]+r[2], x[2]-r[2])))
	} 
	d <- matrix(as.vector(d), ncol=2)
	if (.couldBeLonLat(x)) {
		# normalize longitude to -180..180
		d[,1] <- (d[,1] + 180) %% 360 - 180
	}
	unique(na.omit(cellFromXY(x, d)))
}

