

.gridDistance2 <- function(x) {
	rs <- res(x)
	xdist <- rs[1]
	ydist <- rs[2]
	xydist <- sqrt(xdist^2 + ydist^2)
	z1 <- z2 <- raster(x)
	x <- getValues(x)
	z1[] <- .Call('broom', x, as.integer(dim(z1)), c(xdist, ydist, xydist), as.integer(1))
	z2[] <- .Call('broom', x, as.integer(dim(z2)), c(xdist, ydist, xydist), as.integer(0))
	min(z1, z2)
}




