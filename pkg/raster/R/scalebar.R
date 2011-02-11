

.scalebar <- function(object, xy=click(), length=100000, label='100 km', offset=0.3, lwd=4, ... ) {
	xy2 <- xy
	xy2[1,1] <- xy2[1,1] + length
	lines(rbind(xy, xy2), lwd=lwd, ...)
	xy[1,1] <- xy[1,1] + 0.5 * length
	xy[1,2] <- xy[1,2] + offset * length
	text(xy[1,1], xy[1,2], label)
}



.arrow <- function(object, xy=click(), length=50000, headlength=0.1, ...) {
	arrows(xy[1], xy[2], xy[1], xy[2]+length, length=headlength, ...)
	lines(rbind(xy, rbind(cbind(xy[1], xy[2]-length))), ...)
	text(xy[1,1], xy[1,2]-(0.25*length), 'N')
}


