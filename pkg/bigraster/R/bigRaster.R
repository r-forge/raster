
bigRaster <- function(x, ...) {
	b <- as(as(x, 'BasicRaster'), 'BigRasterLayer')
	b@bigtrix <- big.matrix(nrow(b), ncol(b))
	if (hasValues(x)) {
		if (canProcessInMemory(x)) {
			b@bigtrix[] <- as.matrix(x)
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, type=raster:::.progress())
			for (i in 1:tr$n) {
				row1 <- tr$row[i]
				row2 <- row1 + tr$nrows[i] -1
				b@bigtrix[row1:row2, ] <- getValues(x, row=row1, nrows=tr$nrows[i], format='matrix')
				pbStep(pb, i)
			}
			pbClose(pb)
		}
	}
}
	

bigBrick <- function(x, ...) {
	b <- as(x, 'BigRasterBrick')
	b@bigtrix <- big.matrix(nrow(b), ncol(b))
	if (hasValues(x)) {
		if (canProcessInMemory(x)) {
			b@bigtrix[] <- as.matrix(x)
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, type=raster:::.progress())
			for (i in 1:tr$n) {
				row1 <- tr$row[i]
				row2 <- row1 + tr$nrows[i] -1
				b@bigtrix[row1:row2, ] <- getValues(x, row=row1, nrows=tr$nrows[i], format='matrix')
				pbStep(pb, i)
			}
			pbClose(pb)
		}
	}
	b
}
	
