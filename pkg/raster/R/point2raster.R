
pointsToRaster <- function(raster, xy, values, fun=length, filename="", overwrite=FALSE) {
	if (class(xy) != 'matrix') {stop('xy must be a matrix')}
	if (length(values) != length(xy[,1])) {stop('values must be a vector of length=length(xy[,1])')}
	
	xya <- cbind(xy, values)
	rs <- setRaster(raster, filename)
	cells <- cellFromXY(rs, xya[,1:2])
	rows <- rowFromCell(rs, cells)
	cols <- colFromCell(rs, cells)
	xyarc <- cbind(xya, rows, cols)
	urows <- unique(rows)
	urows <- urows[order(urows)]
#	allrows <- seq(1:nrow(rs))
#	allrows <- cbind(allrows, FALSE)
#	allrows[urows, 2] <- TRUE
	d <- vector(length=ncol(rs))
	d[] <- NA
	dna <- d
	v <- vector(length=0)	
	for (r in 1 : rs@nrows) {
#		if (!allrows[r, 2]) {	
		if (r %in% urows) {
			ss <- subset(xyarc, xyarc[,4] == r)
			cols <- ss[,5]
			ucols <- unique(cols)
			ucols <- ucols[order(ucols)]
			d <- dna
			for (c in 1:length(ucols)) {
				sss <- subset(ss, ss[,5] == ucols[c] )
				d[ucols[c]] <- fun(sss[,3])	
			}
			if (filename != "") {
				rs <- setValues(rs, d, r)
				rs <- writeRaster(rs)
			} else {
				v <- c(v, d)
			}
		} else {
			if (filename != "") {
				rs <- setValues(rs, dna, r)
				rs <- writeRaster(rs, r) 
			} else {
				v <- c(v, dna)
			}
		} 
	}	
	if (filename == "") {
		rs <- setValues(rs, v)
	}
	return(rs)
}
