
pointsToRaster <- function(raster, xy, values, fun=length, filename="", overwrite=FALSE) {
	if (class(xy) != 'matrix') {stop('xy must be a matrix')}
	if (length(values) != length(xy[,1])) {stop('values must be a vector of length=length(xy[,1])')}
	
	rs <- setRaster(raster, filename)
	cells <- cellFromXY(rs, xy)
	rows <- rowFromCell(rs, cells)
	cols <- colFromCell(rs, cells)
	xyarc <- cbind(xy, values, rows, cols)
	urows <- unique(rows)
	urows <- urows[order(urows)]
	d <- vector(length=ncol(rs))
	d[] <- NA
	dna <- d
	v <- vector(length=0)	
	for (r in 1:rs@nrows) {
		if (r %in% urows) {
			ss <- subset(xyarc, xyarc[,4] == r)
			ucols <- unique(ss[,5])
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
