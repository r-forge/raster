# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3


pointsToRaster <- function(raster, xy, values=rep(1, length(xy[,1])), fun=length, background=NA, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) {
# make this an argument ?  so that you can use e.g.  background=0 
	
	if (class(xy) != 'matrix') {
		stop('xy must be a matrix')
	}
	if (length(values) != length(xy[,1])) {
		stop('values must be a vector of length=length(xy[,1])')
	}
	
	rs <- setRaster(raster, filename)
	rs <- setDatatype(rs, datatype)
	
	cells <- cellFromXY(rs, xy)
	rows <- rowFromCell(rs, cells)
	cols <- colFromCell(rs, cells)
	xyarc <- cbind(xy, values, rows, cols)
	urows <- unique(rows)
	urows <- urows[order(urows)]
	dna <- vector(length=ncol(rs))
	dna[] <- background
	v <- vector(length=0)	
	
	starttime <- proc.time()

	for (r in 1:rs@nrows) {
		d <- dna
		if (r %in% urows) {
			ss <- subset(xyarc, xyarc[,4] == r)
			ucols <- unique(ss[,5])
#			ucols <- ucols[order(ucols)]
			for (c in 1:length(ucols)) {
				sss <- subset(ss, ss[,5] == ucols[c] )
				d[ucols[c]] <- fun(sss[,3])	
			}
		}
		if (filename != "") {
			rs <- setValues(rs, d, r)
			rs <- writeRaster(rs, overwrite=overwrite, filetype=filetype) 
		} else {
			v <- c(v, d)
		}

		if (r %in% track) { .showTrack(r, track, starttime) }
		
	}	
	if (filename == "") {
		rs <- setValues(rs, v)
	}
	return(rs)
}


