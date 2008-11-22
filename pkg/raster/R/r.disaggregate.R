# Author: Robert J. Hijmans
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,7
# Licence GPL v3


r.disaggregate <- function(raster, fact=2, filename="", overwrite=FALSE) {
	if (length(fact)==1) {
		fact <- round(fact)
		if (fact < 2) { stop('fact should be > 1') }
		xfact <- yfact <- fact
	} else if (length(fact)==2) {
		xfact <- round(fact[1])
		yfact <- round(fact[2])
		if (xfact < 2) { stop('fact[1] should be > 1') } 
		if (yfact < 2) { stop('fact[2] should be > 1') }
	} else {
		stop('length(fact) should be 1 or 2')
	}
	
	outraster <- set.raster(raster, filename)
	outraster <- set.rowcol(outraster, nrow(raster) * yfact, ncol(raster) * xfact) 

	if ( dataContent(raster)=='all') {
		
		cols <- rep(rep(1:ncol(raster), each=xfact), times=nrow(raster)*yfact)
		rows <- rep(1:nrow(raster), each=ncol(raster)*xfact*yfact)
		cells <- get.cell.from.rowcol(raster, rows, cols)
		outraster <- set.values(outraster, values(raster)[cells])
		if (filename(outraster) != "") {write.raster(outraster)}
		
	} else if ( dataSource(raster) == 'disk') { 

		v <- vector(length=0)
		cols <- rep(1:ncol(raster), each=xfact)
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			for (i in 1:yfact) {
			
				if (filename(outraster) == '') {
					v <- c(v, values(raster)[cols])
				} else {
					outraster <- set.values.row(outraster, values(raster)[cols], (r-1) * xfact + i)
					outraster <- write.row(outraster)
				}	
			}	
		}
		if (filename(outraster) == '') { outraster <- set.values(outraster, v) }
	} 
	return(outraster)
}
