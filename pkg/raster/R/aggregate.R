# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,7
# Licence GPL v3

Aggregate <- function(raster, fact = 2, fun = mean, expand = TRUE, rm.NA = TRUE, filename="", overwrite=FALSE, asInt = FALSE)  {
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

	if (expand) {
		rsteps <- as.integer(ceiling(nrow(raster)/yfact))
		csteps <- as.integer(ceiling(ncol(raster)/xfact))
	} else 	{
		rsteps <- as.integer(floor(nrow(raster)/yfact))
		csteps <- as.integer(floor(ncol(raster)/xfact))
	}
	ymn <- ymax(raster) - rsteps * yfact * yres(raster)
	xmx <- xmin(raster) + csteps * xfact * xres(raster)
		
	outraster <- setRaster(raster, filename)
	bndbox <- newBbox(xmin(raster), xmx, ymn, ymax(raster))
	outraster <- setBbox(outraster, bndbox, keepres=F)
	outraster <- setRowCol(outraster, nrows=rsteps, ncols=csteps) 
	
	if (asInt) { outraster <- setDatatype(outraster, 'integer') }
	
	if (dataContent(raster) == 'all') {	
		cols <- rep(rep(1:csteps, each=xfact)[1:ncol(raster)], times=nrow(raster))
		rows <- rep(1:rsteps, each=ncol(raster) * yfact)[1:ncell(raster)]
		cells <- cellFromRowCol(raster, rows, cols)
		
		if (rm.NA) { outraster <- setValues(outraster, as.vector(tapply(values(raster), cells, function(x){fun(na.omit(x))}))) 
		} else {outraster <- setValues(outraster, as.vector(tapply(values(raster), cells, fun))) }

		if (filename(outraster) != "") {writeRaster(outraster, overwrite=overwrite)}
		
	} else if ( dataSource(raster) == 'disk') { 
	
		cols <- rep(rep(1:csteps,each=xfact)[1:ncol(raster)], times=yfact)
		rows <- rep(1, each=(ncol(raster) * yfact))
		v <- vector(length=0)
		for (r in 1:rsteps) 
		{
			startrow <- 1 + (r - 1) * yfact
			if ( r==rsteps) {
				endrow <- min(nrow(raster), startrow + yfact - 1)
				nrows <- endrow - startrow + 1
				theserows <- (startrow * rows)[1:(ncol(raster)*nrows)]
				cols <- cols[1:(ncol(raster)*nrows)]
			} else {
				nrows = yfact
				theserows <- startrow * rows
			}	
			raster <- readRows(raster, startrow = startrow, nrows = nrows)
			cells <- cellFromRowCol(raster, theserows, cols)
			
			if (rm.NA) { vals <- tapply(values(raster), cells, function(x){fun(na.omit(x))} ) 
			} else { vals <- tapply(values(raster), cells, fun) }
			vals <- as.vector(vals)

			if (filename(outraster) == "") {
				v <- c(v, vals)
			} else {
				outraster <- setValues(outraster, vals, r)
				outraster <- writeRaster(outraster, overwrite=overwrite)
			}
		} 
		if (filename(outraster) == "") { 
			outraster <- setValues(outraster, v) 
		}
	}
	return(outraster)
}

