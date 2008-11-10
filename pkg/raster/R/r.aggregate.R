# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,7
# Licence GPL v3


r.aggregate <- function(raster, fact = 2, fun = mean, expand = TRUE, rm.NA = TRUE, INT = FALSE, filename="", overwrite=FALSE)  {
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
	yexpansion <- rsteps * yfact - nrow(raster) * xres(raster)
	xexpansion <- csteps * xfact - ncol(raster) * yres(raster)
		
	outraster <- set.raster(raster, filename)
	outraster <- set.bbox(outraster, xmx = xmax(raster) + xexpansion, ymn = ymin(raster) - yexpansion)
	outraster <- set.rowcol(outraster, nrows=rsteps, ncols=csteps) 
	
	if (INT) { 
		outraster <- set.datatype(outraster, 'integer')
	} else { 
		outraster <- set.datatype(outraster, 'numeric') 
	}
		

	if (data.content(raster) == 'all') {
	
		cols <- rep(rep(1:csteps, each=xfact)[1:ncol(raster)], times=nrow(raster))
		rows <- rep(1:rsteps, each=ncol(raster) * yfact)[1:ncells(raster)]
		cells <- get.cell.from.rowcol(raster, rows, cols)
		
		if (rm.NA) { outraster <- set.values(outraster, as.vector(tapply(values(raster), cells, function(x){fun(na.omit(x))}))) 
		} else {outraster <- set.values(outraster, as.vector(tapply(values(raster), cells, fun))) }

		if (filename(outraster) != "") {write.raster(outraster)}

		
	} else if ( data.source(raster) == 'disk') { 
	
		cols <- rep(rep(1:csteps,each=xfact)[1:ncol(raster)], times=xfact)
		newdata <- vector(length=rsteps*csteps)
		v <- vector(length=0)
		for (r in 1:rsteps) 
		{
			startrow <- 1 + (r - 1) * yfact
			endrow <- min(nrow(raster), startrow + yfact - 1)
			nrows <- endrow - startrow + 1
			raster <- read.rows(raster, startrow = startrow, nrows = nrows)
			cols <- cols[1:(nrows * ncol(raster))]
			rows <- rep(startrow:endrow, each=ncol(raster) * nrows)
			cells <- (as.integer(csteps * (rows - 1)) + cols)
			
			if (rm.NA) { vals <- tapply(values(raster), cells, function(x){fun(na.omit(x))} ) 
			} else { vals <- tapply(values(raster), cells, fun) }
			vals <- as.vector(vals)

			if (filename(outraster) == '') {
				v <- c(v, vals)
			} else {
				outraster <- set.values.row(outraster, vals, r)
				outraster <- write.row(outraster, overwrite)
			}
		} 
		if (filename(outraster) == '') { outraster <- set.values(outraster, v) }
	}
	return(outraster)
}


