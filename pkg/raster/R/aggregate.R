# Authors: Robert J. Hijmans and Jacob van Etten
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3



setMethod('aggregate', signature(x='RasterLayer'), 

function(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename="", ...)  {

	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)
	
	if (length(fact)==1) {
		fact <- as.integer(round(fact))
		if (fact < 2) { stop('fact should be > 1') }
		xfact <- yfact <- fact
	} else if (length(fact)==2) {
		xfact <- as.integer(round(fact[[1]]))
		yfact <- as.integer(round(fact[[2]]))
		if (xfact < 2) { stop('fact[[1]] should be > 1') } 
		if (yfact < 2) { stop('fact[[2]] should be > 1') }
	} else {
		stop('length(fact) should be 1 or 2')
	}
	if (xfact > ncol(x)) {warning('aggregation factor is larger than the number of columns') }
	if (yfact > nrow(x)) {warning('aggregation factor is larger than the number of rows')}

	if (expand) {
		rsteps <- as.integer(ceiling(nrow(x)/yfact))
		csteps <- as.integer(ceiling(ncol(x)/xfact))
	} else 	{
		rsteps <- as.integer(floor(nrow(x)/yfact))
		csteps <- as.integer(floor(ncol(x)/xfact))
	}
	
	ymn <- ymax(x) - rsteps * yfact * yres(x)
	xmx <- xmin(x) + csteps * xfact * xres(x)
		
	outRaster <- raster(x, filename)
	dataType(outRaster) <- datatype
	bndbox <- newExtent(xmin(x), xmx, ymn, ymax(x))
	outRaster <- setExtent(outRaster, bndbox, keepres=FALSE)
	rowcol(outRaster) <- c(rsteps, csteps) 
	
	
	if (na.rm) {
		# this avoid warning messages 
		narmfun <- function(x) { 
			x <- na.omit(x)
			if (length(x) == 0) { 
				return(NA)
			} else { 
				return( fun(x) )
			}
		}
	}
	
	if (dataContent(x) == 'all') {	
		cols <- rep(rep(1:csteps, each=xfact)[1:ncol(x)], times=nrow(x))
		rows <- rep(1:rsteps, each=ncol(x) * yfact)[1:ncell(x)]
		cells <- cellFromRowCol(x, rows, cols)
		
		if (na.rm) {
			outRaster <- setValues(outRaster, as.vector( tapply(values(x), cells, narmfun ))) 
		} else {
			outRaster <- setValues(outRaster, as.vector(tapply(values(x), cells, fun))) 
		}
		if (outRaster@file@name != "") {
			outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype)
		}

	} else if ( dataSource(x) == 'disk') { 
		if (!canProcessInMemory(outRaster, 2) && filename == '') {
			filename <- rasterTmpFile()
			filename(outRaster) <- filename
			if (getOption('verbose')) { cat('writing raster to:', filename(outRaster))	}						
		}
		
		cols <- rep(rep(1:csteps,each=xfact)[1:ncol(x)], times=yfact)
		rows <- rep(1, each=(ncol(x) * yfact))
		v <- vector(length=0)

		cells <- cellFromRowCol(x, rows, cols)
		nrows = yfact

		starttime <- proc.time()
		pb <- .setProgressBar(rsteps, type=.progress(...))
		for (r in 1:rsteps) {
			startrow <- 1 + (r - 1) * yfact
			if ( r==rsteps) {
				endrow <- min(nrow(x), startrow + yfact - 1)
				nrows <- endrow - startrow + 1
				theserows <- (startrow * rows)[1:(ncol(x)*nrows)]
				cols <- cols[1:(ncol(x)*nrows)]
				cells <- cellFromRowCol(x, theserows, cols)
			}	
			x <- readRows(x, startrow = startrow, nrows = nrows)
			
			if (na.rm) { 
				vals <- tapply(values(x), cells, narmfun ) 
			} else { 
				vals <- tapply(values(x), cells, fun) 
			}
			vals <- as.vector(vals)

			if (outRaster@file@name == "") {
				v <- c(v, vals)
			} else {
				outRaster <- setValues(outRaster, vals, r)
				outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype)
			}
		
			.doProgressBar(pb, r) 
		} 
		.closeProgressBar(pb, starttime)
		if (outRaster@file@name == "") { 
			outRaster <- setValues(outRaster, v) 
		}
	}
	return(outRaster)
}

)
