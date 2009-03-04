# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3


setMethod('aggregate', signature(x='RasterLayer'), 

function(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename=NULL, filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1)  {

	if (is.null(filename)) { filename <- "" }

	if (length(fact)==1) {
		fact <- as.integer(round(fact))
		if (fact < 2) { stop('fact should be > 1') }
		xfact <- yfact <- fact
	} else if (length(fact)==2) {
		xfact <- as.integer(round(fact[[1]]))
		yfact <- as.intger(round(fact[[2]]))
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
		
	outRaster <- setRaster(x, filename)
	outRaster <- setDatatype(outRaster, datatype)
	bndbox <- newBbox(xmin(x), xmx, ymn, ymax(x))
	outRaster <- setBbox(outRaster, bndbox, keepres=F)
	outRaster <- setRowCol(outRaster, nrows=rsteps, ncols=csteps) 
	
	if (dataContent(x) == 'all') {	
		cols <- rep(rep(1:csteps, each=xfact)[1:ncol(x)], times=nrow(x))
		rows <- rep(1:rsteps, each=ncol(x) * yfact)[1:ncell(x)]
		cells <- cellFromRowCol(x, rows, cols)
		
		if (na.rm) {
			outRaster <- setValues(outRaster, as.vector( tapply(values(x), cells, function(x){fun(na.omit(x))}))) 
		} else {
			outRaster <- setValues(outRaster, as.vector(tapply(values(x), cells, fun))) 
		}
		if (filename(outRaster) != "") {
			outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype, datatype=datatype)
		}

	} else if ( dataSource(x) == 'disk') { 
		if (!.CanProcessInMemory(x, 2) && filename == '') {
			filename <- tempfile()
			outraster <- setFilename(outraster, filename )
			if (options('verbose')[[1]]) { cat('writing raster to:', filename(raster))	}						
		}
		starttime <- proc.time()
		
		cols <- rep(rep(1:csteps,each=xfact)[1:ncol(x)], times=yfact)
		rows <- rep(1, each=(ncol(x) * yfact))
		v <- vector(length=0)
		for (r in 1:rsteps) {
			startrow <- 1 + (r - 1) * yfact
			if ( r==rsteps) {
				endrow <- min(nrow(x), startrow + yfact - 1)
				nrows <- endrow - startrow + 1
				theserows <- (startrow * rows)[1:(ncol(x)*nrows)]
				cols <- cols[1:(ncol(x)*nrows)]
			} else {
				nrows = yfact
				theserows <- startrow * rows
			}	
			x <- readRows(x, startrow = startrow, nrows = nrows)
			cells <- cellFromRowCol(x, theserows, cols)
			
			if (na.rm) { 
				vals <- tapply(values(x), cells, function(x){fun(na.omit(x))} ) 
			} else { 
				vals <- tapply(values(x), cells, fun) 
			}
			vals <- as.vector(vals)

			if (filename(outRaster) == "") {
				v <- c(v, vals)
			} else {
				outRaster <- setValues(outRaster, vals, r)
				outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype, datatype=datatype)
			}
			
			if (r %in% track) {
				elapsed <- (proc.time() - starttime)[3]
				tpr <- elapsed /r
				ttg <- round(tpr/60 * (nrow(raster) - r), digits=1)
				cat('row', r, '-', ttg, 'minutes to go\n')
			}			
		} 
		if (filename(outRaster) == "") { 
			outRaster <- setValues(outRaster, v) 
		}
	}
	return(outRaster)
}
)


