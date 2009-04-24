# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3


setMethod('aggregate', signature(x='RasterLayer'), 

function(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename=NULL, filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1, old=FALSE)  {

	if (old) { return(.aggregate_old(x,fact, fun, expand, na.rm, filetype, datatype, overwrite, track)) }

	if (is.null(filename)) { filename <- "" }

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
		# this avoid warning messages 
	narmfun <- function(x) { 
		x <- na.omit(x)
		if (length(x) == 0) { 
			return(NA)
		} else { 
			return( fun(x) )
		}
	}
	
	if (expand) {
		rsteps <- as.integer(ceiling(nrow(x)/yfact))
		csteps <- as.integer(ceiling(ncol(x)/xfact))
	} else {
		rsteps <- as.integer(floor(nrow(x)/yfact))
		csteps <- as.integer(floor(ncol(x)/xfact))
		nc <- csteps * xfact
		nr <- rsteps * yfact			
	}
	ymn <- ymax(x) - rsteps * yfact * yres(x)
	xmx <- xmin(x) + csteps * xfact * xres(x)
	outRaster <- raster(x, filename)
	dataType(outRaster) <- datatype
	bndbox <- newBbox(xmin(x), xmx, ymn, ymax(x))
	outRaster <- setExtent(outRaster, bndbox, keepres=FALSE)
	rowcol(outRaster) <- c(rsteps, csteps) 

	addcol <- 0
	addrow <- 0
	if (expand) {
		nc <- csteps * xfact
		nr <- rsteps * yfact
		if (nc > ncol(x)) { 
			csteps <- csteps - 1
			nc <- csteps * xfact
			addcol <- ncol(x) - nc
		}
		if (nr > nrow(x)) { 
			nr <- (rsteps-1) * yfact
			addrow <- nrow(x) - nr
		}
	}
	ncells <- xfact * yfact
	
	if (dataContent(x) == 'all' | dataSource(x) == 'disk') { 
		if (dataContent(x) == 'all') { 
			mem <- TRUE 
			ncolumns <- ncol(x)
		} else {
			mem <- FALSE
		}
		if (!canProcessInMemory(x, 2) && filename == '') {
			filename <- tempfile()
			filename(outraster) <- filename
			if (options('verbose')[[1]]) { cat('writing raster to:', filename(raster))	}						
		}
		starttime <- proc.time()
		v <- vector(length=0)
		newcols <- ncol(outRaster)
		vals <- vector(length=newcols)
		nrows = yfact

		for (r in 1:rsteps) {
			startrow <- 1 + (r - 1) * yfact
			if (r==rsteps & addrow > 0) {
				nrows <- addrow
				ncells <- xfact * nrows
			}
			if (mem) {
				firstcell <- (startrow-1) * ncolumns + 1
				lastcell <- firstcell + ncolumns * nrows - 1
				a <- matrix(x@data@values[firstcell:lastcell], nrow=nrows, byrow=T)
			} else {
				x <- readRows(x, startrow = startrow, nrows = nrows)
				a <- matrix(x@data@values, nrow=nrows, byrow=T)
			}
			
			if (addcol > 0) {
				b <- a[,(nc+1):(nc+addcol)] 
				a <- a[,1:nc]
			}
			a <- matrix(as.vector(a), nrow=ncells)
			if (na.rm) { 
				for (i in 1:csteps) {
					vals[i] <- narmfun(a[,i])
				}
#				vals <- apply(a, 2, narmfun ) 
			} else { 
#				vals <- apply(a, 2, fun) 
				for (i in 1:csteps) {
					vals[i] <- fun(a[,i])
				}
			}
			if (addcol > 0) {
				if (na.rm) { 	
					vals[newcols] <- narmfun(b)
				} else {
					vals[newcols] <- fun(b)			
				}
			}
			if (outRaster@file@name == "") {
				v <- c(v, vals)
			} else {
				outRaster <- setValues(outRaster, vals, r)
				outRaster <- writeRaster(outRaster, overwrite=overwrite, filetype=filetype)
			}			
			if (r %in% track) { .showTrack(r, outRaster@nrows, track, starttime) }
		} 

		if (outRaster@file@name == "") { 
			outRaster <- setValues(outRaster, v) 
		}
	}
	return(outRaster)
}

)