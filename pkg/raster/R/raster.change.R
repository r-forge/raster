# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,2
# Licence GPL v3


#r.resample <- function(raster, xmin, xmax, ymin, ymax, ncols, nrows, method="bilinear", filename="", overwrite=FALSE) {
#	stop("sorry, not implemented yet")
#	if (raster@data@content == 'all')  {
#	} else if (raster@data@source == 'disk')  {
#	}
#}



r.expand <- function(raster, boundingbox, filename="", overwrite=FALSE) {
	bbox <- boundingbox(boundingbox)
	res <- resolution(raster)
# snap points to pixel boundaries
	xmn <- round(bbox[1,1] / res[1]) * res[1]
	xmx <- round(bbox[1,2] / res[1]) * res[1]
	ymn <- round(bbox[2,1] / res[2]) * res[2]
	ymx <- round(bbox[2,2] / res[2]) * res[2]
	
# only expanding here, not cutting
	xmn <- min(xmn, xmin(raster))
	xmx <- max(xmx, xmax(raster))
	ymn <- min(ymn, ymin(raster))
	ymx <- max(ymx, ymax(raster))
	
	outraster <- set.raster(raster, filename)
	outraster <- set.bbox(outraster, xmn, xmx, ymn, ymx, keepres=T)

	startrow <- get.row.from.y(outraster, ymax(raster))
	startcol <- get.col.from.x(outraster, xmin(raster))
	
	if (data.content(raster) == 'all')  {
		d <- vector(length=ncells(outraster))
		d[] <- NA
		for (r in 1:nrow(raster)) {
			v <- values.row(raster, r) 
			startcell <- (r + startrow -2) * ncol(outraster) + startcol
			d[startcell:(startcell+ncol(raster)-1)] <- v
			outraster <- set.values(outraster, d)
		}
	} else if (data.source(raster) == 'disk')  {
		if (filename == "") {stop('invalid filename')}
		d <- vector(length=ncol(outraster))
		for (r in 1:nrow(raster)) {
			raster <- read.row(raster, r)
			v <- values(raster)
			d[] <- NA
			startcell <- (r + startrow -2) * ncol(outraster) + startcol
			d[startcell:(startcell+ncol(raster)-1)] <- v
			outraster <- set.values.row(outraster, d, r)
			outraster <- write.raster(outraster)
		}
	}
	return(outraster)
}




r.merge <- function(rasters, filename="", overwrite=FALSE) {
	res <- compare(rasters, rowcol=FALSE)
	
	for (i in 1:length(rasters)) {
		if (!(data.source(rasters[[i]]) == 'disk' | data.content(rasters[[i]]) == 'all' | data.content(rasters[[i]]) == 'sparse')) { 
			stop('rasters should be stored on disk or values should be in memory') 
		}
	}
	bb <- bbox(rasters[[1]])
	for (i in 2:length(rasters)) {
		bb2 <- bbox(rasters[[i]])
		bb[,1] <- pmin(bb[,1], bb2[,1])
		bb[,2] <- pmax(bb[,2], bb2[,2])
	}
	outrs <- set.raster(rasters[[1]], filename)
	outrs <- set.bbox(outrs, bb[1,1], bb[1,2], bb[2,1], bb[2,2], keepres=TRUE)

	rowcol <- matrix(0, ncol=3, nrow=length(rasters))
	for (i in 1:length(rasters)) {
		xy1 <- get.xy.from.cell(rasters[[i]], 1) # first row/col on old raster[[i]]
		xy2 <- get.xy.from.cell(rasters[[i]], ncells(rasters[[i]]) ) #last row/col on old raster[[i]]
		rowcol[i,1] <- get.row.from.y(outrs, xy1[2]) #start row on new raster
		rowcol[i,2] <- get.row.from.y(outrs, xy2[2]) #end row
		rowcol[i,3] <- get.col.from.x(outrs, xy1[1]) #start col
	}
	v <- vector(length=0)
	for (r in 1:nrow(outrs)) {
		rd <- as.vector(matrix(NA, nrow=1, ncol=ncol(outrs))) 
		for (i in length(rasters):1) {  #reverse order so that the first raster covers the second etc.
			if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
				if (rasters[[i]]@data@source == 'disk') {
					rasters[[i]] <- read.row(rasters[[i]], r + 1 - rowcol[i,1]) 
					d <- values(rasters[[i]])
				} else {
					d <- values.row(rasters[[i]], r + 1 - rowcol[i,1]) 
				}
				id2 <- seq(1:ncol(rasters[[i]])) + rowcol[i,3] - 1
				d <- cbind(id2, d)
				d <- na.omit(d)
				rd[d[,1]] <- d[,2]
			}		
		}
		if (filename != '') {
			outrs <- set.values.row(outrs, rd, r)
			outrs <- write.row(outrs, overwrite)
		} else {
			v <- c(v, rd)
		}
	}
	if (filename == '') { outrs <- set.values(outrs, v) }
	return(outrs)
}


r.cut <- function(raster, boundingbox, filename="", overwrite=FALSE) {
# we could also allow the raster to expand but for now let's not and first make a separate expand function
	bbox <- boundingbox(boundingbox)

	xmn <- max(bbox[1,1], xmin(raster))
	xmx <- min(bbox[1,2], xmax(raster))
	ymn <- max(bbox[2,1], ymin(raster))
	ymx <- min(bbox[2,2], ymax(raster))
	
	if (xmn == xmx) {stop("xmin and xmax are less than one cell apart")}
	if (ymn == ymx) {stop("ymin and ymax are less than one cell apart")}
	
	outraster <- set.raster(raster, filename)
	outraster <- set.bbox(outraster, xmn, xmx, ymn, ymx, keepres=T)
	
	if (data.content(raster) == 'all')  {
		first.start.cell <- get.cell.from.xy(raster, c(xmn + 0.5 * xres(raster), ymx - 0.5 * yres(raster) ))	
		last.start.cell <- get.cell.from.xy(raster, c(xmn + 0.5 * xres(raster), ymn + 0.5 * yres(raster) ))
		start.cells <- seq(first.start.cell, last.start.cell, by = ncol(raster))
		end.cells <- start.cells + ncol(outraster) - 1
		selected.cells <- unlist(mapply(seq, start.cells, end.cells))
		outraster <- set.values(outraster, values(raster)[selected.cells])
		outraster <- set.minmax(outraster)
		if (nchar(filename(outraster)) > 0 ) { outraster <- try(write.raster(outraster)) }		
	} else if (data.content(raster) == 'disk')  {
		first.col <- get.col.from.x(raster, xmn + 0.5 * xres(outraster))
		first.row <- get.row.from.y(raster, ymx - 0.5 * yres(outraster))
		last.row <- first.row + outraster@nrows - 1
		rownr <- 1
		for (r in first.row:last.row) {
			raster <- read.part.of.row(raster, r, first.col, ncol(outraster) )
			if (filename(outraster) == '') {
				if (r == first.row) {
					outraster <- set.values(values(raster))
				} else {
					outraster <- set.values(c(values(outraster), values(raster)))
				}		
			} else {
				outraster <- set.values.row(outraster, values(raster), rownr)
				outraster <- write.row(outraster, overwrite)
			}	
			rownr <- rownr + 1
		} 
	}
	return(outraster)
}


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
	
	outrs <- set.raster(raster, filename)
	outrs <- set.rowcol(outrs, nrow(raster) * yfact, ncol(raster) * xfact) 
	if ( data.content(raster)=='all') {
		
		cols <- rep(rep(1:ncol(raster), each=xfact), times=nrow(raster)*yfact)
		rows <- rep(1:nrow(raster), each=ncol(raster)*xfact*yfact)
		cells <- get.cell.from.rowcol(raster, rows, cols)
		outrs <- set.values(outrs, values(raster)[cells])
		if (filename != "") {write.raster(outrs)}
		
	} else if ( data.source(raster) == 'disk') { 

		cols <- rep(1:ncol(raster), each=xfact)
		for (r in 1:nrow(raster)) {
			raster <- read.row(raster, r)
			for (i in 1:yfact) {
				outrs <- set.values.row(outrs, values(raster)[cols], (r-1) * xfact + i)
				outrs <- write.row(outrs)
			}	
		}
	}
	return(outrs)
}


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

		if (nchar(filename(outraster)) > 0 ) { 
			outraster <- try(write.raster(outraster))
		}

	} else if (data.source(raster) == 'disk') {
	
		cols <- rep(rep(1:csteps,each=xfact)[1:ncol(raster)], times=xfact)
		newdata <- vector(length=rsteps*csteps)
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
				if (r == 1) {
					allvals <- vals
				} else {
					allvals <- c(allvals, vals)
				}		
				if (r == rsteps) {
					outraster <- set.values(outraster, allvals)
				}
			} else {
				outraster <- set.values.row(outraster, vals, r)
				outraster <- write.row(outraster, overwrite)
			}	
		} 			
	}
	return(outraster)
}


