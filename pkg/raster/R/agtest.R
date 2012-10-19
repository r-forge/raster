
.aggtest <- function(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename="", padNA=TRUE, doC=FALSE, ...)  {


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
		stop('length() should be 1 or 2')
	}
	if (xfact > ncol(x)) {
		warning('aggregation factor is larger than the number of columns') 
		xfact <- ncol(x)
	}
	if (yfact > nrow(x)) {
		warning('aggregation factor is larger than the number of rows')
		yfact <- nrow(x)
	}

	ncx <- ncol(x)
	nrx <- nrow(x)
	if (expand) {
		rsteps <- as.integer(ceiling(nrx/yfact))
		csteps <- as.integer(ceiling(ncx/xfact))
		lastcol <- x@ncols
		lastrow <- x@nrows
		#addcols <- csteps * xfact - ncx
		#addrows <- rsteps * yfact - nrx

	} else 	{
		rsteps <- as.integer(floor(nrx/yfact))
		csteps <- as.integer(floor(ncx/xfact))
		lastcol <- min(csteps * xfact, x@ncols)
		lastrow <- min(rsteps * yfact, x@nrows)
	}
	
	
	ymn <- ymax(x) - rsteps * yfact * yres(x)
	xmx <- xmin(x) + csteps * xfact * xres(x)
		
	nl <- nlayers(x)
	if (nl > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)		
	}
	extent(out) <- extent(xmin(x), xmx, ymn, ymax(x))
	dim(out) <- c(rsteps, csteps) 
	names(out) <- names(x)
	ncout <- ncol(out)

	if (! hasValues(x) ) {	return(out) }	
	
	if (nl < 2) {	

	
		fun <- raster:::.makeTextFun(fun)
	
	
		if (!doC & class(fun) == 'character') { 
			rowcalc <- TRUE 
			fun <- raster:::.getColFun(fun)
		} else { 
			rowcalc <- FALSE 
		}

		if ( canProcessInMemory(x)) {

			if (doC) {
				op <- as.integer(match(fun, c('sum', 'mean', 'min', 'max')) - 1)
				dim=c(dim(x)[1:2], dim(out)[1:2], xfact, yfact)
				values(out) <- .Call("aggregate", 
					as.double(getValues(x)), 
					op, 
					as.integer(na.rm), 
					as.integer(dim), PACKAGE='raster')
				return(out)	
			}
			
			m <- ceiling(nrx / yfact)
			vv <- matrix(NA, nrow= yfact*xfact, ncol=csteps * m)
			vend <- 0
			vvstart <- 1
			yf <- nrx %% yfact
			vals <- getValues(x)
			for (j in 1:m) {
				if (j == m & yf > 0) {
					vstart <- vend + 1
					vend <- vend + (lastcol * yf)
					mv <- matrix(vals[vstart:vend], nrow=yf, byrow=TRUE )
					temp <- matrix(nrow=yf*xfact, ncol=csteps)
					temp[1:length(mv)] <- mv
					cols <- 1:(csteps) + (m-1) * csteps
					vv[1:nrow(temp), cols] <- temp
							
				} else {
					vstart <- vend + 1
					vend <- vend + (lastcol * yfact)
					mv <- matrix(vals[vstart:vend], nrow=yfact, byrow=TRUE )
							
					vv[vvstart:(vvstart+length(mv)-1)] <- as.vector(mv)
					vvstart <- vvstart + ncout*nrow(vv)
				}
			}
			if (rowcalc) {
				vals <- fun(vv, na.rm=na.rm )
			} else {
				vals <- apply(vv, 2, fun, na.rm=na.rm )
			}
			out <- setValues(out, as.vector(vals))
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
			return(out)
		
		} else {
			if (filename == '') { 
				filename <- rasterTmpFile() 
			}
			out <- writeStart(out, filename=filename, ...)
			
			tr <- blockSize(x, minrows=yfact)
			st <- round(tr$nrows[1] / yfact) * yfact
			tr$n <- ceiling(lastrow / st)
			tr$row <- c(1, cumsum(rep(st, tr$n-1))+1)
			tr$nrows <- rep(st, tr$n)
			if (expand) {
				tr$nrows[tr$n] <-  tr$nrows[tr$n] - (sum(tr$nrows)-x@nrows)
			}
			pb <- pbCreate(tr$n, ...)
			
		#vv <- matrix(ncol= csteps * yfact, nrow=rsteps * xfact)
		#vv <- matrix(nrow= yfact * xfact, ncol=csteps)
			m <- tr$nrows[1] / yfact
			vv <- matrix(NA, nrow= yfact*xfact, ncol=csteps * m)
		
			w <- getOption('warn')
			on.exit(options('warn' = w))
			options('warn'=-1) 
		

			if (doC) {
				op <- as.integer(match(fun, c('sum', 'mean', 'min', 'max')) - 1)
				stopifnot(!is.na(op))
				dim <- c(dim(x)[1:2], dim(out)[1:2], xfact, yfact)
				for (i in 1:(tr$n-1)) {
					vals <- getValuesBlock(x, tr$row[i], tr$nrows[i], 1, lastcol)
					vals <- .Call("aggregate", as.double(vals), op,
							as.integer(na.rm), as.integer(dim), PACKAGE='raster')
					out <- writeValues(out, vals, tr$row[i])
					pbStep(pb, i) 
				}
				pbClose(pb)
				out <- writeStop(out)
				return(out)
			}


			for (i in 1:(tr$n-1)) {
				
				vals <- getValuesBlock(x, tr$row[i], tr$nrows[i], 1, lastcol)
					
				vend <- 0
				vvstart <- 1
				for (j in 1:m) {
					vstart <- vend + 1
					vend <- vend + (lastcol * yfact)
					mv <- matrix(vals[vstart:vend], nrow=yfact, byrow=TRUE )
						
					vv[vvstart:(vvstart+length(mv)-1)] <- as.vector(mv)			
					vvstart <- vvstart + ncout*nrow(vv)
				}
				if (rowcalc) {
					vals <- fun(vv, na.rm=na.rm )
				} else {
					vals <- apply(vv, 2, fun, na.rm=na.rm )
				}
				out <- writeValues(out, vals, tr$row[i])
				pbStep(pb, i) 
			} 

	#	if (i==tr$n) { 
			m <- ceiling(tr$nrows[i] / yfact)
			vv <- matrix(NA, nrow= yfact*xfact, ncol=csteps * m)
			vend <- 0
			vvstart <- 1
			yf <- tr$nrows[i] %% yfact
			for (j in 1:m) {
				if (j == m & yf > 0) {
					vstart <- vend + 1
					vend <- vend + (lastcol * yf)
					mv <- matrix(vals[vstart:vend], nrow=yf, byrow=TRUE )
					temp <- matrix(nrow=yf*xfact, ncol=csteps)
					temp[1:length(mv)] <- mv
					cols <- 1:(csteps) + (m-1) * csteps
					vv[1:nrow(temp), cols] <- temp
					
				} else {
					vstart <- vend + 1
					vend <- vend + (lastcol * yfact)
					mv <- matrix(vals[vstart:vend], nrow=yfact, byrow=TRUE )
					
					vv[vvstart:(vvstart+length(mv)-1)] <- as.vector(mv)
					vvstart <- vvstart + ncout*nrow(vv)
				}
			}
			if (rowcalc) {
				vals <- fun(vv, na.rm=na.rm )
			} else {
				vals <- apply(vv, 2, fun, na.rm=na.rm )
			}
			pbStep(pb, i) 
			out <- writeValues(out, vals, tr$row[i])
			pbClose(pb)
			out <- writeStop(out)
			return(out)
		}
	} else { # nlayers > 1
	
		
		if (canProcessInMemory(x, nlayers(x)+2)) {
		
			xx <- raster(x)		
			x <- getValues(x)
			cols <- rep(rep(1:csteps, each=xfact)[1:ncol(xx)], times=nrow(xx))
			rows <- rep(1:rsteps, each=ncol(xx) * yfact)[1:ncell(xx)]
			cells <- cellFromRowCol(xx, rows, cols)
			
			x <- as.matrix( aggregate(x, list(cells), fun, na.rm=na.rm ))[,-1]
			rm(cells)
			
			x <- setValues(out, x)
			if (filename != "") {
				x <- writeRaster(x, filename=filename, ...)
			}
			return(x)

		} else  { 
		
			cols <- rep(rep(1:csteps,each=xfact)[1:ncol(x)], times=yfact)
			rows <- rep(1, each=(ncol(x) * yfact))
			
			out <- writeStart(out, filename=filename, ...)
			
			cells <- cellFromRowCol(x, rows, cols)
			nrows = yfact

			w <- getOption('warn')
			on.exit(options('warn' = w))
			options('warn'=-1) 
			
			pb <- pbCreate(rsteps, ...)
			for (r in 1:rsteps) {
				startrow <- 1 + (r - 1) * yfact
				if ( r==rsteps) {
					endrow <- min(nrow(x), startrow + yfact - 1)
					nrows <- endrow - startrow + 1
					theserows <- (startrow * rows)[1:(ncol(x)*nrows)]
					cols <- cols[1:(ncol(x)*nrows)]
					cells <- cellFromRowCol(x, theserows, cols)
				}	
				vals <- getValues(x, startrow, nrows)
				vals <- as.matrix( aggregate(vals, list(cells), fun, na.rm=na.rm ))[,-1]
			
				out <- writeValues(out, vals, r)
				pbStep(pb, r) 
			} 
			pbClose(pb)
			out <- writeStop(out)
			return(out)
		}
	
	}
}




#library(raster)
#r <- raster(nc=9, nr=9)
#r <- raster()
#r[] = 1:ncell(r)
#raster:::.aggtest(r, 5, 'min', doC=T)




	