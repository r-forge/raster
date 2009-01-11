# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,7
# Licence GPL v3


.calc.ngb <- function(rows, ngb, fun, keepdata) {
	lim <- floor(ngb / 2)
	res <- vector(length=length(rows[1,]))
	lr <- length(rows[1,])
    for (i in 1:length(rows[1,])) {
		d <- rows[, max(1,(i-lim)):min((i+lim),lr)]
		d <- as.vector(d)
		dd <- as.vector(na.omit(d))
		if (length(dd) == 0) {
			res[i] <- NA
		} else if (keepdata) { 
			res[i] <- fun(dd)
		} else {
			if (length(dd) == length(d)) { 
				res[i] <- fun(d)
			} else {
				res[i] <- NA
			}
		}
	}	
	return(res)
}


.calc.ngb2 <- function(rows, ngb, fun, keepdata) {
#TODO, current function is very slow
	lim <- floor(ngb / 2)
	res <- array(dim=length(rows[1,]))
	addNA <- (matrix(ncol=lim, nrow=ngb))
	rows <- cbind(addNA, rows, addNA)
#	d <- rows[, max(1,(i-lim)):min((i+lim),lr)]
#	if (rm.NA) {outraster@data@values <- as.vector(tapply(raster@data,cell.index,function(x){fun(na.omit(x))}))}
#		else {outraster@data@values <- as.vector(tapply(raster@data,cell.index,fun))}
}
	

neighborhood <- function(raster, fun=mean, filename="", ngb=3, keepdata=TRUE, overwrite=FALSE, ForceIntOutput=FALSE) {
	ngb <- round(ngb)
	if ((ngb / 2) == floor(ngb/2)) { stop("only odd neighborhoods are supported") }
	if (ngb == 1) { stop("ngb should be 3 or larger")  } 
	lim <- floor(ngb / 2)
	
	filename <- trim(filename)
	ngbgrid <- setRaster(raster, filename)
	if (ForceIntOutput) {setDatatype(ngbgrid, 'integer') }

# first create an empty matrix with nrows = ngb and ncols = raster@ncols
	ngbdata1 <- array(data = NA, dim = c(ngb, ncol(raster)))
	ngbdata <- ngbdata1
	
	rr <- 1
	v <- vector(length=0)
	for (r in 1:nrow(raster)) {
		if (dataContent(raster)=='all') {
			rowdata <- valuesRow(raster, r)
		} else {	
			rowdata <- values(readRow(raster, r))
		}	
		ngbdata <- rbind(ngbdata[2:ngb,], t(rowdata))
		if (r > lim) {
			ngbvals <- .calc.ngb(ngbdata, ngb, fun, keepdata)
			if (filename != "") {
				ngbgrid <- setValues(ngbgrid, ngbvals, rr)
				ngbgrid <- writeRaster(ngbgrid, overwrite)
			} else {
				v <- c(v, ngbvals)
			}
			rr <- rr + 1
		}
	}
	
	ngbdata1 <- array(data = NA, dim = c(ngb, raster@ncols))
	for (r in (nrow(raster)+1):(nrow(raster)+lim)) {
		ngbdata <- rbind(ngbdata[2:ngb,], t(ngbdata1[1,]))
		ngbvals <- .calc.ngb(ngbdata, ngb, fun, keepdata)
		if (filename != "") {
			ngbgrid <- setValues(ngbgrid, ngbvals, rr)
			ngbgrid <- writeRaster(ngbgrid, overwrite)
		} else {
			v <- c(v, ngbvals)
		}
		rr <- rr + 1
	}
	if (filename == "") { 
		ngbgrid <- setValues(ngbgrid, v) 
	}
	return(ngbgrid)
}
	
