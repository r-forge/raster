# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,5
# Licence GPL v3



r.calc <- function(raster, fun=sqrt, filename="", overwrite=FALSE, INT=FALSE) {
	outraster <- set.raster(raster, filename)
	if (INT) {set.datatype(outraster, 'integer')}
	
	if (!(dataContent(raster) == 'all' | dataContent(raster) == 'sparse' | dataSource(raster) == 'disk')) {
		stop('raster has no data on disk, nor a complete set of raster values in memory')
	}
	
	if ( dataContent(raster) == 'all') {
		outraster <- set.values(outraster, fun(values(raster))) 
		if (filename(outraster)!="") { outraster <- write.raster(outraster, overwrite=overwrite)
		}
	} else if ( dataContent(raster) == 'sparse') {
		outraster <- set.values.sparse(outraster, fun(values(raster)),  dataIndices(raster)) 
		if (filename(outraster) != "") { outraster <- write.raster(outraster, overwrite=overwrite)
		}
	} else if (dataSource(raster) == 'disk') {
		v <- vector(length=0)
		for (r in 1:nrow(raster)) {
			raster <- readRow(raster, r)
			if (filename(outraster)=="") {
				v <- c(v, fun(values(raster)))
			} else {
				outraster <- set.values.row(outraster, fun(values(raster)), r)
				outraster <- write.row(outraster, overwrite=overwrite)
			}
		}
		if (filename(outraster) == "") { outraster <- set.values(outraster, v) }
	}
	return(outraster)
}



r.init <- function(raster, fun=runif, filename="", overwrite=FALSE, INT=FALSE) {
	outraster <- set.raster(raster, filename)
	if (INT) {set.datatype(outraster, 'integer')}

	if ( dataContent(raster) == 'all' | dataSource(raster) == 'ram' ) {
		n <- ncells(raster)
		outraster <- set.values(outraster, fun(n)) 
		if (!is.na(filename)) {	outraster <- write.raster(outraster) }
		
	} else if (dataSource(raster) == 'disk') {
		n <- length(ncol(raster))
		v <- vector(length=0)

		for (r in 1:nrow(raster)) {
			if (filename(outraster)=="") {
				v <- c(v, fun(n))
			} else {			
				outraster <- set.values.row(outraster, fun(n), r) 
				outraster <- write.row(outraster, overwrite=overwrite)
			}	
		}	
		if (filename(outraster) == '') { outraster <- set.values(outraster, v) }
	} 
	return(outraster)
}


r.isNA <- function(raster, value=0, filename="", overwrite=FALSE, INT=FALSE) {
	fun <- function(x) { x[is.na(x)] <- value; return(x)} 
	raster <- r.calc(raster, fun, filename, overwrite=overwrite, INT=INT)
	return(raster) 
}

	
r.setNA <- function(raster, operator= "<=", value=0, filename="", overwrite=FALSE, INT=FALSE) {
	if (operator == ">") { fun <- function(x) { x[x>value] <- NA; return(x)}
	} else if (operator == "<") { fun <- function(x) { x[x<value] <- NA; return(x)}
	} else if (operator == "<=") { fun <- function(x) { x[x<=value] <- NA; return(x)}
	} else if (operator == ">=") { fun <- function(x) { x[x>=value] <- NA; return(x)}
	} else if (operator == "==") { fun <- function(x) { x[x==value] <- NA; return(x)}
	} else if (operator == "!=") { fun <- function(x) { x[x!=value] <- NA; return(x)}
	}
	return(r.calc(raster, fun, filename, overwrite=overwrite, INT=INT))
}


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
#TODO
	lim <- floor(ngb / 2)
	res <- array(dim=length(rows[1,]))
	addNA <- (matrix(ncol=lim, nrow=ngb))
	rows <- cbind(addNA, rows, addNA)
#	d <- rows[, max(1,(i-lim)):min((i+lim),lr)]
#	if (rm.NA) {outraster@data@values <- as.vector(tapply(raster@data,cell.index,function(x){fun(na.omit(x))}))}
#		else {outraster@data@values <- as.vector(tapply(raster@data,cell.index,fun))}
}
	


r.neighborhood <- function(raster, fun=mean, filename="", ngb=3, keepdata=TRUE, overwrite=FALSE) {
	ngb <- round(ngb)
	if ((ngb / 2) == floor(ngb/2)) { stop("only odd neighborhoods are supported") }
	if (ngb == 1) { stop("ngb should be 3 or larger")  } 
	lim <- floor(ngb / 2)
	
	ngbgrid <- set.filename(raster, filename)
	
# first create an empty matrix with nrows = ngb and ncols = raster@ncols

	ngbdata1 <- array(data = NA, dim = c(ngb, raster@ncols))
	ngbdata <- ngbdata1
	
	rr <- 1
	for (r in 1:nrow(raster)) {
		rowdata <- values(readRow(raster, r))
		ngbdata <- rbind(ngbdata[2:ngb,], t(rowdata))
		if (r > lim) {
			ngbgrid <- set.values.row(ngbgrid, .calc.ngb(ngbdata, ngb, fun, keepdata), rr)
			ngbgrid <- write.row(ngbgrid, overwrite)
			rr <- rr + 1
		}
	}

	ngbdata1 <- array(data = NA, dim = c(ngb, raster@ncols))
	for (r in (nrow(raster)+1):(nrow(raster)+lim)) {
		ngbdata <- rbind(ngbdata[2:ngb,], t(ngbdata1[1,]))
		ngbgrid <- set.values.row(ngbgrid, .calc.ngb(ngbdata, ngb, fun, keepdata), rr)
		ngbgrid <- write.row(ngbgrid, overwrite)
		rr <- rr + 1
	}
	return(ngbgrid)
}
	

