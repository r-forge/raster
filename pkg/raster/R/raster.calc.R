# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3

.add.history <- function(raster, message) {
	if (is.character(message) & message != "") {
		raster@history <- c(message, raster@history)
	}	
}


r.calc <- function(raster, fun=sqrt, filename=NA, overwrite=FALSE, INT=FALSE) {
	outraster <- set.raster(raster, filename)
	if (INT) {set.datatype(outraster, 'integer')}
	
	if (!(data.content(raster) == 'all' | data.content(raster) == 'sparse' | data.source(raster) == 'disk')) {
		stop('raster has no data on disk, nor a complete set of raster values in memory')
	}
	
# there is data	
	if ( data.content(raster) == 'all') {
		outraster <- set.values(outraster, fun(values(raster))) 
		if (!is.na(filename) ) { outraster <- write.raster(outraster, overwrite=overwrite)
		}
	} else if ( data.content(raster) == 'sparse') {
		outraster <- set.values.sparse(outraster, fun(values(raster)),  data.indices(raster)) 
		if (!is.na(filename) ) { outraster <- write.raster(outraster, overwrite=overwrite)
		}
	} else if (data.source(raster) == 'disk') {
		for (r in 1:nrow(raster)) {
			raster <- read.row(raster, r)
			outraster <- set.values.row(outraster, fun(values(raster)), r)
			outraster <- write.row(outraster, overwrite=overwrite)
		}
	} else if (is.na(filename) ) {
		if ( data.content(raster) == 'row') {
			outraster <- set.values.row(outraster, fun(values(raster)), get.row.from.cell(raster,  data.indices(raster)[1])) 
		} else if ( data.content(raster) == 'block') {
			outraster <- set.values.block(outraster, fun(values(raster)),  data.indices(raster)[1],  data.indices(raster)[2])  
		}	
	}
	return(outraster)
}



r.init <- function(raster, fun=runif, filename=NA, overwrite=FALSE, INT=FALSE) {
	outraster <- set.raster(raster, filename)
	if (INT) {set.datatype(outraster, 'integer')}
	if ( data.content(raster) == 'all' | data.source(raster) == 'ram' ) {
		n <- ncells(raster)
		outraster <- set.values(outraster, fun(n)) 
		if (!is.na(filename)) {	outraster <- write.raster(outraster) }
	} else if (!is.na(filename)) {
		n <- length(ncol(raster))
		for (r in 1:nrow(raster)) {
			outraster <- set.values.row(outraster, fun(n), r) 
			outraster <- write.row(outraster, overwrite=overwrite)	
		}	
	} else {stop('cannot do')}
	return(outraster)
}


r.reclass <- function(raster, rclmat, filename=NA, overwrite=FALSE, INT=FALSE)  {
	if ( is.null(dim(rclmat)) ) { 
		rclmat <- matrix(rclmat, ncol=3, byrow=TRUE) 
	} else if ( dim(rclmat)[2] == 1 ) { 
		rclmat <- matrix(rclmat, ncol=3, byrow=TRUE) }
	if ( dim(rclmat)[2] != 3 ) { stop('rclmat must have 3 columns') }
	colnames(rclmat) <- c("From", "To", "Becomes")	
	print(rclmat)
	outraster <- set.raster(raster, filename)
	if (INT) { 
		outraster <- set.datatype(outraster, "integer") 
		res <- vector(mode = "integer", length = ncol(raster))
	} else { 
		outraster <- set.datatype(outraster, "numeric") 
		res <- vector(mode = "numeric", length = ncol(raster))
	}
	if ( data.content(raster) == 'all' |  data.content(raster) == 'sparse') {
		for (i in 1:length(rclmat[,1])) {
			if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
				res[ is.na(values(raster)) ] <- rclmat[i, 3] 
			} else { 
				res[ (values(raster) > rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
			}
		}
		if ( data.content(raster) == 'all') { outraster <- set.values(outraster, res) }
		if ( data.content(raster) == 'sparse') { outraster <- set.values.row(outraster, res,  data.indices(raster)) }
		if (!is.na(filename)) {	outraster <- write.raster(outraster) }
	} else {
		for (r in 1:nrow(raster)) {
			raster <- read.row(raster, r)
			for (i in 1:length(rclmat[,1])) {
				if (is.na(rclmat[i,1]) | is.na(rclmat[i,2])) {
					res[ is.na(values(raster)) ] <- rclmat[i, 3] 
				} else if (is.na(rclmat[i,1]) == is.na(rclmat[i,2])) {
					res[ values(raster) == rclmat[i,1] ] <- rclmat[i , 3] 
				} else {
					res[ (values(raster) > rclmat[i,1]) & (values(raster) <= rclmat[i,2]) ] <- rclmat[i , 3] 
				}
			}	
		}
		outraster <- set.values.row(outraster, res, r)
		outraster <- write.row(outraster, overwrite=overwrite)
	}	
	return(outraster)
}


r.isNA <- function(raster, value=0, filename=NA, overwrite=FALSE, INT=FALSE) {
	fun <- function(x) { x[is.na(x)] <- value; return(x)} 
	raster <- r.calc(raster, fun, filename, overwrite=overwrite, INT=INT)
	return(raster) 
}

	
r.setNA <- function(raster, operator= "<=", value=0, filename=NA, overwrite=FALSE, INT=FALSE) {
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
	


r.neighborhood <- function(raster, fun=mean, filename=NA, ngb=3, keepdata=TRUE, overwrite=FALSE) {
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
		rowdata <- values(read.row(raster, r))
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
	

