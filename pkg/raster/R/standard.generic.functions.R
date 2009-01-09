# Authors: Robert J. Hijmans, r.hijmans@gmail.com and Jacob van Etten
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3




setMethod('==', signature(e1='AbstractRaster', e2='AbstractRaster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), origin=TRUE, resolution=TRUE, rowcol=TRUE, projection=TRUE, slack=0.01, stopiffalse=FALSE) 
#		c1 <- identical(ncol(e1), ncol(e2))
#		c2 <- identical(nrow(e1), nrow(e2))
#		c3 <- identical(boundingbox(e1), boundingbox(e2))
#		c4 <- identical(projection(e1),projection(e2))
#		cond <- c1 & c2 & c3 & c4
		return(cond)
	}
)	

setMethod('!=', signature(e1='AbstractRaster', e2='AbstractRaster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), origin=TRUE, resolution=TRUE, rowcol=TRUE, projection=TRUE, slack=0.01, stopiffalse=FALSE) 
		return(!cond)
	}
)	


.getValues <- function(x) {
# need to take care of 'spase'
	if (dataContent(x) != 'all') {
		if (dataSource(x) == 'ram') {
			stop('no data on disk or in memory')
		} else {
			x <- readAll(x)
		}	
	}
	return(values(x))
}	

.getLogicalValues <- function(x) {
	v <- .getValues(x)
	v[v<0] <- 0
	v[v>0] <- 1
	return(v)
}


setMethod("[", "RasterLayer",
	function(x, i, j, ..., drop = TRUE) {
		if (!missing(drop)) { stop("don't supply drop: it needs to be FALSE anyway") }
		if (!missing(j)) { stop("can only set values with a single index (a vector)") }
		if (missing(i)) { return(x) }
		return(setRaster(x, values=i))
	}
)


setMethod("Math", signature(x='RasterLayer'),
    function(x){ 
		return(setRaster(x, values=callGeneric(.getValues(x))))
	}
)

setMethod("Logic", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if ( compare(c(e1, e2)) ) {
			return(setRaster(e1, values=callGeneric(.getLogicalValues(e1), .getLogicalValues(e2))))
		}
	}
)

setMethod("Arith", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		if (compare(c(e1, e2))) {
			return(setRaster(e1, values=callGeneric(.getValues(e1), .getValues(e2))))
		}	
	}
)

setMethod("Arith", signature(e1='RasterLayer', e2='numeric'),
    function(e1, e2){ 
		return(setRaster(e1, values=callGeneric(.getValues(e1), e2)))
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterLayer'),
    function(e1, e2){ 
		return(setRaster(e2, values=callGeneric(.getValues(e2), e1)))
	}
)


setMethod("max", signature(x='RasterLayer'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(x)
		} else {
			v <- .getValues(x)
			for (i in 1:length(obs)) {
				if (class(obs[[1]]) == 'RasterLayer' & compare(c(x, obs[[1]]))) {
					v <- pmax(v, .getValues(obs[[i]]), na.rm=na.rm)
				} else if (is.atomic(obs[[1]])) {
					v <- pmax(v, rep(obs[[1]], ncells(x)), na.rm=na.rm)
				} else if (length(obs[[1]])==ncells(x)) {
					v <- pmax(v, obs[[1]], na.rm=na.rm)
				} else {
					stop(paste("I do not understand this argument:",obs[1])) 
				}	
			}
			return(setRaster(x, values=v))
		}
	}
)


setMethod("min", signature(x='RasterLayer'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(x)
		} else {
			v <- .getValues(x)
			for (i in 1:length(obs)) {
				if (class(obs[[1]]) == 'RasterLayer' & compare(c(x, obs[[1]]))) {
					v <- pmin(v, .getValues(obs[[i]]), na.rm=na.rm)
				} else if (is.atomic(obs[[1]])) {
					v <- pmin(v, rep(obs[[1]], ncells(x)), na.rm=na.rm)
				} else if (length(obs[[1]])==ncells(x)) {
					v <- pmin(v, obs[[1]], na.rm=na.rm)
				} else {
					stop(paste("I do not understand this argument:",obs[1])) 
				}	
			}
			return(setRaster(x, values=v))
		}
	}
)


.getSum <- function(obs, x, ..., na.rm=FALSE) {
	v <- .getValues(x)
	if (!(is.null(dim(v)))) {
		v <- rowSums(.getValues(x), na.rm=na.rm)
	}
	for (i in 1:length(obs)) {
		if ( (class(obs[[1]]) == 'RasterLayer' | class(obs[[1]]) == 'RasterStack' | class(obs[[1]]) == 'RasterBrick') & compare(c(x, obs[[1]])) ) {			
			v <- rowSums(cbind(v, .getValues(obs[[i]]), na.rm=na.rm))
		} else if (is.atomic(obs[[1]])) {
			v <- rowSums(cbind(v, rep(obs[[1]], ncells(x)), na.rm=na.rm))
		} else if (length(obs[[1]])==ncells(x)) {
			v <- rowSums(cbind(v, obs[[1]], na.rm=na.rm))
		} else {
			stop(paste("I do not understand this argument:",obs[1])) 
		}	
	}
	return(setRaster(x, values=v))
}


setMethod("sum", signature(x='RasterLayer'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(x)
		} else {
			return(.getSum(obs, x, ..., na.rm))
		}
	}
)


setMethod("sum", signature(x='RasterStack'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=rowSums(.getValues(x), na.rm)))
		} else {
			return(.getSum(obs, x, ..., na.rm))		
		}
	}
)


setMethod("sum", signature(x='RasterBrick'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=rowSums(.getValues(x), na.rm)))
		} else {
			return(.getSum(obs, x, ..., na.rm))		
		}
	}
)

#todo "any", "all" 

	
setMethod("range", signature(x='RasterLayer'),
	function(x, ..., na.rm=FALSE){
		return(max(x, ..., na.rm=na.rm) - min(x, ..., na.rm=na.rm))
	}
)	

setMethod("is.na", signature(x='RasterLayer'),
	function(x) {
		return(setRaster(x, values=is.na(.getValues(x))))
	}
)	
	
	
	
	
	
	
setMethod('dim', signature(x='AbstractRaster'), 
	function(x){ return(c(nrow(x), ncol(x)))}
)

setMethod('dim', signature(x='RasterStack'), 
	function(x){ return(c(nrow(x), ncol(x), nlayers(x)))}
)

setMethod('dim', signature(x='RasterBrick'), 
	function(x){ return(c(nrow(x), ncol(x), nlayers(x)))}
)

setMethod('nrow', signature(x='AbstractRaster'), 
	function(x){ return(x@nrows)}
)

setMethod('ncol', signature(x='AbstractRaster'), 
	function(x){ return(x@ncols) }
)




setMethod('summary', signature(object='AbstractRaster'), 
	function(object, ...) {
	# to be replaces by something more typical for summary in R, i.e. a sumary of the raster values
		cat ("Cells: " , ncells(object), '\n')
		if ( class(object) == "RasterLayer" ) {
			if ( dataContent(object) == "all") {
				cat("NAs  : ", sum(is.na(values(object))), "\n")
				summary(values(object))
			} else {
				cat("values not in memory\n")
			}
		} else if (class(object) == "RasterStack" | class(object) == "RasterBrick") {
			if (dataContent(object) == 'all') {
				for (n in 1:nlayers(object)) {
					cat("layer ", n, "\n")
					cat("NAs  : ", sum(is.na(values(object)[,n])), "\n")
					summary(values(object)[,n])
				}
			} else {
				cat("values not in memory\n")
			}
		} 
	}	
)


setMethod("plot", signature(x='RasterLayer', y='missing'), 
	function(x, y, ...)  {
		map(x, ...)
	}
)	


setMethod("plot", signature(x='RasterStack', y='numeric'), 
	function(x, y, ...)  {
		ind <- as.integer(round(y))
		ind <- min(max(ind, 1), nlayers(x))
		map(x, ind, ...)
	}
)		


setMethod("plot", signature(x='RasterBrick', y='numeric'), 
	function(x, y, ...)  {
		ind <- as.integer(round(y))
		ind <- min(max(ind, 1), nlayers(x))
		map(x, ind, ...)
	}
)		



setMethod("plot", signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, ...)  {
		comp <- compare(c(x, y), origin=FALSE, resolution=FALSE, rowcol=TRUE, projection=FALSE, slack=0, stopiffalse=TRUE) 
		if (dataContent(x) != 'all') {
			if (ncells(x) > 15000) {
				maxdim <- 200
			} else {
				maxdim <- 10000
			}
			x <- readSkip(x, maxdim=maxdim)
			if (x != y) {
				warning(paste('plot used a sample of ', round(100*ncells(x)/ncells(y)), "% of the cells", sep=""))
			}
			y <- readSkip(y, maxdim=maxdim)
			x <- values(x)
			y <- values(y)
			plot(x, y, cex=0.1, ...)			
		} else {
			maxcell <- 15000
			if (length(na.omit(values(x))) > maxcell) {
				v <- na.omit(cbind(values(x), values(y)))
				r <- order(runif(length(v[,1])))
				v <- v[r,]
				l <- min(maxcell, length(v))
				v <- v[1:l,]
				warning(paste("plot used a sample of ", l, " cells (with data; ", maxcell, " when counting NA cells)", sep=""))
				x <- v[,1]
				y <- v[,2]
				plot(x, y, cex=0.1, ...)
			}	
		}
	}
)	
	

setMethod('hist', signature(x='RasterLayer'), 
	function(x, ...){
		maxsamp <- 1000000
		if (dataContent(x) != 'all') {
			if (dataSource(x) == 'disk') {
		# TO DO: ake a function that does this by block and combines  all data into a single histogram
				if (ncells(x) <= maxsamp) {
					values <- na.omit(values(readAll(x)))
				} else {
					values <- readRandom(x, maxsamp)
					msg <- paste(round(100 * maxsamp / ncells(x)), "% of the raster cells were used", sep="")
					if (maxsamp > length(values)) {
						msg <- paste(msg, " (of which ", 100 - round(100 * length(values) / maxsamp ), "% were NA)", sep="")
					}
					msg <- paste(msg, ". ",length(values)," values used.", sep="")
					warning(msg)
				}	
			} else { stop('cannot make a histogram; need data on disk or in memory')}
		} else {
			values <- values(x)
		}			
		hist(values, ...)
	}	
)

