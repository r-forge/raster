# Authors: Robert J. Hijmans, r.hijmans@gmail.com and Jacob van Etten
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3


setMethod ('show' , 'BoundingBox', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('xmin        :' , xmin(object), '\n')
		cat('xmax        :' , xmax(object), '\n')
		cat('ymin        :' , ymin(object), '\n')
		cat('ymax        :' , ymax(object), '\n')
	}
)	
	
	
setMethod ('show' , 'RasterLayer', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('filename    :' , filename(object), '\n')
		if (nbands(object) > 1) {
			cat('band        :' , band(object), '\n')
		}	
		cat('nrow        :' , nrow(object), '\n')
		cat('ncol        :' , ncol(object), '\n')
		cat('ncells      :' , ncells(object), '\n')
		cat('data type   :' , object@file@datanotation, '\n')
		cat('data content:' ,  dataContent(object), '\n')
		if (object@data@haveminmax) {
			cat('min value   :' , minValue(object), '\n')
			cat('max value   :' , maxValue(object), '\n')
		} else { #if (object@data@source == 'disk')  {
			cat('min value   : NA \n')
			cat('max value   : NA \n')
		}
		cat('projection  :' , projection(object, TRUE), '\n')
		cat('xmin        :' , xmin(object), '\n')
		cat('xmax        :' , xmax(object), '\n')
		cat('ymin        :' , ymin(object), '\n')
		cat('ymax        :' , ymax(object), '\n')
		cat('xres        :' , xres(object), '\n')
		cat('yres        :' , yres(object), '\n')
		cat ('\n')
	}
)


setMethod ('show' , 'RasterBrick',
	function ( object ){
		cat ('class     :' , class ( object ) , '\n')
		cat ('filename  :' , filename(object), '\n')
		cat ('nlayers   :' , nlayers(object), '\n')
		cat ('nrow      :' , nrow(object), '\n')
		cat ('ncol      :' , ncol(object), '\n')
		cat ('ncells    :' , ncells(object), '\n')
		cat ('projection:' , projection(object, TRUE), '\n')
		cat ('xmin      :' , xmin(object), '\n')
		cat ('xmax      :' , xmax(object), '\n')
		cat ('ymin      :' , ymin(object), '\n')
		cat ('ymax      :' , ymax(object), '\n')
		cat ('xres      :' , xres(object) , '\n')
		cat ('yres      :' , yres(object) , '\n')
		cat ('\n')
	}
)


setMethod ('show' , 'RasterStack',
	function ( object ){
		cat ('class     :' , class ( object ) , '\n')
		cat ('filename  :' , filename(object), '\n')
		cat ('nlayers   :' , nlayers(object), '\n')
		cat ('nrow      :' , nrow(object), '\n')
		cat ('ncol      :' , ncol(object), '\n')
		cat ('ncells    :' , ncells(object), '\n')
		cat ('projection:' , projection(object, TRUE), '\n')
		cat ('xmin      :' , xmin(object), '\n')
		cat ('xmax      :' , xmax(object), '\n')
		cat ('ymin      :' , ymin(object), '\n')
		cat ('ymax      :' , ymax(object), '\n')
		cat ('xres      :' , xres(object) , '\n')
		cat ('yres      :' , yres(object) , '\n')
		cat ('\n')
	}
)



setMethod('==', signature(e1='Raster', e2='Raster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), bb=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=FALSE) 
		return(cond)
	}
)	

setMethod('!=', signature(e1='Raster', e2='Raster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), bb=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=FALSE) 
		return(!cond)
	}
)	


.getRasterValues <- function(x) {
# need to take care of 'spase'
	if (dataContent(x) != 'all') {
		if (class(x) == "RasterLayer") {
			if (dataSource(x) == 'ram') {
				stop('no data on disk or in memory')
			} else x <- readAll(x)	
		} else {
			x <- readAll(x)
		}
	}
	return(values(x))
}	

.getLogicalValues <- function(x) {
	v <- .getRasterValues(x)
	v[v<0] <- 0
	v[v>0] <- 1
	return(v)
}

.getAllTypeOfValues <- function(x, y, i) {
	if ( (class(y) == 'RasterLayer' | class(y) == 'RasterStack' | class(y) == 'RasterBrick') & compare(c(x, y)) ) {			
		return(.getRasterValues(y))
	} else if (is.atomic(y)) {
		return(rep(y, ncells(x)))
	} else if (length(y)==ncells(x)) {
		return(y)
	} else {
		stop(paste("I do not understand argument",i + 1)) 
	}	
}

setMethod("[", "RasterLayer",
	function(x, i, j, ..., drop = TRUE) {
		if (!missing(drop)) { stop("drop is ignored. It is always set to FALSE") }
		if (!missing(j)) { stop("can only set values with a single index (a vector)") }
		if (missing(i)) { return(x) }
		v <- values(i)
		v[x] <- i
		return(setRaster(x, v))
	}
)


setMethod("Math", signature(x='RasterLayer'),
    function(x){ 
		return(setRaster(x, values=callGeneric(.getRasterValues(x))))
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
			return(setRaster(e1, values=callGeneric(.getRasterValues(e1), .getRasterValues(e2))))
		}	
	}
)

setMethod("Arith", signature(e1='RasterLayer', e2='numeric'),
    function(e1, e2){ 
		return(setRaster(e1, values=callGeneric(.getRasterValues(e1), e2)))
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterLayer'),
    function(e1, e2){ 
		return(setRaster(e2, values=callGeneric(.getRasterValues(e2), e1)))
	}
)


setMethod("max", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=apply(as.matrix(.getRasterValues(x)), 1, max, na.rm=na.rm)))
		} else {
			v <- .getRasterValues(x)
			for (i in 1:length(obs)) {
				v <- apply(cbind(v, .getAllTypeOfValues(x, obs[[i]], i)), 1, max, na.rm=na.rm)
			}
			return(setRaster(x, values=v))
		}
	}
)


setMethod("min", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=apply(as.matrix(.getRasterValues(x)), 1, min, na.rm=na.rm)))
		} else {
			v <- .getRasterValues(x)
			for (i in 1:length(obs)) {
				v <- apply(cbind(v, .getAllTypeOfValues(x, obs[[i]], i)), 1, min, na.rm=na.rm)
			}
			return(setRaster(x, values=v))
		}
	}
)


setMethod("sum", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		obs <- list(...)
		if (length(obs) == 0) {
			return(setRaster(x, values=rowSums(as.matrix(.getRasterValues(x)), na.rm)))
		} else {
			v <- .getRasterValues(x)
			if (!(is.null(dim(v)))) {
				v <- rowSums(as.matrix(.getRasterValues(x)), na.rm=na.rm)
			} 
			for (i in 1:length(obs)) {
				vv <- .getAllTypeOfValues(x, obs[[i]], i)
				v <- rowSums(cbind(v, vv), na.rm=na.rm)
			}
		return(setRaster(x, values=v))
		}
	}
)


#todo "any", "all" 

	
setMethod("range", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		return(max(x, ..., na.rm=na.rm) - min(x, ..., na.rm=na.rm))
	}
)	

setMethod("is.na", signature(x='RasterLayer'),
	function(x) {
		return(setRaster(x, values=is.na(.getRasterValues(x))))
	}
)	
	
	
setMethod('dim', signature(x='Raster'), 
	function(x){ return(c(nrow(x), ncol(x), nlayers(x)))}
)

setMethod('nrow', signature(x='Raster'), 
	function(x){ return(x@nrows)}
)

setMethod('ncol', signature(x='Raster'), 
	function(x){ return(x@ncols) }
)



setMethod('summary', signature(object='Raster'), 
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


setMethod("plot", signature(x='Raster', y='missing'), 
	function(x, y, ...)  {
		map(x, ...)
	}
)	

setMethod("plot", signature(x='Raster', y='numeric'), 
	function(x, y, ...)  {
		map(x, y, ...)
	}
)		


# helper function to set ... variables if they are not specified by the user. There probably exists a formal, and direct, mechanism to do this in R, but I have not discovered this yet...
#.getmaxdim <- function(maxdim=1000, ...) {
#	return(maxdim)
#}

getcex <- function(cex = 0.1, ...) {
	return(cex)
}

setMethod("plot", signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, maxdim=1000, ...)  {
		comp <- compare(c(x, y), bb=TRUE, rowcol=TRUE, prj=FALSE, tolerance=0.0001, stopiffalse=TRUE) 
		nc <- ncells(x)
		x <- readSkip(x, maxdim=maxdim)
		y <- readSkip(y, maxdim=maxdim)
		if (length(x) < nc) {
			warning(paste('plot used a sample of ', round(100*length(x)/nc), "% of the cells", sep=""))
		}
		cex <- getcex(...)
		plot(x, y, ...)			
	}
)
	

setMethod('hist', signature(x='RasterLayer'), 
	function(x, maxsamp=10000, ...){
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



