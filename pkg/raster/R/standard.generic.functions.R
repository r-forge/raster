# Authors: Robert J. Hijmans, r.hijmans@gmail.com and Jacob van Etten
# International Rice Research Institute
# Date :  June 2008
# Version 0,8
# Licence GPL v3


setMethod('dim', signature(x='Raster'), 
	function(x){ return(c(nrow(x), ncol(x), nlayers(x)))}
)

setMethod('nrow', signature(x='Raster'), 
	function(x){ return(x@nrows)}
)

setMethod('ncol', signature(x='Raster'), 
	function(x){ return(x@ncols) }
)



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

.getcex <- function(cex = 0.1, ...) {
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
		cex <- .getcex(...)
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



