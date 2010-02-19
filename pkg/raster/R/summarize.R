# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2008
# Version 0.9
# Licence GPL v3

setMethod('summary', signature(object='RasterLayer'), 
	function(object, ...) {
		sumobj <- new("RasterSummary")
		sumobj@ncell <- ncell(object)
		sumobj@dataContent <- dataContent(object) 
		if ( sumobj@dataContent == "all") {
			sumobj@NAs <- sum(is.na(object@data@values))
			sumobj@values <- as.matrix( summary(object@data@values) )
			colnames(sumobj@values)=""
		} 
		return(sumobj)
	}	
)


setMethod('summary', signature(object='RasterStack'), 
	function(object, ...) {
		sumobj <- new("RasterSummary")
		sumobj@ncell <- ncell(object)
		for (n in 1:nlayers(object)) {
			sumobj@dataContent <- c(sumobj@dataContent, dataContent(object@layers[[n]]) )
			if (dataContent(object@layers[[n]]) == 'all') {
				sumobj@NAs <- c(sumobj@NAs, sum(is.na(object@layers[[n]]@data@values)))
				sm = as.matrix( summary(object@layers[[n]]@data@values) )
				sumobj@values <- cbind(sumobj@values, as.matrix(sm))
				rownames(sumobj@values) <- rownames(sm)
			} else {
				sumobj@NAs <- c(sumobj@NAs, NA)
				sumobj@values <- cbind(sumobj@values , NA)
			}
		}
		colnames(sumobj@values) <- 1:nlayers(object)
		return(sumobj)
	}
)	




setMethod('summary', signature(object='RasterBrick'), 
	function(object, ...) {
		sumobj <- new("RasterSummary")
		sumobj@ncell <- ncell(object)
		sumobj@dataContent <- dataContent(object)

		if (dataContent(object) == 'all') {
			for (n in 1:nlayers(object)) {
				sumobj@NAs <- c(sumobj@NAs, sum(is.na(object@data@values[, n])))
				sm = as.matrix( summary( object@data@values[,n] ) )
				sumobj@values <- cbind(sumobj@values, as.matrix(sm))
				rownames(sumobj@values) <- rownames(sm)
			} 
			colnames(sumobj@values) <- 1:nlayers(object)
		}
		return(sumobj)
	}
)	


setClass('RasterSummary',
	representation (
		ncell = 'numeric',
		dataContent = 'vector',
		NAs = 'vector',
		values = 'matrix'
	),
	prototype (	
		dataContent = vector(mode='character'),
		NAs = vector(mode='numeric'),
		values = matrix(nrow=6, ncol=0)
	),
)
	

setMethod('show', signature(object='RasterSummary'), 	
	function(object) {
		cat ("Cells: " , object@ncell, "\n")
		if ( any(object@dataContent=="all")) {
			cat("NAs  : ", object@NAs, "\n")
			cat("\n")
			print(object@values) 
		} else {
			cat("values not in memory\n")
		}
	}	
)
	

