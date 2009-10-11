# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2008
# Version 0.9
# Licence GPL v3


setMethod('summary', signature(object='RasterStack'), 
	function(object, ...) {
		cat ("Cells: " , ncell(object), "\n")
		for (n in 1:nlayers(object)) {
			if (dataContent(object@layers[[n]]) == 'all') {
				cat("layer ", n, "\n")
				cat("   NAs : ", sum(is.na(values(object@layers[[n]]))), "\n")
				summary(values(object@layers[[n]]))
			} else {
				cat("layer ", n, "\n")
				cat("   values not in memory\n")
			}
		}
	}
)	


setClass('RasterLayerSummary',
	representation (
		ncell = 'numeric',
		dataContent = 'character',
		NAs = 'numeric',
		values = 'matrix'
	)
)
	

setMethod('show', signature(object='RasterLayerSummary'), 	
	function(object) {
		cat ("Cells: " , object@ncell, "\n")
		if ( object@dataContent == "all") {
			cat("NAs  : ", object@NAs, "\n")
			cat("\nValues")
			tab <- as.table(object@values) 
			colnames(tab) <- ""
			print(tab)
		} else {
			cat("values not in memory\n")
		}
	}	
)
	
setMethod('summary', signature(object='RasterLayer'), 
	function(object, ...) {
		sumobj <- new("RasterLayerSummary")
		sumobj@ncell <- ncell(object)
		sumobj@dataContent <- dataContent(object) 
		sumobj@NAs <- NA
		if ( sumobj@dataContent == "all") {
			sumobj@NAs <- sum(is.na(values(object)))
			sumobj@values <- as.matrix( summary(values(object)) )
		} 
		return(sumobj)
	}	
)

