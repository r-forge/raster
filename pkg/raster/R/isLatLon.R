# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("isLatLon")) {
	setGeneric("isLatLon", function(object)
		standardGeneric("isLatLon"))
}	

setMethod('isLatLon', signature(object='Raster'), 
# copied from the SP package (slightly adapted)
#author:
# ...
	function(object){
		p4str <- projection(object)
		if (is.na(p4str) || nchar(p4str) == 0) {
			return(as.logical(NA))
		} 
		res <- grep("longlat", p4str, fixed = TRUE)
		if (length(res) == 0) {
			return(FALSE)
		} else {
			return(TRUE)
		}
    }
)

setMethod('isLatLon', signature(object='character'), 
# copied from the SP package (slightly adapted)
#author:
# ...
	function(object){
		res <- grep("longlat", object, fixed = TRUE)
		if (length(res) == 0) {
			return(FALSE)
		} else {
			return(TRUE)
		}
    }
)


setMethod('isLatLon', signature(object='CRS'), 
# copied from the SP package (slightly adapted)
#author:
# ...
	function(object){
		if (is.na(object@projargs)) { 
			p4str <- "NA"
		} else {
			p4str <- trim(object@projargs)
		}	
		if (is.na(p4str) || nchar(p4str) == 0) {
			return(as.logical(NA))
		} 
		res <- grep("longlat", p4str, fixed = TRUE)
		if (length(res) == 0) {
			return(FALSE)
		} else {
			return(TRUE)
		}
    }
)

