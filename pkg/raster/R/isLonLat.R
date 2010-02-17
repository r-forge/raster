# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("isLonLat")) {
	setGeneric("isLonLat", function(x)
		standardGeneric("isLonLat"))
}	

setMethod('isLonLat', signature(x='Raster'), 
# copied from the SP package (slightly adapted)
#author:
# ...
	function(x){
		p4str <- projection(x)
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

setMethod('isLonLat', signature(x='character'), 
# copied from the SP package (slightly adapted)
#author:
# ...
	function(x){
		res <- grep("longlat", x, fixed = TRUE)
		if (length(res) == 0) {
			return(FALSE)
		} else {
			return(TRUE)
		}
    }
)


setMethod('isLonLat', signature(x='CRS'), 
# copied from the SP package (slightly adapted)
#author:
# ...
	function(x){
		if (is.na(x@projargs)) { 
			p4str <- "NA"
		} else {
			p4str <- trim(x@projargs)
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

