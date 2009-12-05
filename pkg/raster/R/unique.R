# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("unique")) {
	setGeneric("unique", function(x, incomparables=FALSE, ...)
		standardGeneric("unique"))
}	


setMethod('unique', signature(x='RasterLayer', incomparables='missing'), 
function(x) {
	if (canProcessInMemory(x, 2)) {
		if (dataContent(x) != 'all') {
			x <- readAll(x)
		}
	} 
	if (dataContent(x) == 'all') {
		u <- callGeneric(values(x))
		return(sort(u))
	} else if (dataSource(x) == 'disk' ) {
		u1 <- vector()
		u2 <- vector()
		for (r in 1:nrow(x)) {
			u1 <- unique(c(values(readRow(x, r)), u1))
			if (length(u1) > 10000 ) {
				u2 <- unique(c(u1, u2))
				u1 <- vector()
			}
		}
		return(sort(unique(c(u1, u2))))	
	} else {
		stop('This RasterLayer has no values')
	}
}
)

