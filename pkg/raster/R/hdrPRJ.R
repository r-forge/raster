# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2009
# Version 0.9
# Licence GPL v3


.writeHdrPRJ <- function(x, ESRI=TRUE) {
	p4s <- try(	showWKT(projection(x), file = NULL, morphToESRI = ESRI) )
	if (class(p4s) != 'try-error') {
		prjfile <- filename(x)
		ext(prjfile) <- '.prj'
		cat(p4s, file=filename)
	} else {
		return(FALSE)
	}
	return(invisible(TRUE))
}

	
