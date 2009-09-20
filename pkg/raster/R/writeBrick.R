# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

writeBrick <- function(brick, filename, bandorder='BIL', ...) {
	return(writeStack(brick, filename, bandorder='BIL', ...))
}


