# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.8
# Licence GPL v3

.showTrack <- function(r, nrows, track, starttime) {
	elapsed <- (proc.time() - starttime)[3]
	tpr <- round((elapsed /r), digits=2)
	print(paste('row', r, '--', tpr, 'seconds/row --', nrows+1-r, " rows to go"))	
}
