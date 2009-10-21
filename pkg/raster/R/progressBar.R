# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2008
# Version 0.9
# Licence GPL v3



pbSet <- function(nsteps, type, style=3) {
	if (type=='text') {
		pb <- txtProgressBar(min = 0, max = nsteps, style=style)
	} else if (type == 'tcltk') {
		require(tcltk)
		pb <- tkProgressBar(title = "progress bar", min = 0, max = nsteps, width = 300)
	} else if (type == 'windows') {
		if (substr( R.Version()$platform, 1, 7) == "i386-pc" ) {
			pb <- winProgressBar(title = "progress bar", min = 0, max = nsteps, width = 300)
		} else {
			pb <- tkProgressBar(title = "progress bar", min = 0, max = nsteps, width = 300)
		}
	} else {
		pb <- 'none'
	}
	return(pb)
}

pbDo <- function(pb, step, label='row', value=step) {
	pbclass <- class(pb)
	if (pbclass=="txtProgressBar") {
		setTxtProgressBar(pb, step)
	} else if (pbclass=="tkProgressBar") {
		setTkProgressBar(pb, step, label=paste(label, value))	
	} else if (pbclass=="winProgressBar") {
		setWinProgressBar(pb, step, title=paste(label, value))	
	} 
}

pbClose <- function(pb, starttime) {
	pbclass <- class(pb)
	if (pbclass=="txtProgressBar") {
		cat("\n")
		close(pb)
	} else if (pbclass=="tkProgressBar") {
		close(pb)
	} else if (pbclass=="winProgressBar") {
		close(pb)
	} 
	if (! missing(starttime)) {
		elapsed <- (proc.time() - starttime)[3]
		cat('finished in ', elapsed, ' seconds')
	}
}