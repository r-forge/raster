# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2008
# Version 0.9
# Licence GPL v3



.setProgressBar <- function(nrows, type) {
	if (type=='text') {
		pb <- txtProgressBar(min = 0, max = nrows, style = 3)
	} else if (type == 'tcltk') {
		require(tcltk)
		pb <- tkProgressBar(title = "progress bar", min = 0, max = nrows, width = 300)
	} else if (type == 'windows') {
		if (substr( R.Version()$platform, 1, 7) == "i386-pc" ) {
			pb <- winProgressBar(title = "progress bar", min = 0, max = nrows, width = 300)
		} else {
			warning('windows progress bar is only availble on the Windows Operating System')
			pb <- txtProgressBar(min = 0, max = nrows, style = 3)
		}
	} else {
		pb <- 'no progress bar'
	}
	return(pb)
}

.doProgressBar <- function(pb, r, starttime) {
	pbclass <- class(pb)
	if (pbclass=="txtProgressBar") {
		setTxtProgressBar(pb, r)
	} else if (pbclass=="tkProgressBar") {
		setTkProgressBar(pb, r, label=paste( 'row', r))	
	} else if (pbclass=="winProgressBar") {
		setWinProgressBar(pb, r, title=paste( 'row', r))	
	} 
}

.closeProgressBar <- function(pb, type, starttime) {
	pbclass <- class(pb)
	if (pbclass=="txtProgressBar") {
		cat("\n")
		close(pb)
	} else if (pbclass=="tkProgressBar") {
		close(pb)
	} else if (pbclass=="winProgressBar") {
		close(pb)
	} 
}