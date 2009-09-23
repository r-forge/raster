# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2008
# Version 0.9
# Licence GPL v3



.setProgressBar <- function(nrows, type='text') {
	if (type=='text') {
		pb <- txtProgressBar(min = 0, max = nrows, style = 3)
	} else if (type == 'tcltk') {
		require(tcltk)
		pb <- tkProgressBar(title = "progress bar", min = 0, max = nrows, width = 300)
	} else if (type == 'windows') {
		pb <- winProgressBar(title = "progress bar", min = 0, max = nrows, width = 300)
	}
	return(pb)
}

.doProgressBar <- function(pb, r, type='text') {
	if (type=='text') {
		setTxtProgressBar(pb, r)
	} else if (type == 'tcltk') {
		setTkProgressBar(pb, r, label=paste( 'row', r))	
	} else if (type == 'windows') {
		setWinProgressBar(pb, r, title=paste( 'row', r))	
	}
}

