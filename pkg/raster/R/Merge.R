# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,7
# Licence GPL v3


Merge <- function(rasters, slack=0.01, filename="", overwrite=FALSE) {
	compare(rasters, rowcol=FALSE, slack=slack)
	
#	for (i in 1:length(rasters)) {
#		if (!(data.source(rasters[[i]]) == 'disk' | dataContent(rasters[[i]]) == 'all' | dataContent(rasters[[i]]) == 'sparse')) { 
#			stop('rasters should be stored on disk or values should be in memory') 
#		}
#	}

	bb <- bbox(rasters[[1]])
	for (i in 2:length(rasters)) {
		bb2 <- bbox(rasters[[i]])
		bb[,1] <- pmin(bb[,1], bb2[,1])
		bb[,2] <- pmax(bb[,2], bb2[,2])
	}
	outraster <- setRaster(rasters[[1]], filename)
	outraster <- setBbox(outraster, bb[1,1], bb[1,2], bb[2,1], bb[2,2], keepres=TRUE)

	rowcol <- matrix(0, ncol=3, nrow=length(rasters))
	for (i in 1:length(rasters)) {
		xy1 <- xyFromCell(rasters[[i]], 1) # first row/col on old raster[[i]]
		xy2 <- xyFromCell(rasters[[i]], ncells(rasters[[i]]) ) #last row/col on old raster[[i]]
		rowcol[i,1] <- rowFromY(outraster, xy1[2]) #start row on new raster
		rowcol[i,2] <- rowFromY(outraster, xy2[2]) #end row
		rowcol[i,3] <- colFromX(outraster, xy1[1]) #start col
	}
	v <- vector(length=0)
	for (r in 1:nrow(outraster)) {
		rd <- as.vector(matrix(NA, nrow=1, ncol=ncol(outraster))) 
		for (i in length(rasters):1) {  #reverse order so that the first raster covers the second etc.
			if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
				if (rasters[[i]]@data@source == 'disk') {
					rasters[[i]] <- readRow(rasters[[i]], r + 1 - rowcol[i,1]) 
					d <- values(rasters[[i]])
				} else if (dataContent(rasters[[i]]) == 'all') {
					d <- valuesRow(rasters[[i]], r + 1 - rowcol[i,1]) 
				} else {
					d <- vector(length=ncol(rasters[[i]]))
					d[] <- NA
				}	
				id2 <- seq(1:ncol(rasters[[i]])) + rowcol[i,3] - 1
				d <- cbind(id2, d)
				d <- na.omit(d)
				rd[d[,1]] <- d[,2]
			}		
		}
		if (filename(outraster) != '') {
			outraster <- setValuesRow(outraster, rd, r)
			outraster <- write.row(outraster, overwrite)
		} else {
			v <- c(v, rd)
		}
	}
	if (filename(outraster) == '') { outraster <- setValues(outraster, v) }
	return(outraster)
}
