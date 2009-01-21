# R code for changing rasters (spatial data)
# Authors: Robert J. Hijmans and Jacob van Etten
# International Rice Research Institute
#contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.7
# Licence GPL v3


.outerBox <- function(objects) {
	if (length(objects) == 1) {
		return(getBbox(objects))
	}
	bb <- getBbox(objects[[1]])
	for (i in 2:length(objects)) {
		bb2 <- getBbox(objects[[i]])
		bb@xmin <- min(xmin(bb), xmin(bb2))
		bb@xmax <- max(xmax(bb), xmax(bb2))
		bb@ymin <- min(ymin(bb), ymin(bb2))
		bb@ymax <- max(ymax(bb), ymax(bb2))
	}
	return(bb)
}

.innerBox <- function(objects) {
	if (length(objects) == 1) {
		return(getBbox(objects))
	}
	bb <- getBbox(objects[[1]])
	for (i in 2:length(objects)) {
		bb2 <- getBbox(objects[[i]])
		bb@xmin <- max(xmin(bb), xmin(bb2))
		bb@xmax <- min(xmax(bb), xmax(bb2))
		bb@ymin <- max(ymin(bb), ymin(bb2))
		bb@ymax <- min(ymax(bb), ymax(bb2))
	}
	validObject(bb)
	return(bb)
}



Merge <- function(rasters, tolerance=0.05, filename="", overwrite=FALSE) {
	compare(rasters, bb=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)
	bb <- .outerBox(rasters)
	outraster <- setRaster(rasters[[1]], filename)
#	bndbox <- newBbox(bb[1,1], bb[1,2], bb[2,1], bb[2,2])
	outraster <- setBbox(outraster, bb, keepres=TRUE, snap=FALSE)

	rowcol <- matrix(0, ncol=3, nrow=length(rasters))
	for (i in 1:length(rasters)) {
		xy1 <- xyFromCell(rasters[[i]], 1) # first row/col on old raster[[i]]
		xy2 <- xyFromCell(rasters[[i]], ncell(rasters[[i]]) ) #last row/col on old raster[[i]]
		rowcol[i,1] <- rowFromY(outraster, xy1[2]) #start row on new raster
		rowcol[i,2] <- rowFromY(outraster, xy2[2]) #end row
		rowcol[i,3] <- colFromX(outraster, xy1[1]) #start col
	}
	
	v <- vector(length=0)
	for (r in 1:nrow(outraster)) {
		rd <- as.vector(matrix(NA, nrow=1, ncol=ncol(outraster))) 
		for (i in length(rasters):1) {  #reverse order so that the first raster covers the second etc.
			if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
				if (dataSource(rasters[[i]]) == 'disk') {
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
			outraster <- setValues(outraster, rd, r)
			outraster <- writeRaster(outraster, overwrite)
		} else {
			v <- c(v, rd)
		}
	}
	if (filename(outraster) == '') { outraster <- setValues(outraster, v) }
	return(outraster)
}
