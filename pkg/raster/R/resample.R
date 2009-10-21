# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  Jaunary 2009
# Version 0.9
# Licence GPL v3


resample <- function(from, to, method="ngb", filename="", ...)  {
	datatype <- .datatype(...)
	filetype <- .filetype(...)
	overwrite <- .overwrite(...)

	
	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') 	}
		
	bb <- intersectExtent(from, to)
	validObject(bb)
	if (is.null(filename)){filename <- ""}
	to <- raster(to, filename)
	dataType(to) <- datatype
	
	if (!canProcessInMemory(to, 1) && filename(to) == '') {
		filename <- rasterTmpFile()
		filename(to) <- filename
		if (getOption('verbose')) { cat('writing raster to:', filename(to))	}
	}
	inMemory <- filename(to) == ""

	v <- vector(length=0)
	rowCells <- 1:ncol(to)

	starttime <- proc.time()		
	pb <- pbSet(nrow(to), type=.progress(...))

	for (r in 1:nrow(to)) {
		cells <- rowCells + (r-1) * ncol(to)
		xy <- xyFromCell(to, cells)
		if (method=='ngb') {
			vals <- xyValues(from, xy)
		} else {
			vals <- xyValues(from, xy, method='bilinear')
		}
		if (inMemory) {
			v <- c(v, vals)
		} else {
			to <- setValues(to, vals, r)
			to <- writeRaster(to, overwrite=overwrite, filetype=filetype)
		}

		pbDo(pb, r)
	}
	pbClose(pb, starttime)

	if (inMemory) {
		to <- setValues(to, v) 
	}
	return(to)
}

