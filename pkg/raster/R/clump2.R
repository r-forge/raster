# Authors: Robert J. Hijmans and Jacob van Etten, 
# Date : May 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("clump2")) {
	setGeneric("clump2", function(x, ...)
		standardGeneric("clump2"))
}	


.clump <- function(x, directions) {
	x1 <- raster(x)
	val <- which(getValues(x) != 0)
	if (length(val) == 0) { 
		return( setValues(x1, NA) )
	}
	adjv <- as.vector( t ( adjacency(x1, val, val, directions=directions) ) )
	cl <- clusters(graph(adjv, directed=FALSE))$membership[val+1]
	ucl <- sort(unique(cl))
	ucl <- data.frame(id=ucl, value=1:length(ucl))
	test <- subset(ucl, ucl$id != ucl$value)
	if (nrow(test) > 0 ) {
		cl <- merge(cl, ucl, by=1, all.x=TRUE)
	}
	x1[val] <- cl
	return(x1)
}



setMethod('clump2', signature(x='RasterLayer'), 
function(x, filename='', directions=8, ...) {

	if( !require(igraph)) {
		stop('you need to install the igraph package to be able to use this function')
	}

	if (! directions %in% c(4,8)) {stop('directions should be 4 or 8')}

	if (filename != ""  & file.exists(filename)) {
		if (.overwrite(...)==FALSE) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}

	outRaster <- raster(x)
	global <- .isGlobalLatLon(outRaster)
	
	if (canProcessInMemory(outRaster, 3)) {
		x <- .clump(x, directions)
		return(x)
	} 
	# else 
	
	if (filename == '') filename <- rasterTmpFile()
	writeStart(outRaster, filename, ...)

	tr <- blockSize(outRaster)
	pb <- pbCreate(tr$n, type=.progress(...))
	
	ext <- c(xmin(outRaster), xmax(outRaster), ymax(outRaster), NA)
	maxval <- 0
	
	for (i in 1:tr$n) {
		ext[4] <- ext[3]
		ext[3] <- yFromRow(tr$row[i] + 1) # one more row
		xc <- crop(x, ext)
		xc <- .clump(xc, directions) + maxval
		if (i > 1) {
			firstrow <- getValues(xc, 1)
			rr <- na.omit(unique(cbind(firstrow, lastrow)))
			if (nrow(rr) > 0 ) {
				xc <- subs(xc, rr, subsWithNA=FALSE)
			}
		}
		lastrow <- getValues(xc, nrows(cx))
		maxval <- max(getValues(xc))
		outRaster <- writeValues(outRaster, getValues(xc, 1, tr$nrows[i]))
	}
	
	outRaster <- writeStop(outRaster)
	return(outRaster)
}
)