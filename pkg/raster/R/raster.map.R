# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3



rasterstack.map <- function(rstack, index=1, col = rev(terrain.colors(25)), subsample=TRUE, maxdim=500, ...) {
	index <- round(index)
	i <- min(max(1, index), rstack@data@nlayers)
	if (i != index) {stop("index should be >= 1 and <= rstack@data@nlayers")}
	raster <- rstack@rasters[[i]]
	if (rstack@data@content == 'all') {
		raster <- set.values(raster, rstack@data@values[i,])
	}
	raster.map(raster, col=col, subsample=subsample, maxdim=maxdim, ...)
}


raster.map <- function(raster, col = rev(terrain.colors(25)), subsample=TRUE, maxdim=500, addbox=TRUE, axes = TRUE, xlab="", ylab="", legend.shrink = 0.5, legend.width = 0.8, ...) {
#TODO if xlim and/or ylim are used, only read (and sample) for those areas.
#	require(fields)
	if (class(raster) == 'character') { raster <- raster.from.file(raster) }
	if (class(raster) == 'RasterStack') { raster <- raster@rasters[[1]] }
	if (class(raster) != 'RasterLayer') { stop("class of 'raster' should be RasterLayer") }

	maxdim <- max(1, maxdim)
	if ( data.content(raster) == 'all') {
		skip <- round(max(ncol(raster), nrow(raster)) / maxdim)
		if (skip < maxdim) { subsample <- FALSE }
		if (subsample)  {
			cols <- (0:round(ncol(raster)/skip)) * skip + 1
			cols <- cols[ cols <= ncol(raster) ]
			rows <- (0:round(nrow(raster)/skip)) * skip + 1
			rows <- rows[ rows <= nrow(raster) ]
			
			m <- values(raster, format='matrix')[rows, cols]

			sampraster <- set.raster(raster)
			sampraster <- set.rowcol(sampraster, dim(m)[1], dim(m)[2])
			xmx <- xmax(raster) - (ncol(raster) - cols[length(cols)]) * xres(raster)
			ymn <- ymin(raster) + (nrow(raster) - rows[length(rows)]) * yres(raster)
			raster <- set.bbox(sampraster, xmx=xmx, ymn=ymn)
 		} else { 
			m <- values(raster, format='matrix')
			subsample=FALSE
		}
	} else {
		if (subsample) {
			raster <- .read.skip(raster, maxdim=maxdim)
		} else {
			raster <- .raster.read.all(raster)
		}
		m <- values(raster, format='matrix')
	} 
	x <- (0:ncol(raster)) * xres(raster) + xmin(raster) 
	y <- (0:nrow(raster)) * yres(raster) + ymin(raster) 		

	z <- t(m[nrow(m):1,])
	
	z[is.infinite(z)] <- NA
	
	image.plot(x, y, z, col=col, axes = TRUE, xlab=xlab, ylab=ylab, legend.shrink = legend.shrink, legend.width=legend.width, ...)
	if (addbox) {box()}
#	image(x, y, z, col=col, axes = FALSE, xlab="", ylab="")
#	contour(x, y, z, add = TRUE, col = "peru")
#	xincr <- (raster@xmax - raster@xmin) / 12
#	yincr <- (raster@ymax - raster@ymin) / 10
#	axis(1, at = seq(raster@xmin, raster@xmax, by = xincr))
#	axis(2, at = seq(raster@ymin, raster@ymax, by = yincr))
#	title(main = raster@file@shortname, font.main = 4)
}	

