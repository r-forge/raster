# The functions is based on a function in the fields package
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html


.rasterImagePlot <- function(x, add=FALSE, legend=TRUE, nlevel = 64, horizontal = FALSE, 
    legend.shrink = 0.5, legend.width = 0.6, legend.mar = ifelse(horizontal, 3.1, 5.1), legend.lab = NULL, graphics.reset = FALSE, 
    bigplot = NULL, smallplot = NULL, legend.only = FALSE, col = heat.colors(nlevel), 
    lab.breaks = NULL, axis.args = NULL, legend.args = NULL, interpolate=FALSE, box=TRUE, breaks=NULL, ...) {

	
	asRaster <- function(x, col, breaks=NULL) {
		if (!is.null(breaks)) {
			x[] <- as.numeric(cut(x, breaks))
		}
		r <- range(x, na.rm=TRUE)
		x <- (x - r[1])/ (r[2] - r[1])
		x <- round(x * (length(col)-1) + 1)
		x[] <- col[x]
		as.raster(x)
	}
	
	e <- as.vector(t(bbox(extent(x))))
	x <- as.matrix(x)
	zrange <- range(x, na.rm=TRUE)
	x <- asRaster(x, col, breaks)
	
	
    old.par <- par(no.readonly = TRUE)
    if (add) {
        big.plot <- old.par$plt
    }
    if (legend.only) {
        graphics.reset <- TRUE
    }
	
    if (is.null(legend.mar)) {
        legend.mar <- ifelse(horizontal, 3.1, 5.1)
    }
    temp <- raster:::.imageplotplt(add = add, legend.shrink = legend.shrink, legend.width = legend.width, legend.mar = legend.mar, 
									horizontal = horizontal, bigplot = bigplot, smallplot = smallplot)
		
    smallplot <- temp$smallplot
    bigplot <- temp$bigplot
	
    if (legend.only) {
		box <- FALSE
	} else {
        if (!add) {
            par(plt = bigplot)
        }
		plot(e[1:2], e[3:4], type = "n", ...)
#		plot(e[1:2], e[3:4], type = "n")
#		yd <- (e[4] - e[3]) * 0.04
#		xd <- (e[2] - e[1]) * 0.04
#		rasterImage(x, e[1]-xd, e[3]-yd, e[2]+xd, e[4]+yd, interpolate=interpolate)

		rasterImage(x, e[1], e[3], e[2], e[4], interpolate=interpolate)
		
        big.par <- par(no.readonly = TRUE)
    } 
	
	if (legend) {
		if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
			par(old.par)
			stop("plot region too small to add legend\n")
		}
		ix <- 1
		minz <- zrange[1]
		maxz <- zrange[2]

		par(new=TRUE, pty = "m", plt=smallplot, err = -1)
		
		if (!is.null(breaks)) {
			binwidth <- (maxz - minz)/nlevel
			midpoints <- seq(minz + binwidth/2, maxz - binwidth/2, by = binwidth)
			iy <- midpoints
			iz <- matrix(iy, nrow = 1, ncol = length(iy))
		}

		if (!is.null(breaks)) {
			if (is.null(lab.breaks) ) {
				lab.breaks=as.character(breaks)
			}
			axis.args <- c(list(side = ifelse(horizontal, 1, 4), mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2), 
								at = breaks, labels = lab.breaks), axis.args)
		} else {
			axis.args <- c(list(side = ifelse(horizontal, 1, 4), mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2)), axis.args)
		}
		
		if (!horizontal) {
			plot(c(0, 1), c(minz, maxz), type = "n", xlab="", ylab="", axes=FALSE)
			xx <- asRaster(length(col):1, col) 

			if (is.null(breaks)) {
				xx <- asRaster(length(col):1, col) 
			} else {
				xx <- asRaster(midpoints, col, breaks=breaks) 
			}

			rasterImage(xx, 0, minz, 1, maxz, interpolate=FALSE)
			do.call("axis", axis.args)
			polygon(c(0, 0, 1, 1, 0), c(minz, maxz, maxz, minz, minz))
#			box()
		} else {
			if (is.null(breaks)) {
				plot(c(minz, maxz), c(0, 1), type = "n", xlab="", ylab="", axes=FALSE)
				xx <- t( asRaster(length(col):1, col) )
				rasterImage(xx, minz, 0, maxz, 1, interpolate=FALSE)
				
			} else {
				if (R.Version()$minor >= 13) {
					image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = col, breaks = breaks, useRaster=TRUE)
				} else {
					image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = col, breaks = breaks)
				}
			}
			do.call("axis", axis.args)
			polygon(c(minz, maxz, maxz, minz, minz), c(0, 0, 1, 1, 0))
#			box()
		}
	
		if (!is.null(legend.lab)) {
			legend.args <- list(text = legend.lab, side = ifelse(horizontal, 1, 4), line = legend.mar - 2)
		}
		if (!is.null(legend.args)) {
			do.call(mtext, legend.args)
		}
	}
	
	mfg.save <- par()$mfg
    if (graphics.reset | add) {
        par(old.par)
        par(mfg = mfg.save, new = FALSE)
    } else {
        par(big.par)
        par(plt = big.par$plt, xpd = FALSE)
        par(mfg = mfg.save, new = FALSE)
    }
	
	if (!add & box ) box()
	invisible()
}


