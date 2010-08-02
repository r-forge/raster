# Authors: Robert J. Hijmans and Jacob van Etten, 
# Date : May 2010
# Version 0.9
# Licence GPL v3


if (!isGeneric("clump")) {
	setGeneric("clump", function(x, ...)
		standardGeneric("clump"))
}	


.smallClump <- function(x, directions=8) {
	x1 <- raster(x)
	val <- which(getValues(x) != 0)
	if (length(val) == 0) { 
		return( setValues(x1, NA) )
	}
	adjv <- as.vector( t ( adjacency(x1, val, val, directions=directions) ) )
	adjv <- c(adjv, rep(val, each=2))  # fixed problem of missing the last cells, perhaps clumsy
	
	cl <- clusters(graph(adjv, directed=FALSE))$membership[val+1]
	ucl <- sort(unique(cl))
	ucl <- data.frame(id=ucl, value=1:length(ucl))

	x1[val] <- as.numeric(as.factor(cl)) # force 1 to n
	return(x1)
}


setMethod('clump', signature(x='RasterLayer'), 

function(x, filename='', directions=8, gaps=TRUE, ...) {

	if( !require(igraph)) {
		stop('you need to install the igraph package to be able to use this function')
	}

	if (! directions %in% c(4,8)) { stop('directions should be 4 or 8') }

	filename <- trim(filename)
	if (filename != ""  & file.exists(filename)) {
		if (.overwrite(...)==FALSE) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}

	outRaster <- raster(x)
	global <- .isGlobalLatLon(outRaster)
	
	if (canProcessInMemory(outRaster, 3)) {
		x <- .smallClump(x, directions)
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)
	} 
	# else 

	outRaster <- writeStart(outRaster, filename=rasterTmpFile(), datatype='INT2U')

	tr <- blockSize(outRaster)
	pb <- pbCreate(tr$n, type=.progress(...))
	
	ext <- c(xmin(outRaster), xmax(outRaster), ymax(outRaster), NA)
	maxval <- 0
	
	rcl <- matrix(nrow=0, ncol=2)
	for (i in 1:tr$n) {
	
		ext[4] <- yFromRow(outRaster, tr$row[i]) + 0.5 * yres(outRaster)
		
		endrow <- tr$row[i] + tr$nrows[i] - 1 
		ext[3] <- yFromRow(outRaster, endrow) - 1.5 * yres(outRaster) # one additional row
		xc <- crop(x, extent(ext))
		
		xc <- .smallClump(xc, directions) + maxval
		if (i > 1) {
			firstrow <- getValues(xc, 1)
			rc <- na.omit(unique(cbind(lastrow, firstrow)))
			if (nrow(rc) > 1) {
				# here we need to catch the case where one number becomes two other numbers
				tt <- table(rc[,1])
				j <- as.integer(names(tt[ which(tt > 1) ]))
				if ( length(j) > 0 ) {
					k <- rc[,1] %in% j
					rc2 <- rc[!k,]
					rcl <- rbind(rcl, rc2)
					ss <- rc[k,]
					h <- cbind(ss[,2], ss[,1])
					tt2 <- table(h[,1])
					j2 <- as.integer(names(tt2[ which(tt2 > 1) ]))
					if (length(j2) > 0) {
						k2 <- h[,1] %in% j2
						hh <- sort(unique(h[k2,2]))
						if (length(hh) > 1) {
							hh <- cbind(hh[-1], hh[1])
							rcl <- rbind(rcl, hh)
						}
						h[h[, 2] %in% h[h[,1] == j2,2], 2] <- min(h[h[,1] == j2,2])
					}
					h <- unique(h)
					xc <- subs(xc, data.frame(h), subsWithNA=FALSE)
					rcl <- rbind(rcl, h)
					
				} else {
					rcl <- rbind(rcl, rc)
				}
			}
		}
		lastrow <- getValues(xc, nrow(xc))
		
		maxval <- maxValue(xc)
		outRaster <- writeValues(outRaster, getValues(xc, 1, tr$nrows[i]), tr$row[i])
		pbStep(pb)
	}
	outRaster <- writeStop(outRaster)
	pbClose(pb)
	
	# now reclass
	if (nrow(rcl) > 1) {
		i = rcl[rcl[,2] %in% rcl[,1],2]
		s = rcl[rcl[,1] %in% i,]
		if (nrow(s) > 0) {
			for (b in 1:nrow(s)) { 
				rcl[rcl[,2]==s[b,1],2] <- s[b,2] 
			}
		}
	
		if (gaps) {
			outRaster <- subs(outRaster, data.frame(rcl), subsWithNA=FALSE, filename=filename, ...)
			return(outRaster)
		} else {
			outRaster <- subs(outRaster, data.frame(rcl), subsWithNA=FALSE, filename='', datatype='INT2U')
			un <- unique(outRaster)
			un <- data.frame(cbind(un, 1:length(un)))
			outRaster <- subs(outRaster, un, filename=filename, ...)
		}
	}
	if (filename == '') {
		filename <- rasterTmpFile()
		return( writeRaster(outRaster, filename=filename, ...) )
	} else {
		return(outRaster)
	}
}

)


