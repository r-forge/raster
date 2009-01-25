# R code for reading raster (grid) data
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0,4
# Licence GPL v3


#read a block of data  (a rectangular area  of any dimension)  
.rasterReadBlock <- function(raster, startrow, nrows=3, startcol=1, ncolumns=(ncol(raster)-startcol+1)) {
	if (startrow < 1 ) { stop("startrow too small") } 
	if (startrow > nrow(raster) ) { stop("startrow too high") }
	if (nrows < 1) { stop("nrows should be > 1") } 
	if (startcol < 1) { stop("startcol < 1") }
	if (startcol > ncol(raster)) { stop("startcol > ncol(raster)")  }
	if (ncolumns < 1) { stop("ncolumns should be > 1") }
	if ((startcol + ncolumns - 1) > ncol(raster) ) {
		warning("ncolumns too high, truncated")
		ncolumns <- ncol(raster)-startcol }
		
	endrow <- startrow+nrows-1
	if (endrow > nrow(raster)) {
		warning("Rows beyond raster not read")
		endrow <- nrow(raster)
		nrows <- endrow - startrow + 1
	}
	raster <- .rasterRead(raster, startrow, startcol, ncolumns)
	blockdata <- values(raster)
	if (nrows > 1) {
		for (r in (startrow+1):endrow) {
			raster <- .rasterRead(raster, r,  startcol, ncolumns)
			blockdata <- c(blockdata, values(raster))
		}	
	}	
	startcell <- cellFromRowCol(raster, startrow, startcol)
	endcell <- cellFromRowCol(raster, endrow, (startcol+ncolumns-1))
	raster <- setValuesBlock(raster, blockdata, startcell, endcell)
	return(raster)
}


#read part of a single row
.rasterRead <- function(raster, rownr,  startcol=1, ncolumns=(ncol(raster)-startcol+1)) {
	rownr <- round(rownr)
	if (rownr == 0) { stop("rownr == 0. It should be between 1 and nrow(raster), or -1 for all rows") }
	if (rownr > nrow(raster)) { stop("rownr too high") }
	if (startcol < 1) { stop("startcol < 1") }
	if (startcol > ncol(raster)) { stop("startcol > ncol(raster)") }
	if (ncolumns < 1) { stop("ncols should be > 1") }

	endcol <- startcol + ncolumns - 1
	if (endcol > ncol(raster)) { 
		endcol <- ncol(raster) 
		ncolumns <- ncol(raster) - startcol + 1  
	}

	if (.driver(raster) == 'raster') {
		rastergri <- .setFileExtensionValues(filename(raster))
		if (!file.exists( filename(raster))) { 
			stop(paste(filename(raster)," does not exist"))
		}
		con <- file(rastergri, "rb")
		if (raster@file@datatype == "integer") { 
			dtype <- integer()
		} else { 
			dtype <- numeric() 
		}
		if (rownr > 0) {
			seek(con, ((rownr-1) * ncol(raster) + (startcol-1)) * raster@file@datasize)
			result <- readBin(con, what=dtype, n = ncolumns, size = raster@file@datasize, endian = raster@file@byteorder) }	
		else {	
			result <- readBin(con, what=dtype, n = ncell(raster), size = raster@file@datasize, endian = raster@file@byteorder) 
		}
		close(con)
		result[is.nan(result)] <- NA
		result[result <=  (0.999 * .nodatavalue(raster)) ] <- NA 
	}
	else { #use GDAL  
		if (is.na(raster@file@band)) { result <- NA }
		else {
			if (rownr > nrow(raster)) {
				stop("rownr too high")
			}
			if (rownr <= 0) {
				offs <- c(0, 0) 
				reg <- c(nrow(raster), ncol(raster)) #	reg <- dim(raster@file@gdalhandle[[1]])
			}
			else {
				offs= c((rownr-1), (startcol-1)) 
				reg <- c(1, ncolumns)
			}
		}
		result <- getRasterData(raster@file@gdalhandle[[1]], offset=offs, region.dim=reg, band = raster@file@band)
		if (!is.vector(result)) { result <- as.vector(result) }
	} 
	raster@data@values <- as.vector(result)
	if (rownr < 0) {
		raster@data@indices <- c(1, ncell(raster))
		raster@data@content <- "all"
		raster <- setMinMax(raster)
	} else if (startcol==1 & ncolumns==(ncol(raster)-startcol+1)) {
		raster@data@indices <- c(cellFromRowCol(raster, rownr, startcol), cellFromRowCol(raster, rownr, endcol))
		raster@data@content <- "row"
	} else {
		raster@data@indices <- c(cellFromRowCol(raster, rownr, startcol), cellFromRowCol(raster, rownr, endcol))
		raster@data@content <- "block"
	}	
	
	return(raster)
}


#sample while reading and return matrix (for plotting )

readRandom <- function(raster, n=500, na.rm = TRUE) {
	if (dataContent(raster) == 'all') {
		values <- values(raster)
		if (na.rm) { values <- na.omit(values) }
		if (length(values) > n) {
			r <- order(runif(length(values)))
			values <- values[r]
			values <- values[1:n]
		}
	} else {
		if (dataSource(raster) == 'disk') {
			if (ncell(raster) <= n) {
				raster <- readAll(raster)
				values <- cbind(1:ncell(raster), values(raster))
				if (na.rm) { values <- na.omit(values) }
			} else {	
				if (na.rm) {
					N <- n 
				} else {
					N <- 2 * n 
				}	
				cells <- unique(as.integer(round(runif(N) * ncell(raster) + 0.5)))
				cells <- cells[cells > 0]
				values <- cellValues(raster, cells)
				if (na.rm) {
					values <- na.omit(values)
					if (length(values) >= n) {
						values <- values[1:n]
					}
				}	
			}
		}
	}	
	return(values)
}



readSkip <- function(raster, maxdim=500, bndbox=NA, asRaster=FALSE) {
	if (!(is.na(bndbox))) { 
		rcut <- crop(raster, bndbox) 
		warning('bndbox option has not been implemented yet')
	} else {
		rcut <- setRaster(raster)
	}
	# Need to do something with this now.....
	
	rasdim <- max(ncol(raster), nrow(raster) )
	if (rasdim <= maxdim) { 
		if (dataContent(raster) == 'all') {
			outras <- raster
		} else { 
			outras <- readAll(raster) 
		}
	} else {
		fact <- maxdim / rasdim
		nc <- max(1, trunc(fact * ncol(raster)))
		nr <- max(1, trunc(fact * nrow(raster)))
		colint <- round(ncol(raster) / nc)
		rowint <- round(nrow(raster) / nr)
		nc <- trunc(ncol(raster) / colint)
		nr <- trunc(nrow(raster) / rowint)
		cols <- 1:nc
		cols <- 1 + (cols-1) * colint 
		dd <- vector()
		if (dataContent(raster) == 'all') {
			for (i in 1:nr) {
				row <- 1 + (i-1) * rowint
				v <- values(raster, row)
				dd <- c(dd, v[cols])
			}	
		} else {
			for (i in 1:nr) {
				row <- 1 + (i-1) * rowint
				raster <- readRow(raster, row)
				dd <- c(dd, values(raster)[cols])
			}	
		}	
		outras <- setRaster(raster)
		outras <- setRowCol(outras, nr, nc)
		xmx <- xmax(raster) - (ncol(raster) - cols[nc]) * xres(raster)
		ymn <- ymin(raster) + (nrow(raster) - row) * yres(raster)
		bndbox <- changeBbox(raster, xmx=xmx, ymn=ymn)
		outras <- setBbox(outras, bndbox, keepres=F)
		outras <- setValues(outras, dd)
	}
	if (asRaster) {
		return(outras)
	} else {
		return(values(outras))
	}	
}

#.readrandom
#			if (length(na.omit(values(x))) > maxcell) {
#				v <- na.omit(cbind(values(x), values(y)))
#				r <- order(runif(length(v[,1])))
#				v <- v[r,]
#				l <- min(maxcell, length(v))
#				v <- v[1:l,]
#				warning(paste("plot used a sample of ", l, " cells (with data; ", maxcell, " when counting NA cells)", sep=""))
#				x <- v[,1]
#				y <- v[,2]




#read data on the raster for cell numbers
.rasterReadCells <- function(raster, cells) {
	uniquecells <- na.omit(unique(cells[order(cells)]))
	uniquecells <- uniquecells[(uniquecells > 0) & (uniquecells <= ncell(raster))]
	res <- cbind(cells, NA)
	if (length(uniquecells) > 0) {
		if (dataContent(raster) == 'all') {
			vals <- cbind(uniquecells, values(raster)[uniquecells])
		} else if (dataSource(raster) == 'disk') {
			if (.driver(raster) == 'gdal') {
				vals <- .readCellsGDAL(raster, uniquecells)
			} else {
				vals <- .readCellsRaster(raster, uniquecells)
			}	
		} else { 
			vals <- cbind(uniquecells, NA)
		}	
		if (length(vals) == 2) {
			res[res[,1]==vals[1],2] <- vals[2] 
		} else {
			for (i in 1:length(vals[,1])) {
				res[res[,1]==vals[i,1],2] <- vals[i,2] 
			}	
		}
	}	
	return(res[,2])
}


.readCellsGDAL <- function(raster, cells) {
	colrow <- matrix(ncol=5, nrow=length(cells))
#	valuename <- raster@file@shortname
#	if (valuename == "") {valuename <- "value" }
#	colnames(colrow) <- c("id", "colnr", "rownr", "cell", valuename)
	for  (i in 1:length(cells)) {
		colrow[i,1] <- colFromCell(raster, cells[i])
		colrow[i,2] <- rowFromCell(raster, cells[i])
		colrow[i,3] <- cells[i]
		colrow[i,4] <- NA
	}	
	rows <- na.omit(unique(colrow[order(colrow[,2]), 2]))
	for (i in 1:length(rows)) {
		raster <- .rasterRead(raster, rows[i])
		thisrow <- subset(colrow, colrow[,2] == rows[i])
		for (j in 1:length(thisrow[,1])) {
			colrow[colrow[,3]==thisrow[j,3],4] <- raster@data@values[thisrow[j,1]]
		}	
	}
	return(colrow[,3:4]) 
}	



.readCellsRaster <- function(raster, cells) {
#	cells <- cbind(cells, NA)
#	valuename <- raster@file@shortname
#	if (valuename == "") {valuename <- "value" }
#	colnames(cells) <- c("id", "cell", valuename)
#	uniquecells <- na.omit(unique(cells[order(cells[,2]),2]))
	
	rastergri <- .setFileExtensionValues(filename(raster))
	if (!file.exists(filename(raster))) { stop(paste(filename(raster)," does not exist")) }
	con <- file(rastergri, "rb")

	res <- vector(length=length(cells))
	res[] <- NA
	for (i in 1:length(cells)) {
		seek(con, (cells[i]-1) * raster@file@datasize)
		if (raster@file@datatype == "integer") { dtype <- integer() } else { dtype <- numeric() }
			res[i] <- readBin(con, what=dtype, n=1, size=raster@file@datasize, endian=raster@file@byteorder) 
	}
	close(con)
	res[res <=  max(-3e+38, .nodatavalue(raster))] <- NA
	return(cbind(cells,res))
}


.stackRead <- function(rstack, rownumber, startcol=1, ncolumns=(ncol(rstack)-startcol+1)) {
	for (i in seq(nlayers(rstack))) {
		raster <- readPartOfRow(rstack@layers[[i]], rownumber, startcol, ncolumns)
		if ( i == 1 )  {
			rstack@data@values <- matrix(nrow=length(values(raster)), ncol=nlayers(rstack)) 
			rstack@data@content <- dataContent(raster)
			rstack@data@indices <- dataIndices(raster)
		}
		rstack@data@values[,i] <- values(raster)
	}
	return(rstack)
}


.stackReadCells <- function(object, cells) {
		for (i in seq(nlayers(object))) {
			v <- .rasterReadCells(object@layers[[i]], cells)
			if (i == 1) {
				result <- v
			} else {
				result <- cbind(result, v)
	#			colnames(result)[length(result[1,])] <- rstack@layers[[i]]@file@shortname
			}
		}
		if (!(is.null(dim(result)))) {
			for (i in seq(nlayers(object))) {
				colnames(result) <- object@data@colnames
			}
		}	
		return(result)
}