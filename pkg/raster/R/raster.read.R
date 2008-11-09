# R code for reading raster (grid) data
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0,3
# Licence GPL v3


# read entire raster
.raster.read.all <- function(raster) {
	raster <- .raster.read.row(raster, -1)
	return(raster)
}

#read a single row
.raster.read.row <- function(raster, rownr) {
	raster <- .raster.read.part.of.row(raster, rownr)
	return(raster)
}

#read multiple rows
.raster.read.rows <- function(raster, startrow, nrows=3) {
	return(.raster.read.block(raster, startrow, nrows))
}	

#read a block of data  (a rectangular area  of any dimension)  
.raster.read.block <- function(raster, startrow, nrows=3, startcol=1, ncolumns=(ncol(raster)-startcol+1)) {
	if (startrow < 1 ) { stop("startrow too small") } 
	if (startrow > raster@nrows ) { stop("startrow too high") }
	if (nrows < 1) { stop("nrows should be > 1") } 
	if (startcol < 1) { stop("startcol < 1") }
	if (startcol > raster@ncols) { stop("startcol  > raster@ncols")  }
	if (ncolumns < 1) { stop("ncolumns should be > 1") }
	if ((startcol + ncolumns - 1) > raster@ncols ) {
		warning("ncolumns too high, truncated")
		ncolumns <- raster@ncols-startcol }
		
	endrow <- startrow+nrows-1
	if (endrow > raster@nrows) {
		warning("Rows beyond raster not read")
		endrow <- raster@nrows
		nrows <- endrow - startrow + 1
	}
	raster <- .raster.read.part.of.row(raster, startrow, startcol, ncolumns)
	blockdata <- values(raster)
	if (nrows > 1) {
		for (r in (startrow+1):endrow) {
			raster <- .raster.read.part.of.row(raster, r,  startcol, ncolumns)
			blockdata <- c(blockdata, values(raster))
		}	
	}	
	startcell <- get.cell.from.rowcol(raster, startrow, startcol)
	endcell <- get.cell.from.rowcol(raster, endrow, (startcol+ncolumns-1))
	raster <- set.values.block(raster, blockdata, startcell, endcell)
	return(raster)
}


#read part of a single row
.raster.read.part.of.row <- function(raster, rownr,  startcol=1, ncolumns=(ncol(raster)-startcol+1)) {
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

	if (raster@file@driver == 'raster') {
		rastergri <- file.change.extension(raster@file@name, ".gri")
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
			result <- readBin(con, what=dtype, n = ncells(raster), size = raster@file@datasize, endian = raster@file@byteorder) 
		}
		close(con)
		result[is.nan(result)] <- NA
		result[result <=  (0.999 * raster@file@nodatavalue) ] <- NA 
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
		raster@data@indices <- c(1, ncells(raster))
		raster@data@content <- "all"
		raster <- set.minmax(raster)
	} else if (startcol==1 & ncolumns==(ncol(raster)-startcol+1)) {
		raster@data@indices <- c(get.cell.from.rowcol(raster, rownr, startcol), get.cell.from.rowcol(raster, rownr, endcol))
		raster@data@content <- "row"
	} else {
		raster@data@indices <- c(get.cell.from.rowcol(raster, rownr, startcol), get.cell.from.rowcol(raster, rownr, endcol))
		raster@data@content <- "block"
	}	
	
	return(raster)
}


#sample while reading and return matrix (for plotting )
.read.skip <- function(raster, maxdim=500) {
	rasdim <- max(ncol(raster), nrow(raster) )
	if (rasdim <= maxdim) { 
		outras <- .raster.read.all(raster)
	} else {
		fact <- maxdim / rasdim
		nc <- trunc(fact * ncol(raster))
		nr <- trunc(fact * nrow(raster))
		colint <- round(ncol(raster) / nc)
		rowint <- round(nrow(raster) / nr)
		nc <- trunc(ncol(raster) / colint)
		nr <- trunc(nrow(raster) / rowint)
		cols <- 1:nc
		cols <- 1 + (cols-1) * colint 
		for (i in 1:nr) {
			row <- 1 + (i-1) * rowint
			raster <- read.row(raster, row)
			if (i == 1) {
				dd <- values(raster)[cols]
			} else {
				dd <- c(dd, values(raster)[cols])
			}
		}	
		outras <- set.raster(raster)
		outras <- set.rowcol(outras, nr, nc)
		xmx <- xmax(raster) - (ncol(raster) - cols[nc]) * xres(raster)
		ymn <- ymin(raster) + (nrow(raster) - row) * yres(raster)
		outras <- set.bbox(outras, xmx=xmx, ymn=ymn)
		outras <- set.values(outras, dd)
	}
	return(outras)
}


#read data on the raster for xy coordinates
.raster.read.xy <- function(raster, xy) {
	if (!is.matrix(xy)) { xy <- as.matrix(t(xy)) }
	cells <- get.cell.from.xy(raster, xy)
	return(.raster.read.cells(raster, cells))
}	


#read data on the raster for cell numbers
.raster.read.cells <- function(raster, cells) {
	uniquecells <- na.omit(unique(cells[order(cells)]))
	uniquecells <- uniquecells[(uniquecells > 0) & (uniquecells <= ncells(raster))]
	res <- cbind(cells, NA)
	if (length(uniquecells) > 0) {
		if (raster@file@driver == 'gdal') {
			vals <- (.read.cells.gdal(raster, uniquecells))
		} else {
			vals <- (.read.cells.raster(raster, uniquecells))
		}
		for (i in 1:length(vals[,1])) {
			res[res[,1]==vals[i,1],2] <- vals[i,2] 
		}
	}	
	return(res[,2])
}


.read.cells.gdal <- function(raster, cells) {
	colrow <- matrix(ncol=5, nrow=length(cells))
#	valuename <- raster@file@shortname
#	if (valuename == "") {valuename <- "value" }
#	colnames(colrow) <- c("id", "colnr", "rownr", "cell", valuename)
	for  (i in 1:length(cells)) {
		colrow[i,1] <- get.col.from.cell(raster, cells[i])
		colrow[i,2] <- get.row.from.cell(raster, cells[i])
		colrow[i,3] <- cells[i]
		colrow[i,4] <- NA
	}	
	rows <- na.omit(unique(colrow[order(colrow[,2]), 2]))
	for (i in 1:length(rows)) {
		raster <- .raster.read.row(raster, rows[i])
		thisrow <- subset(colrow, colrow[,2] == rows[i])
		for (j in 1:length(thisrow[,1])) {
			colrow[colrow[,3]==thisrow[j,3],4] <- raster@data@values[thisrow[j,1]]
		}	
	}
	return(colrow[,3:4]) 
}	



.read.cells.raster <- function(raster, cells) {
	cells <- cbind(cells, NA)
#	valuename <- raster@file@shortname
#	if (valuename == "") {valuename <- "value" }
#	colnames(cells) <- c("id", "cell", valuename)
#	uniquecells <- na.omit(unique(cells[order(cells[,2]),2]))
	
	rastergri <- file.change.extension(filename(raster), ".gri")
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
	res[res <=  max(-3e+38, raster@file@nodatavalue)] <- NA
	return(cbind(cells,res))
}


