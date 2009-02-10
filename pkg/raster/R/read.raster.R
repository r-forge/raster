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
	blockvalues <- values(raster)
	if (nrows > 1) {
		for (r in (startrow+1):endrow) {
			raster <- .rasterRead(raster, r,  startcol, ncolumns)
			blockvalues <- c(blockvalues, values(raster))
		}	
	}	
	raster@data@values <- blockvalues
	raster@data@content <- 'block' 
	firstcell <- cellFromRowCol(raster, startrow, startcol)
	lastcell <- cellFromRowCol(raster, endrow, (startcol+ncolumns-1))
	raster@data@indices <- c(firstcell, lastcell)

	return(raster)
}


#read part of a single row
.rasterRead <- function(raster, rownr,  startcol=1, ncolumns=(ncol(raster)-startcol+1)) {
	rownr <- round(rownr)
	if (rownr == 0) { stop("rownr == 0. It should be between 1 and nrow(raster), or < 0 for all rows") }
	if (rownr > nrow(raster)) { stop("rownr too high") }
	if (startcol < 1) { stop("startcol < 1") }
	if (startcol > ncol(raster)) { stop("startcol > ncol(raster)") }
	if (ncolumns < 1) { stop("ncols should be > 1") }

	endcol <- startcol + ncolumns - 1
	if (endcol > ncol(raster)) { 
		endcol <- ncol(raster) 
		ncolumns <- ncol(raster) - startcol + 1  
	}
	
	if (dataSource(raster)=='ram') {
		result <- valuesRow(raster, rownr)[startcol:endcol]
	} else 	if (.driver(raster) == 'raster') {
	
		rastergri <- .setFileExtensionValues(filename(raster))
		if (!file.exists( filename(raster))) { 
			stop(paste(filename(raster)," does not exist"))
		}
		con <- file(rastergri, "rb")
		if (raster@file@datatype == "ascii") {
			stop("this type of ascii raster is not supported yet")
		} else if (raster@file@datatype == "integer" | raster@file@datatype == "logical" ) { 
			dtype <- "integer"
		} else { 
			dtype <- "numeric" 
		}
		if (rownr > 0) {
			seek(con, ((rownr-1) * ncol(raster) + (startcol-1)) * raster@file@datasize)
			result <- readBin(con, what=dtype, n=ncolumns, size=raster@file@datasize, signed=raster@file@datasigned, endian=raster@file@byteorder) }	
		else {	
			result <- readBin(con, what=dtype, n=ncell(raster), size=raster@file@datasize, signed=raster@file@datasigned, endian=raster@file@byteorder) 
		}
		close(con)
		result[is.nan(result)] <- NA
		if (dtype == 'numeric') {
			result[result <=  (0.999 * .nodatavalue(raster)) ] <- NA 	
		} else {
			result[result == raster@file@nodatavalue ] <- NA 			
		}
		if (raster@file@datatype == 'logical') {
			result <- as.logical(result)
		}
	}
	else { #use GDAL  
		if (is.na(raster@file@band)) { result <- NA }
		else {
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