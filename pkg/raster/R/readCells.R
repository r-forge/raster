# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.8
# Licence GPL v3


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
	dsize <- dataSize(raster@file@datanotation)
	dtype <- .shortDataType(raster@file@datanotation)
	for (i in 1:length(cells)) {
		seek(con, (cells[i]-1) * dsize)
		if (dtype == "FLT") { 
			dtype <- "numeric"
		} else { 
			dtype <- "integer"
		}
		res[i] <- readBin(con, what=dtype, n=1, size=dsize, endian=raster@file@byteorder) 
	}
	close(con)
	res[res <=  max(-3e+38, .nodatavalue(raster))] <- NA
	return(cbind(cells,res))
}

