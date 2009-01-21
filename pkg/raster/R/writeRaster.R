# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


 
rasterFormats <- function() {
	gd <- gdalDrivers()
	gd <- as.matrix(subset(gd, gd[,3] == T))
	short <- c("raster", "ascii", as.vector(gd[,1]))
	long <- c("raster package format", "Arc ascii", as.vector(gd[,2]))
	m <- cbind(short, long)
	colnames(m) <- c("name", "long_name")
	return(m)
}


.isSupportedGDALFormat <- function(dname) {
	gd <- gdalDrivers()
	gd <- as.matrix(subset(gd, gd[,3] == T))
	res <- dname %in% gd
	if (!res) { stop(paste(dname, "is not a supported file format. See rasterFormats()" ) ) }
	return(res)
}

 
 
writeRaster <- function(raster, format='raster', overwrite=FALSE) {
	
	if (dataContent(raster) != 'row' & dataContent(raster) != 'all' & dataContent(raster) != 'sparse' ) {
		stop('First use setValues()')
	}

	if (format=='raster') {
		if (dataContent(raster) == 'row' ) {
			raster <- .writeRasterRow(raster, overwrite)
		} else {
			raster <- .writeRasterAll(raster, overwrite)
		}  
	} else if (format=='ascii') {
		raster <- .writeAscii(raster, overwrite)
	} else { 
		.isSupportedGDALFormat(format)
		if (dataContent(raster) == 'row' ) {
			raster <- .writeGDALrow(raster, format, overwrite, ForceIntOutput=FALSE, mvFlag=NA, options=NULL)
		} else {
			raster <- .writeGDALall(raster, format, overwrite, ForceIntOutput=FALSE, mvFlag=NA, options=NULL)
		}  
	}
	return(raster)
}	





.setFileExtensionValues <- function(fname) {
	fname <- setFileExtension(fname, ".gri")
	return(fname)
}
 
.setFileExtensionHeader <- function(fname) {
	fname <- setFileExtension(fname, ".grd")
	return(fname)
}
 

.writeRasterAll <- function(raster, overwrite=FALSE) {
	raster <- setFilename(raster, .setFileExtensionHeader(filename(raster)))
	if (filename(raster) == "") {
		stop('first provide a filename. E.g.: raster <- setFilename(raster, "c:/myfile")')
	}
	if (!overwrite & file.exists(filename(raster))) {
		stop(paste(filename(raster),"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	raster@file@driver <- 'raster'
	raster@file@gdalhandle <- list()
	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	raster <- setMinMax(raster)

	if ( raster@file@datatype =='integer') {
		if (xmin(raster) > -32767 & xmax(raster) < 32768) {
			raster <- setDatatype(raster, 'integer', datasize=2)
			raster@data@values <- as.integer(round(values(raster)))
		} else if (xmin(raster) > -2147483647 & xmax(raster) < 2147483648 ) {
			raster <- setDatatype(raster, 'integer', datasize=4)
			raster@data@values <- as.integer(round(values(raster)))
		} else if (xmin(raster) > -(2^63/2) & xmax(raster) < (2^64/2)) {
			raster <- setDatatype(raster, 'integer', datasize=8)
			raster@data@values <- as.integer(round(values(raster)))
		} else {
			raster <- setDatatype(raster, 'numeric', datasize=8)
			raster@data@values <- as.numeric(values(raster))
		}
	} else {
		if (xmin(raster) < -3.4E38 | xmax(raster) > 3.4E38) {
			raster <- setDatatype(raster, 'numeric', 8)
		} else {
			raster <- setDatatype(raster, 'numeric', 4)
		}	
	}

	if (raster@data@content == 'sparse') { 
		raster <- .writeSparse(raster, overwrite) 
	} else {
		binraster <- .setFileExtensionValues(filename(raster))
		con <- file(binraster, "wb")
		writeBin( values(raster), con, size = raster@file@datasize) 
		close(con)
		.writeRasterHdr(raster) 
	}	
	return(raster)
}
 
 ..startWriting <- function(raster, overwrite) {
	raster <- setFilename(raster, .setFileExtensionHeader(filename(raster)))
	if (filename(raster) == "") {
		stop('first provide a filename. E.g.: raster <- setFilename(raster, "c:/myfile")')
	}
	if (!overwrite & file.exists(filename(raster))) {
		stop(paste(filename(raster),"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	raster@file@name <- .setFileExtensionHeader(filename(raster))
	binraster <- .setFileExtensionValues(filename(raster))
	attr(raster, "filecon") <- file(binraster, "wb")
	raster@data@min <- 3e34
	raster@data@max <- -3e34
	raster@data@haveminmax <- FALSE
	raster@file@driver <- 'raster'
	raster@file@gdalhandle <- list()
	return(raster)
}

..stopWriting <- function(raster) {
	.writeRasterHdr(raster) 
	close(raster@filecon)
	raster@data@haveminmax <- TRUE
	raster@data@source <- 'disk'
	raster@data@content <- 'nodata'
	raster@data@values <- vector(length=0)
	return(raster)
}		
 
 
.writeRasterRow <- function(raster, overwrite=FALSE) {
	if (dataContent(raster) != 'row') { 
		stop('raster does not contain a row') 
	}
	if (raster@file@datatype == "integer") { 
		raster@data@values <- as.integer(round(raster@data@values))  
	}
	if (class(values(raster)) == "integer" & raster@file@datatype == "numeric") { 
		raster@data@values  <- as.numeric(values(raster)) 
	}
	if (dataIndices(raster)[1] == 1) { 
		raster <- ..startWriting(raster, overwrite)
 	} 
	
	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA

	writeBin(raster@data@values, raster@filecon, size = raster@file@datasize)
	
	if (dataIndices(raster)[2] >= ncell(raster)) {
		raster <- ..stopWriting(raster)
		if (dataIndices(raster)[2] > ncell(raster)) {
			warning(paste('You have written beyond the end of file. last cell:', dataIndices(raster)[2], '>', ncell(raster)))
		}
	}
	return(raster)	
}



.writeSparse <- function(raster, overwrite=FALSE) {

	raster@file@driver <- 'raster'
    raster@file@gdalhandle <- list()
	raster <- setFilename(raster, .setFileExtensionHeader(filename(raster)))
	if (!overwrite & file.exists(filename(raster))) {
		stop(paste(filename(raster), "exists. Use 'overwrite=TRUE' if you want to overwrite it")) 
	}

	raster@data@values[is.nan(values(raster))] <- NA
	if (raster@file@datatype == "integer") { 
		raster@data@values <- as.integer(values(raster)) 
	}
	if (class(values(raster))=='integer') {
		raster <- setDatatype(raster, 'integer')
	}	
	raster <- setMinMax(raster)

	binraster <- .setFileExtensionValues(filename(raster))
	con <- file(binraster, "wb")
	writeBin( as.vector(dataIndices(raster)), con, size = as.integer(4)) 
	writeBin( as.vector(values(raster)), con, size = raster@file@datasize) 
	close(con)

	# add the 'sparse' key word to the hdr file!!!
	.writeRasterHdr(raster) 
	return(raster)
} 


.writeRasterHdr <- function(raster) {
	rastergrd <- .setFileExtensionHeader(filename(raster))
	thefile <- file(rastergrd, "w")  # open an txt file connectionis
	cat("[general]", "\n", file = thefile)
	cat("creator=R package:raster", "\n", file = thefile)
	cat("created=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)
	cat("title=", raster@file@shortname, "\n", file = thefile)
	cat("[georeference]", "\n", file = thefile)
	cat("nrows=",  nrow(raster), "\n", file = thefile)
	cat("ncols=",  ncol(raster), "\n", file = thefile)
	cat("xmin=", xmin(raster), "\n", file = thefile)
	cat("ymin=", ymin(raster), "\n", file = thefile)
	cat("xmax=", xmax(raster), "\n", file = thefile)
	cat("ymax=", ymax(raster), "\n", file = thefile)
	cat("xres=", xres(raster), "\n", file = thefile)
	cat("yres=", yres(raster), "\n", file = thefile)
	cat("projection=", projection(raster), "\n", file = thefile)
	cat("[data]", "\n", file = thefile)
	if (raster@file@datatype == 'ascii') {  
		datatype <- "ASC" 
	} else if (raster@file@datatype == 'integer') {  
		datatype <- "INT"  
	} else { 
		datatype <- "FLT" 
	}
	if (datatype != "ASC") {
		datatype <- paste(datatype, raster@file@datasize, "BYTES", sep="")
		cat("DataType=",  datatype, "\n", file = thefile)
		cat("ByteOrder=",  .Platform$endian, "\n", file = thefile)
	}	
	cat("nbands=",  nbands(raster), "\n", file = thefile)
	cat("bandOrder=",  raster@file@bandorder, "\n", file = thefile)
	cat("minValue=",  minValue(raster), "\n", file = thefile)
	cat("maxValue=",  maxValue(raster), "\n", file = thefile)
	cat("NoDataValue=", .nodatavalue(raster), "\n", file = thefile)
#	cat("Sparse=", raster@sparse, "\n", file = thefile)
#	cat("nCellvals=", raster@data@ncellvals, "\n", file = thefile)	
	close(thefile)
}


