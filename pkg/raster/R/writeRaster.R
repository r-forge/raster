# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


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
			raster <- setDatatype(raster, 'INT2S')
			raster@data@values <- as.integer(round(values(raster)))
			raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)						
		} else if (xmin(raster) > -2147483647 & xmax(raster) < 2147483648 ) {
			raster <- setDatatype(raster, 'INT4S')
			raster@data@values <- as.integer(round(values(raster)))
			raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)			
		} else if (xmin(raster) > -(2^63/2) & xmax(raster) < (2^64/2)) {
			raster <- setDatatype(raster, 'INT8S')
			raster@data@values <- as.integer(round(values(raster)))
			raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)			
		} else {
			raster <- setDatatype(raster, 'FLT8S')
			raster@data@values <- as.numeric(values(raster))
		}
	} else if ( raster@file@datatype =='LOGICAL') {
		raster@data@values <- as.integer(values(raster))
		raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)
	} else {
		if (xmin(raster) < -3.4E38 | xmax(raster) > 3.4E38) {
			raster <- setDatatype(raster, 'FLT8S')
		} else {
			raster <- setDatatype(raster, 'FLT4S')
		}	
	}

	if (raster@data@content == 'sparse') { 
		raster <- .writeSparse(raster, overwrite=overwrite) 
	} else {
		binraster <- .setFileExtensionValues(filename(raster))
		con <- file(binraster, "wb")
		writeBin( values(raster), con, size = raster@file@datasize) 
		close(con)
		.writeRasterHdr(raster) 
	}	
	
	# put logical values back to T/F
	if ( raster@file@datatype =='logical') {
		raster@data@values[raster@data@values <=  raster@file@nodatavalue]  <- NA
		raster@data@values <- as.logical(values(raster))
	}
	
	return(raster)
}
 
 

 
 .startRowWriting <- function(raster, overwrite) {
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
	raster@data@min <- Inf
	raster@data@max <- -Inf
	raster@data@haveminmax <- FALSE
	raster@file@driver <- 'raster'
	raster@file@gdalhandle <- list()
	return(raster)
}

.stopRowWriting <- function(raster) {
	.writeRasterHdr(raster) 
	close(raster@filecon)
	raster@data@haveminmax <- TRUE
	raster@data@source <- 'disk'
	raster@data@content <- 'nodata'
	raster@data@values <- vector(length=0)
	return(raster)
}		
 
 
.writeRasterRow <- function(raster, overwrite=FALSE) {
#	if (dataContent(raster) != 'row') { 
#		stop('raster does not contain a row') 
#	}
	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	
	if (raster@file@datatype == "integer" |  raster@file@datatype =='logical' ) { 
		values <- as.integer(round(raster@data@values))  
		values[is.na(values)] <- as.integer(raster@file@nodatavalue)		
	} else { 
		values  <- as.numeric(raster@data@values) 
	}
	if (dataIndices(raster)[1] == 1) { 
		raster <- .startRowWriting(raster, overwrite=overwrite)
 	} 
	
	rsd <- na.omit(raster@data@values) # min and max values
	if (length(rsd) > 0) {
		raster@data@min <- min(raster@data@min, min(rsd))
		raster@data@max <- max(raster@data@max, max(rsd))
	}	
	
	writeBin(values, raster@filecon, size = raster@file@datasize)
	
	if (dataIndices(raster)[2] >= ncell(raster)) {
		raster <- .stopRowWriting(raster)
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
		raster <- setDatatype(raster, 'INT4S')
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
	cat("DataType=",  raster@file@datanotation, "\n", file = thefile)
	cat("ByteOrder=",  .Platform$endian, "\n", file = thefile)
	cat("nbands=",  nbands(raster), "\n", file = thefile)
	cat("bandOrder=",  raster@file@bandorder, "\n", file = thefile)
	cat("minValue=",  minValue(raster), "\n", file = thefile)
	cat("maxValue=",  maxValue(raster), "\n", file = thefile)
	cat("NoDataValue=", .nodatavalue(raster), "\n", file = thefile)
#	cat("Sparse=", raster@sparse, "\n", file = thefile)
#	cat("nCellvals=", raster@data@ncellvals, "\n", file = thefile)	
	close(thefile)
}


