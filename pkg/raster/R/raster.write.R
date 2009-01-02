# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


 

writeValues <- function(raster, overwrite=FALSE) {
	if (dataContent(raster) == 'row' ) {
		raster <- .writeValuesRow(raster, overwrite)
	} else if (dataContent(raster) != 'all' & dataContent(raster) != 'sparse' ) {
		stop('there are not (enough) values to write the file. First use setValues(); or use writeValues') 
	} else {
		raster <- .writeValuesAll(raster, overwrite)
	}  
	return(raster)
}



.writeValuesAll <- function(raster, overwrite=FALSE) {
	raster <- setFilename(raster, setFileExtension(filename(raster), ".grd"))
	if (!overwrite & file.exists(raster@file@name)) {
		stop(paste(raster@file@name,"exists.","use 'overwrite=TRUE' if you want to overwrite it")) }

	raster@file@driver <- 'raster'
	raster@file@gdalhandle <- list()
	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	raster <- setMinmax(raster)

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
		raster <- setDatatype(raster, 'numeric')
		if (xmin(raster) > -3.4E38 & xmax(raster) < 3.4E38) {
			raster <- setDatatype(raster, 'numeric', 8)
		}	
	}

	if (raster@data@content == 'sparse') { 
		raster <- .write.sparse(raster, overwrite) 
	} else {
		binraster <- setFileExtension(filename(raster), ".gri")
		con <- file(binraster, "wb")
		writeBin( values(raster), con, size = raster@file@datasize) 
		close(con)
		writeHeader(raster) 
	}	
	return(raster)
}
 
 
 
.writeValuesRow <- function(raster, overwrite=FALSE) {
	if (raster@data@content != 'row') { stop('raster does not contain a row') }
	
	if (raster@data@indices[1] == 1) {
 	#  FIRST  ROW
		if (!overwrite & file.exists(filename(raster))) {
			stop(paste(filename(raster),"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
		}
		raster@file@name <- setFileExtension(raster@file@name, ".grd")
		binraster <- setFileExtension(raster@file@name, ".gri")
		attr(raster, "filecon") <- file(binraster, "wb")
		raster@data@min <- 3e34
		raster@data@max <- -3e34
		raster@data@haveminmax <- FALSE
		raster@file@driver <- 'raster'
		raster@file@gdalhandle <- list()
	}	

	if (raster@file@datatype == "integer") { raster@data@values <- as.integer(round(raster@data@values))  }
	if (class(values(raster)) == "integer" & raster@file@datatype == "numeric") { raster@data@values  <- as.numeric(values(raster)) }
	
	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	rsd <- na.omit(raster@data@values) # min and max values
	if (length(rsd) > 0) {
		raster@data@min <- min(raster@data@min, min(rsd))
		raster@data@max <- max(raster@data@max, max(rsd))
	}	

#	raster@data@values[is.na(raster@data@values)] <-  raster@file@nodatavalue
	writeBin(as.vector(raster@data@values), raster@filecon, size = raster@file@datasize)
	
	if (raster@data@indices[2] == ncells(raster)) {
	# LAST  ROW
		writeHeader(raster) 
		close(raster@filecon)
		raster@data@haveminmax <- TRUE
		raster@data@source <- 'disk'
		raster@data@content <- 'nodata'
		raster@data@values <- vector(length=0)
	}		
	if (raster@data@indices[2] > ncells(raster)) {
		stop(paste('writing beyond end of file. last cell:', raster@data@indices[2], '>', ncells(raster)))
	}
	return(raster)	
}


.write.sparse <- function(raster, overwrite=FALSE) {

	raster@file@driver <- 'raster'
    raster@file@gdalhandle <- list()
	raster <- setFilename(raster, setFileExtension(filename(raster), ".grd"))
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
	raster <- setMinmax(raster)

	binraster <- setFileExtension(raster@file@name, ".gri")
	con <- file(binraster, "wb")
	writeBin( as.vector(dataIndices(raster)), con, size = as.integer(4)) 
	writeBin( as.vector(values(raster)), con, size = raster@file@datasize) 
	close(con)

	# add the 'sparse' key word to the hdr file!!!
	writeHeader(raster) 
	return(raster)
} 




writeHeader <- function(raster) {
	rastergrd <- setFileExtension(filename(raster), ".grd")
	thefile <- file(rastergrd, "w")  # open an txt file connectionis
	cat("[General]", "\n", file = thefile)
	cat("CREATOR=R package:raster", "\n", file = thefile)
	cat("CREATED=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)
	cat("TITLE=", raster@file@shortname, "\n", file = thefile)
	
	cat("[GeoReference]", "\n", file = thefile)
	cat("Rows=",  nrow(raster), "\n", file = thefile)
	cat("Columns=",  ncol(raster), "\n", file = thefile)
	cat("MinX=", xmin(raster), "\n", file = thefile)
	cat("MinY=", ymin(raster), "\n", file = thefile)
	cat("MaxX=", xmax(raster), "\n", file = thefile)
	cat("MaxY=", ymax(raster), "\n", file = thefile)
	cat("ResolutionX=", xres(raster), "\n", file = thefile)
	cat("ResolutionY=", yres(raster), "\n", file = thefile)
	cat("Projection=", projection(raster), "\n", file = thefile)
	
	cat("[Data]", "\n", file = thefile)
	if (raster@file@datatype == 'integer') {  datatype <- "ForceIntOutput"  } else { datatype <- "FLT" }
	datatype <- paste(datatype, raster@file@datasize, "BYTES", sep="")
	cat("DataType=",  datatype, "\n", file = thefile)
	cat("ByteOrder=",  .Platform$endian, "\n", file = thefile)
	cat("nBands=",  raster@file@nbands, "\n", file = thefile)
	cat("BandOrder=",  raster@file@bandorder, "\n", file = thefile)
	cat("MinValue=",  minValue(raster), "\n", file = thefile)
	cat("MaxValue=",  maxValue(raster), "\n", file = thefile)
	cat("NoDataValue=",  raster@file@nodatavalue, "\n", file = thefile)
#	cat("Sparse=", raster@sparse, "\n", file = thefile)
#	cat("nCellvals=", raster@data@ncellvals, "\n", file = thefile)	
	close(thefile)
}




#
#write.gdal <- function(gdata, filename, filetype = "GTiff", gdata) {
#   datatype <- "Float32"
#   writeGDAL(gdata, filename, drivername = filetype, type = datatype, mvFlag = NA, options=NULL)
#}   

