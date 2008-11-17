# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0,1
# Licence GPL v3


write.ascii <- function(raster, overwrite=FALSE) {

  	resdif <- abs((yres(raster) - xres(raster)) / yres(raster) )
	if (resdif > 0.01) {
		print(paste("raster has unequal horizontal and vertical resolutions","\n", "these data cannot be stored in arc-ascii format"))
	} else {
		if (raster@data@indices[1] == 1) {
			raster <- set.filename(raster, file.change.extension(filename(raster), '.asc'))
			if (!overwrite & file.exists(filename(raster))) {
				stop(paste(filename(raster), "exists. Use 'overwrite=TRUE'")) }

			thefile <- file(raster@file@name, "w")  # open an txt file connection
			cat("NCOLS", ncol(raster), "\n", file = thefile)
			cat("NROWS", nrow(raster), "\n", file = thefile)
			cat("XLLCORNER", xmin(raster), "\n", file = thefile)
			cat("YLLCORNER", ymin(raster), "\n", file = thefile)
			cat("CELLSIZE",  xres(raster), "\n", file = thefile)
			cat("NODATA_value", raster@file@nodatavalue, "\n", file = thefile)
			close(thefile) #close connection
		}
		raster@data@values[is.na(values(raster))] <- raster@file@nodatavalue 
		write.table(values(raster), filename(raster), append = TRUE, quote = FALSE, 
							sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
    }
	if ( data.indices(raster)[2] >= ncells(raster)) {
		if ( data.indices(raster)[2] > ncells(raster)) {
			stop(paste('writing beyond end of file. last cell:', raster@data@indices[2], '>', ncells(raster)))
		} else {
		# create a new object with gdal handle tfrom the new file
			raster <- raster.from.file(filename(raster)) 
		}
	}
	return(raster)
}
 
 
.write.sparse <- function(raster, overwrite=FALSE) {

	raster@file@driver <- 'raster'
    raster@file@gdalhandle <- list()
	raster <- set.filename(raster, file.change.extension(filename(raster), ".grd"))
	if (!overwrite & file.exists(filename(raster))) {
		stop(paste(filename(raster), "exists. Use 'overwrite=TRUE' if you want to overwrite it")) 
	}

	raster@data@values[is.nan(values(raster))] <- NA
	if (raster@file@datatype == "integer") { 
		raster@data@values <- as.integer(values(raster)) 
	}
	if (class(values(raster))=='integer') {
		raster <- set.datatype(raster, 'integer')
	}	
	raster <- set.minmax(raster)

	binraster <- file.change.extension(raster@file@name, ".gri")
	con <- file(binraster, "wb")
	writeBin( as.vector(data.indices(raster)), con, size = as.integer(4)) 
	writeBin( as.vector(values(raster)), con, size = raster@file@datasize) 
	close(con)

	# add the 'sparse' key word to the hdr file!!!
	.write.hdr.grd(raster) 
	return(raster)
} 

.write.raster.grd <- function(raster, INT=FALSE, overwrite=FALSE) {
	raster <- set.filename(raster, file.change.extension(filename(raster), ".grd"))
	if (!overwrite & file.exists(raster@file@name)) {
		stop(paste(raster@file@name,"exists.","use 'overwrite=TRUE' if you want to overwrite it")) }

	raster@file@driver <- 'raster'
	raster@file@gdalhandle <- list()
	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	raster <- set.minmax(raster)

	
	if (class(values(raster))=='integer' | INT==TRUE) {
		if (INT==TRUE) {
			raster@data@values <- as.integer(values(raster))
		}
		if (xmin(raster) > -32767 & xmax(raster) < 32768) {
			raster <- set.datatype(raster, 'integer', datasize=2)
		} else if (xmin(raster) > -2147483647 & xmax(raster) < 2147483648 ) {
			raster <- set.datatype(raster, 'integer', datasize=4)
		} else if (xmin(raster) > -(2^63/2) & xmax(raster) < (2^64/2)) {
			raster <- set.datatype(raster, 'integer', datasize=8)
		} else {
			raster <- set.datatype(raster, 'numeric', datasize=8)
			raster@data@values <- as.numeric(values(raster))
		}
	} else 	if (class(values(raster))=='numeric' ) {
		raster <- set.datatype(raster, 'numeric')
		if (xmin(raster) > -3.4E38 & xmax(raster) < 3.4E38) {
			raster <- set.datatype(raster, 'numeric', 8)
		}	
	}

	if (raster@data@content == 'sparse') { 
		raster <- .write.sparse(raster, overwrite) 
	} else {
		binraster <- file.change.extension(filename(raster), ".gri")
		con <- file(binraster, "wb")
		writeBin( values(raster), con, size = raster@file@datasize) 
		close(con)
		.write.hdr.grd(raster) 
	}	
	return(raster)
}
 
write.raster <- function(raster, type="grd", INT=FALSE, overwrite=FALSE) {

	if (data.content(raster) != 'all' & data.content(raster) != 'sparse' ) {
		stop('there are not (enough) values to write the file. first use set.values()') 
	}
	if (type == "grd") {
		raster <- .write.raster.grd(raster, INT, overwrite)
	} else {
		stop(paste("file type:", type, "is not supported"))
	}
	return(raster)
}

 
write.row <- function(raster, overwrite=FALSE) {
	if (raster@data@content != 'row') { stop('raster does not contain a row') }
	
	if (raster@data@indices[1] == 1) {
 	#  FIRST  ROW
		if (!overwrite & file.exists(filename(raster))) {
			stop(paste(filename(raster),"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
		}
		raster@file@name <- file.change.extension(raster@file@name, ".grd")
		binraster <- file.change.extension(raster@file@name, ".gri")
		attr(raster, "filecon") <- file(binraster, "wb")
		raster@data@min <- 3e34
		raster@data@max <- -3e34 
		raster@data@haveminmax <- FALSE
		raster@file@driver <- 'raster'
		raster@file@gdalhandle <- list()
	}	

	if (raster@file@datatype == "integer") { raster@data@values <- as.integer(raster@data@values)  }
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
		.write.hdr.grd(raster) 
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


.write.hdr.grd <- function(raster) {
	rastergrd <- file.change.extension(filename(raster), ".grd")
	thefile <- file(rastergrd, "w")  # open an txt file connectionis
	cat("[General]", "\n", file = thefile)
	cat("CREATOR=R package:raster", "\n", file = thefile)
	cat("CREATED=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)
	cat("TITLE=", raster@file@shortname, "\n", file = thefile)
	
	cat("[GeoReference]", "\n", file = thefile)
	cat("Rows=",  raster@nrows, "\n", file = thefile)
	cat("Columns=",  raster@ncols, "\n", file = thefile)
	cat("MinX=", xmin(raster), "\n", file = thefile)
	cat("MinY=", ymin(raster), "\n", file = thefile)
	cat("MaxX=", xmax(raster), "\n", file = thefile)
	cat("MaxY=", ymax(raster), "\n", file = thefile)
	cat("ResolutionX=", xres(raster), "\n", file = thefile)
	cat("ResolutionY=", yres(raster), "\n", file = thefile)
	
	cat("[Data]", "\n", file = thefile)
	if (raster@file@datatype == 'integer') {  datatype <- "INT"  } else { datatype <- "FLT" }
	datatype <- paste(datatype, raster@file@datasize, "BYTES", sep="")
	cat("DataType=",  datatype, "\n", file = thefile)
	cat("ByteOrder=",  .Platform$endian, "\n", file = thefile)
	cat("nBands=",  raster@file@nbands, "\n", file = thefile)
	cat("BandOrder=",  raster@file@bandorder, "\n", file = thefile)
	cat("MinValue=",  raster@data@min, "\n", file = thefile)
	cat("MaxValue=",  raster@data@max, "\n", file = thefile)
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


write.import <- function(raster, outfile, overwrite=FALSE) {
# check extension
	rsout <- set.raster(raster, filename=outfile)
	for (r in 1:nrow(raster)) {
		d <- read.row(raster, r)
		write.row(rsout, overwrite)
		}
	return(rsout)
}

write.export <- function(raster, outfile, filetype='ascii', overwrite=FALSE) {
	rsout <- set.raster(raster, filename=outfile)
	if (filetype == 'ascii') {
		for (r in 1:nrow(raster)) {
			d <- read.row(raster, r)
			write.ascii(rsout, overwrite) 
		}
	} else {
		stop("filetype not yet supported (sorry..., more coming ...)")
	}
	return(rsout)
}
