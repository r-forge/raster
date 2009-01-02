
import <- function(raster, filename="", overwrite=FALSE) {
# check extension
	if (raster@file@driver <- "raster") {
		stop("file is a raster format file, it cannot be imported")
	}	
	if (trim(filename) == "") { 
		filename <- setFileExtension(filename(raster), ".grd")
	}
	rsout <- setRaster(raster, filename=filename)
	for (r in 1:nrow(raster)) {
		d <- readRow(raster, r)
		setValuesRow(rsout, d, r)
		writeValues(rsout, overwrite=overwrite)
	}
	clearValues(rsout)
	return(rsout)
}


export <- function(raster, filename="", filetype="ascii", overwrite=FALSE) {
	if (trim(filename) == "") { 
		filename <- filename(raster) 
	}
	if (filetype == "ascii") {
		raster <- setFilename(raster,filename=filename)
		for (r in 1:nrow(raster)) {
			writeAscii(raster, overwrite=overwrite) 
		}
	} else if (filetype == "bil") {
		grdToBil(raster, filename=filename, keepGRD=TRUE, overwrite=overwrite) 
	} else {
		stop("filetype not yet supported (sorry..., more coming ...)")
	}
}


grdToBil <- function(raster, filename="", keepGRD=TRUE, overwrite=FALSE) {
	sourcefile <- setFileExtension(filename(raster), ".gri")
	if (keepGRD) {
		targetfile <- setFileExtension(filename(raster), ".bil")
		if (file.exists(targetfile)) { 
			if (!(overwrite)) { 
				stop(paste("File exists:", targetfile, " Use overwrite=TRUE, if you want to overwrite it"))
			}
		}	
		file.copy(from=sourcefile, to=targetfile, overwrite=overwrite)
	} else {
		targetfile <- setFileExtension(filename(raster), ".bil")
		if (file.exists(targetfile)) { 
			if (overwrite) { 
				file.remove(targetfile) 
			} else {
				stop(paste("File exists:", targetfile, " Use overwrite=TRUE, if you want to overwrite it"))
			}
		}	
		file.rename(from=sourcefile, to=targetfile)
	}
	.writeBilHdr(raster)
	if (!(keepGRD)) { file.remove(filename(raster)) }
	return(rasterFromFile(targetfile))
}



writeAscii <- function(raster, filename, overwrite=FALSE) {
	if (raster@data@indices[1] == 1) {
		resdif <- abs((yres(raster) - xres(raster)) / yres(raster) )
		if (resdif > 0.01) {
			stop(paste("raster has unequal horizontal and vertical resolutions","\n", "these data cannot be stored in arc-ascii format"))
		}
		if (!overwrite & file.exists(filename)) {
				stop(paste(filename, "exists. Use 'overwrite=TRUE'")) 
		}

		thefile <- file(filename, "w")  # open an txt file connection
		cat("NCOLS", ncol(raster), "\n", file = thefile)
		cat("NROWS", nrow(raster), "\n", file = thefile)
		cat("XLLCORNER", xmin(raster), "\n", file = thefile)
		cat("YLLCORNER", ymin(raster), "\n", file = thefile)
		cat("CELLSIZE",  xres(raster), "\n", file = thefile)
		cat("NODATA_value", raster@file@nodatavalue, "\n", file = thefile)
		close(thefile) #close connection
		
    } else if ( dataIndices(raster)[2] > ncells(raster)) {
		stop(paste('writing beyond end of file. last cell:', raster@data@indices[2], '>', ncells(raster)))
	}

	raster@data@values[is.na(values(raster))] <- raster@file@nodatavalue 
	if (dataContent(raster) == 'all') {
		for (r in 1:nrow(raster)) {
			write.table(t(valuesRow(raster, r)), filename, append = TRUE, quote = FALSE, 
								sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
		}					
	} else {
		write.table(t(values(raster)), filename, append = TRUE, quote = FALSE, 
							sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
    }
	
	if ( dataIndices(raster)[2] == ncells(raster)) {
		return(rasterFromFile(filename))
	} else {
		return("writing in progress")
	}	
}
 
 

writeOtherHeader <- function(raster, format="BIL") {
	if (format=="BIL") {
		.writeBilHdr(raster)
	} else if (format=="ErdasRaw") {
		.writeErdasRawHdr(raster)
	} else 	if (format=="ENVI") {
		.writeENVIHdr(raster)
	} else 	if (format=="worldfile") {
		.writeWorldfile(raster)
	} else {
		stop("This format is not supported")
	}
 }
 
.writeBilHdr <- function(raster) {
	hdrfile <- setFileExtension(filename(raster), ".hdr")
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("NROWS ",  nrow(raster), "\n", file = thefile)
	cat("NCOLS ",  ncol(raster), "\n", file = thefile)
	cat("NBANDS ",  raster@file@nbands, "\n", file = thefile)
	cat("NBITS ",  raster@file@datasize * 8, "\n", file = thefile)
	if (.Platform$endian == "little") { btorder <- "I" 
	} else { btorder <- "M" }
	cat("BYTEORDER ", btorder, "\n", file = thefile)
	
#  PIXELTYPE should work for Gdal, and perhpas ArcGIS, see:
# http://lists.osgeo.org/pipermail/gdal-dev/2006-October/010416.html	
	if (raster@file@datatype == 'integer') { pixtype <- "SIGNEDINT"
	} else { pixtype <- "FLOAT" }
	cat("PIXELTYPE ", pixtype, "\n", file = thefile)	
	cat("LAYOUT ", "BIL", "\n", file = thefile)
    cat("SKIPBYTES 0\n", file = thefile)
    cat("ULXMAP", xmin(raster) + 0.5 * xres(raster), "\n", file = thefile) 
    cat("ULYMAP", ymax(raster) - 0.5 * yres(raster), "\n", file = thefile) 
	cat("XDIM", xres(raster), "\n", file = thefile)
	cat("YDIM", yres(raster), "\n", file = thefile)
	browbytes <- round(ncol(raster) * raster@file@datasize)
	cat("BANDROWBYTES ", browbytes, "\n", file = thefile)
	cat("TOTALROWBYTES ", browbytes *  raster@file@nbands, "\n", file = thefile)
	cat("BANDGAPBYTES  0", "\n", file = thefile)
    cat("NODATA", raster@file@nodatavalue, "\n", file = thefile)	

	cat("\n\n", file = thefile)
	cat("The below is additional metadata, not part of the BIL/HDR format\n", file = thefile)
	cat("----------------------------------------------------------------\n", file = thefile)
	cat("CREATOR=R package:raster\n", file = thefile)
	cat("CREATED=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)
	cat("Projection=", projection(raster), "\n", file = thefile)
	cat("MinValue=",  minValue(raster), "\n", file = thefile)
	cat("MaxValue=",  maxValue(raster), "\n", file = thefile)

	close(thefile)
}


.writeErdasRawHdr <- function(raster) {
	hdrfile <- setFileExtension(filename(raster), ".raw")
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("IMAGINE_RAW_FILE\n", file = thefile)
	cat("PIXEL_FILES ", setFileExtension(filename(raster), ".gri"), "\n", file = thefile)
# this may not work. Some implementations may ignore this keyword and expect the pixelfile to have the same file name, no extension.		

	cat("HEIGHT ",  nrow(raster), "\n", file = thefile)
	cat("WIDTH ",  ncol(raster), "\n", file = thefile)
	cat("NUM_LAYERS ",  raster@file@nbands, "\n", file = thefile)

	if (raster@file@datatype == 'integer') { dd <- "S"
	} else { dd <- "F" }
	nbits <- raster@file@datasize * 8 
    dtype <- paste(dd, nbits, sep="")
	cat("DATA_TYPE ",  dtype, "\n", file = thefile)
#U1, U2, U4, U8, U16, U32
#S16, S32
#F32, and F64.
	if (.Platform$endian == "little") { btorder <- "LSB" 
	} else { btorder <- "MSB" }
	cat("BYTE_ORDER ", btorder, "\n", file = thefile)
#Required for DATA_TYPE values of U16, S16, U32, S32

	cat("FORMAT ", "BIL", "\n", file = thefile)
	cat("DATA_OFFSET 0\n", file = thefile)
	cat("END_RAW_FILE\n", file = thefile)
	
	cat("\n\n", file = thefile)
	cat("The below is additional metadata, not part of the BIL/HDR format\n", file = thefile)
	cat("----------------------------------------------------------------\n", file = thefile)
	cat("CREATOR=R package:raster\n", file = thefile)
	cat("CREATED=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)
	cat("Projection=", projection(raster), "\n", file = thefile)
	cat("MinValue=",  minValue(raster), "\n", file = thefile)
	cat("MaxValue=",  maxValue(raster), "\n", file = thefile)
	close(thefile)	
 }
 
.writeWorldfile <- function(raster, extension=".world") {
	hdrfile <- setFileExtension(filename(raster), ".world")
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat(xres(raster), "\n", file = thefile)
	cat("0\n", file = thefile)
	cat("0\n", file = thefile)
	cat(-1 * yres(raster), "\n", file = thefile)
    cat(xmin(raster) + 0.5 * xres(raster), "\n", file = thefile) 
    cat(ymax(raster) - 0.5 * yres(raster), "\n", file = thefile) 
	close(thefile)	
}

 
 

.writeENVIHdr <- function(raster) {
	hdrfile <- setFileExtension(filename(raster), ".hdr")
	thefile <- file(hdrfile, "w") 
	cat("ENVI\n", file = thefile)
	cat("description = {", raster@file@shortname, "}", "\n", file = thefile)
	cat("samples = ", ncol(raster), "\n", file = thefile)		
	cat("lines = ", nrow(raster), "\n", file = thefile)		
	cat("bands = ", raster@file@nbands, "\n", file = thefile)		
	cat("header offset = 0\n", file = thefile)		
	cat("file type = ENVI Standard\n", file = thefile)		
	if (raster@file@datatype == 'integer') {
		if (raster@file@datasize == 1) { dtype <- 1
		} else if (raster@file@datasize == 2) { dtype <- 2
		} else if (raster@file@datasize == 4) { dtype <- 3
		} else if (raster@file@datasize == 8) { dtype <- 14
		} else { stop('what?')
		}
	} else {
		if (raster@file@datasize == 4) { dtype <- 4
		} else if (raster@file@datasize == 8) { dtype <- 5
		} else { stop('what?')
		}
	}	
	cat("data type = ", dtype, "\n", file = thefile)
#1=8-bit byte; 2=16-bit signed integer; 3=32-bit signed long integer; 4=32-bit floating point; 
#5=64-bit double-precision floating point; 6=2x32-bit complex, real-imaginary pair of double precision;
#9=2x64-bit double-precision complex, real-imaginary pair of double precision; 12=16-bit unsigned integer; 
#13=32-bit unsigned long integer; 14=64-bit signed long integer; and 15=64-bit unsigned long integer.

	cat("interleave = bil\n", file = thefile)	
	cat("sensor type = \n", file = thefile)		
	if (.Platform$endian == "little") { btorder <- 0 
	} else { btorder <- 1 }
	cat("byte order = ", btorder, "\n",file = thefile)		

	cat("map info = {projection, 1, 1,", xmin(raster),", ", ymax(raster),", ", xres(raster),", ", yres(raster), "}\n", file = thefile)
    cat("projection info =", projection(raster), "\n", file = thefile) 
	cat("z plot range = {", minValue(raster),", ", maxValue(raster), "}\n", file = thefile) 
	close(thefile)	
}

