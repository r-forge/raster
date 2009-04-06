 
writeHeader <- function(raster, type) {
	type <- toupper(type)
	if (type=="BIL") {
		.writeBilHdr(raster)
	} else if (type=="ERDASRAW") {
		.writeErdasRawHdr(raster)
	} else 	if (type=="ENVI") {
		.writeENVIHdr(raster)
	} else 	if (type=="RASTER") {
		.writeRasterHdr(raster)
	} else {
		stop("This format is not supported")
	}
	.writeStx(raster)
 }
 
 
.writeStx <- function(raster) {
	if (raster@data@haveminmax) {
		stxfile <- filename(raster)
		ext(stxfile) <- ".stx"
		thefile <- file(stxfile, "w")  # open an txt file connectionis
		cat(1, " ", minValue(raster), " ", maxValue(raster), "\n", file = thefile)
		close(thefile)
	}	
}
 
 
.writeBilHdr <- function(raster) {
	hdrfile <- filename(raster)
	ext(hdrfile) <- ".hdr"
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("NROWS ",  nrow(raster), "\n", file = thefile)
	cat("NCOLS ",  ncol(raster), "\n", file = thefile)
	cat("NBANDS ",  nbands(raster), "\n", file = thefile)
	cat("NBITS ",  dataSize(raster@file@datanotation) * 8, "\n", file = thefile)
	if (.Platform$endian == "little") { btorder <- "I" 
	} else { btorder <- "M" }
	cat("BYTEORDER ", btorder, "\n", file = thefile)
	
#  PIXELTYPE should work for Gdal, and perhpas ArcGIS, see:
# http://lists.osgeo.org/pipermail/gdal-dev/2006-October/010416.html	

	dtype <- .shortDataType(raster@file@datanotation)
	if (dtype == 'INT' | dtype == 'LOG' ) { 
		if (dataSigned(raster@file@datanotation)) {
			pixtype <- "SIGNEDINT"
		} else {
			pixtype <- "INT"
		}
	} else { 
		pixtype <- "FLOAT" 
	}
	cat("PIXELTYPE ", pixtype, "\n", file = thefile)	
	cat("LAYOUT ", "BIL", "\n", file = thefile)
    cat("SKIPBYTES 0\n", file = thefile)
    cat("ULXMAP", xmin(raster) + 0.5 * xres(raster), "\n", file = thefile) 
    cat("ULYMAP", ymax(raster) - 0.5 * yres(raster), "\n", file = thefile) 
	cat("XDIM", xres(raster), "\n", file = thefile)
	cat("YDIM", yres(raster), "\n", file = thefile)
	browbytes <- round(ncol(raster) * dataSize(raster@file@datanotation) )
	cat("BANDROWBYTES ", browbytes, "\n", file = thefile)
	cat("TOTALROWBYTES ", browbytes *  nbands(raster), "\n", file = thefile)
	cat("BANDGAPBYTES  0", "\n", file = thefile)
    cat("NODATA", .nodatavalue(raster), "\n", file = thefile)	

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
	hdrfile <- filename(raster)
	ext(hdrfile) <- ".raw"
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("IMAGINE_RAW_FILE\n", file = thefile)
	cat("PIXEL_FILES ", .setFileExtensionValues(filename(raster)), "\n", file = thefile)
# this may not work. Some implementations may ignore this keyword and expect the pixelfile to have the same file name, no extension.		

	cat("HEIGHT ",  nrow(raster), "\n", file = thefile)
	cat("WIDTH ",  ncol(raster), "\n", file = thefile)
	cat("NUM_LAYERS ",  nbands(raster), "\n", file = thefile)

	if (.shortDataType(raster@file@datanotation) == 'INT') { 
		dd <- "S"
	} else { 
		dd <- "F" 
	}
	nbits <- dataSize(raster@file@datanotation) * 8 
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
	
	worldFile(raster, ".rww")	
 }
 

worldFile <- function(raster, extension=".wld") {
	hdrfile <- filename(raster)
	ext(hdrfile) <- extension
	thefile <- file(hdrfile, "w")  
	cat(xres(raster), "\n", file = thefile)
	cat("0\n", file = thefile)
	cat("0\n", file = thefile)
	cat(-1 * yres(raster), "\n", file = thefile)
    cat(xmin(raster) + 0.5 * xres(raster), "\n", file = thefile) 
    cat(ymax(raster) - 0.5 * yres(raster), "\n", file = thefile) 
	close(thefile)	
}

 
 

.writeENVIHdr <- function(raster) {
	hdrfile <- filename(raster)
	ext(hdrfile) <- ".hdr"
	thefile <- file(hdrfile, "w") 
	cat("ENVI\n", file = thefile)
	cat("description = {", raster@file@shortname, "}", "\n", file = thefile)
	cat("samples = ", ncol(raster), "\n", file = thefile)		
	cat("lines = ", nrow(raster), "\n", file = thefile)		
	cat("bands = ", raster@file@nbands, "\n", file = thefile)		
	cat("header offset = 0\n", file = thefile)		
	cat("file type = ENVI Standard\n", file = thefile)		
	dsize <- dataSize(raster@file@datanotation)
	if (.shortDataType(raster@file@datanotation) == 'INT') {
		if (dsize == 1) { dtype <- 1
		} else if (dsize == 2) { dtype <- 2
		} else if (dsize == 4) { dtype <- 3
		} else if (dsize == 8) { dtype <- 14
		} else { stop('what?')
		}
	} else {
		if (dsize == 4) { dtype <- 4
		} else if (dsize == 8) { dtype <- 5
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

