# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2010
# Version 1.0
# Licence GPL v3

 
.vrt <- function(x) {
	if (inherits(x), 'RasterStack') { stop() }
	if (!fromDisk(x)) { stop() }
	if (x@file@driver != 'raster') { stop() }
	fn <- fname <- x@file@name
	ext(fname) <- 'vrt'
	ext(fn) <- '.gri'	
	pixsize <- dataSize(dtype)
	lineoff <- pixsize * x@ncols

	datatype <- raster:::.getGdalDType(dataType(x))	
	
	if (x@file@byteorder == "little") { 
		byteorder <- "LSB" 
	} else { 
		byteorder <- "MSB" 
	}

	f <- file(fname, "w") 
	cat('<VRTDataset rasterXSize="', x@ncols, '" rasterYSize="', x@nrows, '">\n' , sep = "", file = f)
	cat('<VRTRasterBand dataType="', datatype, '" band="1" subClass="VRTRawRasterBand">\n', sep = "" , file = f)
	cat('<SourceFilename relativetoVRT="1">', basename(fn), '</SourceFilename>\n', sep = "", file = f)
	cat('<ImageOffset>0</ImageOffset>\n', sep = "", file = f)
	cat('<PixelOffset>', pixsize, '</PixelOffset>\n', sep = "", file = f)
	cat('<LineOffset>', lineoff, '</LineOffset>\n', sep = "", file = f)
	cat('<ByteOrder>', byteorder, '</ByteOrder>\n', sep = "", file = f)
	cat('</VRTRasterBand>\n', sep = "", file = f)
	cat('</VRTDataset>\n', sep = "", file = f)
	close(f)
	return( invisible(TRUE) )
}  


