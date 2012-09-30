# Author: Robert J. Hijmans
# Date :  September 2011
# Version 1.0
# Licence GPL v3

.writeBigMatrix <- function(x, filename, ... ) {

	stopifnot(require(bigmemory))
	
	filetype <- 'big.matrix'
	x@file@driver <- filetype
 	filename <- trim(filename)
	fnamevals <- .setFileExtensionValues(filename, filetype)
	fnamehdr <- .setFileExtensionHeader(filename, filetype)
	filename <- fnamevals
	x@file@name <- filename
	
	overwrite <- .overwrite(...)
	if ( ! overwrite & (file.exists(fnamehdr) | file.exists(fnamevals))) {
		stop(paste(filename,"exists. Use 'overwrite=TRUE' if you want to overwrite it"))
	}
	
#	x <- setMinMax(x)
	datatype <- .datatype(...)
	dataType(x) <- datatype
	if (.shortDataType(datatype) == 'INT') {
		dtype <- 'integer'
	} else {
		dtype <- 'double'
	}
	
	dscfile <- extension(basename(fnamevals), 'big.dsc')
	if (inherits(x, 'RasterLayer')) {
		b <- filebacked.big.matrix(nrow(x), ncol(x), type=dtype, backingfile=basename(fnamevals),
			backingpath=dirname(fnamevals), descriptorfile=dscfile)
		b[] <- as.matrix(x)	
	} else {
		b <- filebacked.big.matrix(ncell(x), nlayers(x), type=dtype, backingfile=basename(fnamevals),
			backingpath=dirname(fnamevals), descriptorfile=dscfile)
		b[] <- getValues(x)
	}
	
	
#	mn <- minValue(x)
#	mx <- maxValue(x)
#	dsize <- dataSize(x@file@datanotation)
	
	.writeHdrRaster(x, type='big.matrix')
	return(raster(filename))
}
 
 