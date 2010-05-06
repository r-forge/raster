# Authors: Robert J. Hijmans and Jacob van Etten
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("crop")) {
	setGeneric("crop", function(x, y, ...)
		standardGeneric("crop"))
}	


setMethod('crop', signature(x='RasterLayer', y='ANY'), 
function(x, y, filename='', datatype=dataType(x), ...) {
	filename <- trim(filename)

	y <- try ( extent(y), silent=TRUE )
	if (class(y) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

# we could also allow the raster to expand but for now let's not and first make a separate expand function
	e <- intersectExtent(x, y)
	e <- alignExtent(e, x)
	outRaster <- raster(x)
	outRaster <- setExtent(outRaster, e, keepres=TRUE)
	col1 <- colFromX(x, xmin(outRaster)+0.5*xres(outRaster))
	col2 <- colFromX(x, xmax(outRaster)-0.5*xres(outRaster))
	row1 <- rowFromY(x, ymax(outRaster)-0.5*yres(outRaster))
	row2 <- rowFromY(x, ymin(outRaster)+0.5*yres(outRaster))
	
	datatype=dataType(x)

	if (dataContent(x) == 'all')  {
		x <- values(x, format='matrix')[(row1:row2), (col1:col2)]
		outRaster <- setValues(outRaster, as.vector(t(x)))
		if (filename != "") { 
			outRaster <- writeRaster(outRaster, filename=filename, datatype=datatype, ...)
		}

	} else if ( dataSource(x) == 'disk') { 
		nc <- ncol(outRaster)
		nr <- row2 - row1 + 1
		if (canProcessInMemory(outRaster, 3)) {
			v <- values(.readRasterLayerValues(x, row1, nrows=nr, startcol=col1, ncols=nc))
			outRaster <- setValues(outRaster, as.vector(v) )
			if (filename != '') { 
				outRaster <- writeRaster(outRaster, filename=filename, datatype=datatype, ...) 
			}
			return(outRaster)
		} else if ( filename == '') {
			filename <- rasterTmpFile()
									
		}
		
		tr <- blockSize(outRaster)
		pb <- pbCreate(tr$n, type=.progress(...))
		outRaster <- writeStart(outRaster, filename=filename, datatype=datatype, ... )
		ncols <- col2-col1+1
		tr$row <- tr$row+row1-1
		for (i in 1:tr$n) {
			vv <- getValuesBlock(x, row=tr$row[i], nrows=tr$size, col1, ncols)
			outRaster <- writeValues(outRaster, vv)
			pbStep(pb, r) 			
		} 
		outRaster <- writeStop(outRaster)
		pbClose(pb)


		
#		pb <- pbCreate(nrow(outRaster), type=.progress(...))
#		outRaster <- writeStart(outRaster, filename=filename, datatype=datatype, ... )
#		for (r in row1:row2) {
#			vv <- getValues(x, r)[col1:col2]
#			outRaster <- writeValues(outRaster, vv)
#			pbStep(pb, r) 			
#		} 
#		pbClose(pb)
		
	}
	return(outRaster)
}
)

