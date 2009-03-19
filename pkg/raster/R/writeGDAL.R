# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3

# based on  create2GDAL and saveDataset from the rgdal package
# authors: Timothy H. Keitt, Roger Bivand, Edzer Pebesma, Barry Rowlingson


.getGDALtransient <- function(raster, gdalfiletype, overwrite, mvFlag,  options)  {
#	.isSupportedFormat(gdalfiletype)
	
# this is a RasterLayer hence nbands = 1:
    nbands = nlayers(raster)
# but we keep this for later (RatserStack)

	filename(raster) <- trim(raster@file@name)
	if (filename(raster) == "") {	
		stop('first provide a filename. E.g.: filename(raster) <- "c:/myfile"')	
	}

	if (file.exists( filename(raster) )) {
		if (!overwrite) {
			stop("filename exists; use overwrite=TRUE")
		} else if (!file.remove( filename(raster) )) {
			stop("cannot delete existing file. permissin denied.")
		}
	}	

# this needs to get fancier; depending on object and the abilties of the drivers
	dataformat <- .getGdalDType(raster@file@datanotation)
	driver = new("GDALDriver", gdalfiletype)
	
    gdoptions <- NULL
	
#	GDALDataTypes <- c('Unknown', 'Byte', 'UInt16', 'Int16', 'UInt32','Int32', 'Float32', 'Float64', 'CInt16', 'CInt32',   'CFloat32', 'CFloat64')	
#	typeNum <- match(gdalfiletype, GDALDataTypes, 1) - 1
# 	my_tempfile <- tempfile()
#	gdhandle <- .Call('RGDAL_CreateDataset', driver=driver, as.integer(c(ncol(raster), nrow(raster), nlayers(raster))), as.integer(typeNum), options=NULL, my_tempfile, PACKAGE="rgdal")
#   transient = new("GDALTransientDataset", driver=driver, rows=nrow(raster), cols=ncol(raster), bands=nbands, type=dataformat, options=gdoptions, handle=gdhandle)

    transient = new("GDALTransientDataset", driver=driver, rows=nrow(raster), cols=ncol(raster), bands=nbands, type=dataformat, options=gdoptions, handle=NULL)
 
	gt <- c(xmin(raster), xres(raster), 0, ymax(raster), 0, -yres(raster))
    .Call("RGDAL_SetGeoTransform", transient, gt, PACKAGE = "rgdal")
    p4s <- projection(raster)
    .Call("RGDAL_SetProject", transient, p4s, PACKAGE = "rgdal")
	
	return(transient)
}


.writeGDALrow <- function(raster, gdalfiletype, overwrite, mvFlag, options ) {
	
	rownr <- rowFromCell(raster, dataIndices(raster)[1])
#	if (rownr %in%  c(1, 10, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 20000, 30000, 40000, 50000, 100000)) {
#		print( paste("writing row", rownr, "at:", format(Sys.time(), "%H:%M:%S")))
#	}
	if ( rownr == 1) {
		transient <- .getGDALtransient(raster, gdalfiletype, overwrite, mvFlag, options)
		attr(raster@file, "transient") <- transient
		
#		raster@file@driver <- 'gdal'
		raster@data@source <- 'disk'		
	}	
    for (band in 1:nlayers(raster)) {
		x <- putRasterData(raster@file@transient, values(raster, rownr), band, c((rownr-1), 0)) 
	}
	if ( rownr == nrow(raster)) {
		saveDataset(raster@file@transient, filename(raster) )
		GDAL.close(raster@file@transient) 
		
		# establish the handle:
		rasterout <- raster(filename(raster))
		
		rasterout@data@haveminmax <- raster@data@haveminmax
		rasterout@data@min <- raster@data@min
		rasterout@data@max <- raster@data@max
		.writeStx(rasterout) 
		return(rasterout)
	}
	return(raster)
}


# ALTERNATIVE
#.writeGDALall <- function(raster, gdalfiletype, overwrite, asInt, mvFlag, options) {
#	spgrid <- asSpGrid(raster)	
#	writeGDAL(spgrid, filename(raster))
#}

.writeGDALall <- function(raster, gdalfiletype, overwrite, mvFlag, options) {
	
	transient <- .getGDALtransient(raster, gdalfiletype, overwrite, mvFlag, options)
    for (band in 1:nlayers(raster)) {
		x <- putRasterData(transient, t(values(raster, format='matrix')), band, c(0, 0)) 
	}	
#        if (!is.na(mvFlag)) {
#            transient_b <- getRasterBand(dataset = transient, band = band)
#            .Call("RGDAL_SetNoDataValue", transient_b, as.double(mvFlag), PACKAGE = "rgdal")
#       }
	saveDataset(transient, filename(raster) )
	GDAL.close(transient) 
	.writeStx(raster) 

	tempras <- raster(filename(raster) )
#	raster@file@driver <- 'gdal'
	attr(raster@file, "con") <- tempras@file@con
	raster@data@source <- 'disk'
	return(raster)
}

