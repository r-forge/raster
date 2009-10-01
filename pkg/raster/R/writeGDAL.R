# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

# based on  create2GDAL and saveDataset from the rgdal package
# authors: Timothy H. Keitt, Roger Bivand, Edzer Pebesma, Barry Rowlingson


.getGDALtransient <- function(raster, gdalfiletype, overwrite, mvFlag,  options)  {

	.isSupportedFormat(gdalfiletype)
	
# this is a RasterLayer hence nbands = 1:
# but we keep this for later (RasterStack / Brick)
    nbands = nlayers(raster)

	
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
	
    transient = new("GDALTransientDataset", driver=driver, rows=nrow(raster), cols=ncol(raster), bands=nbands, type=dataformat, options=gdoptions, handle=NULL)
 
	gt <- c(xmin(raster), xres(raster), 0, ymax(raster), 0, -yres(raster))
    .Call("RGDAL_SetGeoTransform", transient, gt, PACKAGE = "rgdal")
    p4s <- projection(raster)
    .Call("RGDAL_SetProject", transient, p4s, PACKAGE = "rgdal")
	
	return(transient)
}


# ALTERNATIVE; not used
#.spWriteGDALall <- function(raster, gdalfiletype, overwrite, mvFlag, options) {
#	spgrid <- asSpGrid(raster)	
#	writeGDAL(spgrid, filename(raster))
#}


.writeGDALall <- function(raster, gdalfiletype, overwrite, mvFlag, options) {
	if (!require(rgdal)) { stop() }

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

	raster@file@driver <- 'gdal'
	raster@data@source <- 'disk'
	
	return(raster)
}

