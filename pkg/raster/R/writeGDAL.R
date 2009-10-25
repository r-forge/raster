# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

# based on  create2GDAL and saveDataset from the rgdal package
# authors: Timothy H. Keitt, Roger Bivand, Edzer Pebesma, Barry Rowlingson


.getGDALtransient <- function(raster, filename, options, ...)  {
	datatype <- .datatype(...)
	overwrite <- .overwrite(...)
	gdalfiletype <- .filetype(...)

	.isSupportedFormat(gdalfiletype)
	
	if (filename == "") {	
		stop('provide a filename')	
	}

	if (file.exists( filename))  {
		if (!overwrite) {
			stop("filename exists; use overwrite=TRUE")
		} else if (!file.remove( filename)) {
			stop("cannot delete existing file. permission denied.")
		}
	}	
# this needs to get fancier; depending on object and the abilties of the drivers
	dataformat <- .getGdalDType(datatype)

    nbands = nlayers(raster)
	if (gdalfiletype=='GTiff') {
		bytes <- ncell(raster) * dataSize(datatype) * nbands
		if (bytes > (4 * 1024 * 1024 * 1000) ) {  # ~ 4GB
			options <- c(options, 'BIGTIFF=YES')
		}
	}
	driver = new("GDALDriver", gdalfiletype)
    transient = new("GDALTransientDataset", driver=driver, rows=nrow(raster), cols=ncol(raster), bands=nbands, type=dataformat, options=options, handle=NULL)
 
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


.writeGDALall <- function(raster, filename, options=NULL, ...) {
	if (!require(rgdal)) { stop() }

	mvFlag <- NA
	transient <- .getGDALtransient(raster, filename=filename, mvFlag=mvFlag, options=options, ...)
	nl <- nlayers(raster)
	if (nl == 1) {
		x <- putRasterData(transient, t(values(raster, format='matrix')), band=1, c(0, 0)) 
	} else {
	    for (i in 1:nl) {
			v <- matrix(values(raster)[,i], nrow=nrow(raster), ncol=ncol(raster), byrow=TRUE)
			x <- putRasterData(transient, t(v), band=i, c(0, 0)) 
		}
	}	
	saveDataset(transient, filename )
	GDAL.close(transient) 
	.writeStx(raster) 

	if (nl==1) {
		raster <- raster(filename)
	} else {
		raster <- brick(filename)
	}
	raster <- readAll(raster)
	return(raster)
}

