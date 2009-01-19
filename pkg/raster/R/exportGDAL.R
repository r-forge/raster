# R function for the raster package
# Author: Robert J. Hijmans
# International Rice Research Institute. Philippines
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.8
# Licence GPL v3


# based on  create2GDAL and saveDataset from the rgdal package
# authors: Timothy H. Keitt, Roger Bivand, Edzer Pebesma, Barry Rowlingson

.isSupportedGDALdriver <- function(dname) {
	gdrivers <- c("ADRG", "BMP", "BT", "EHdr", "ELAS", "ENVI", "ERS", "GSBG", "GTiff", "HFA", "IDA", "ILWIS", "INGR", "Leveller", "MEM", "MFF", "MFF2", "NITF", "PAux", "PCIDSK", "PNM", "RMF", "RST", "SGI", "Terragen", "VRT")
	res <- dname %in% gdrivers
	if (!res) { stop(paste(dname,"is not a supported GDAL file format. Choose from: \n    ADRG, BMP, BT, EHdr, ELAS, ENVI, ERS, GSBG, GTiff, HFA, IDA, ILWIS,\n    INGR, Leveller, MEM, MFF, MFF2, NITF, PAux, PCIDSK, PNM, RMF, RST, SGI, Terragen, VRT" ) ) }
	return(res)
}


.getGDALtransient <- function(raster, filename, gdalfiletype, mvFlag, options, overwrite, ForceIntOutput)  {
	.isSupportedGDALdriver(gdalfiletype)
	
# this is a RasterLayer hence nbands = 1:
    nbands = nlayers(raster)
# but we keep this for later (stack, brick)

	if (file.exists(filename)) {
		if (!overwrite) {
			stop("filename exists; use overwrite=TRUE")
		} else if (!file.remove(filename)) {
			stop("cannot delete file. permissin denied.")
		}
	}	

#.GDALDataTypes <- c('Unknown', 'Byte', 'UInt16', 'Int16', 'UInt32','Int32', 'Float32', 'Float64', 'CInt16', 'CInt32',   'CFloat32', 'CFloat64')	
# this needs to get fancier; depending on object and the abilties of the drivers
	if (dataType(raster) == 'integer' | ForceIntOutput) {
		dataformat <- 'Int32'
		if (raster@data@haveminmax) {
			if (minValue(raster) > -32768 & maxValue(raster) <= 32767) {
				dataformat <- 'Int16'
			}
		}
	} else { dataformat <- 'Float32' }

	driver = new("GDALDriver", gdalfiletype)
	
    if (!is.null(options) && !is.character(options)) { stop("options not character") }
    transient = new("GDALTransientDataset", driver = driver, rows = nrow(raster), cols = ncol(raster), bands = nbands, type = dataformat, options = options, handle = NULL)
 
	gt <- c(xmin(raster), xres(raster), 0, ymax(raster), 0, -yres(raster))
    .Call("RGDAL_SetGeoTransform", transient, gt, PACKAGE = "rgdal")
    p4s <- projection(raster)
    .Call("RGDAL_SetProject", transient, p4s, PACKAGE = "rgdal")
	
	return(transient)
}



exportGDAL <- function(raster, filename, gdalfiletype = "GTiff", overwrite=FALSE, ForceIntOutput = FALSE ) {
	mvFlag = NA
	options = NULL
	nbands = nlayers(raster)
	
	transient <- .getGDALtransient(raster, filename, gdalfiletype, mvFlag, options, overwrite, ForceIntOutput)
	
    for (band in 1:nbands) {

		if (dataContent(raster)=='all') {
#			if (!is.na(mvFlag)) vals[is.na(vals)] = mvFlag
# This would work, but could potentially lead to memory problems (making a copy of the values before writing)
#			x <- putRasterData(transient, t(values(raster, format='matrix')), band, c(0, 0)) 
			for (r in 1:nrow(raster)) {
				x <- putRasterData(transient, valuesRow(raster, r), band, c((r-1), 0)) 
			}
		} else {
			if (dataSource(raster)=='ram') {
				stop("No data on disk, and not all values in memory. Cannot write the file")
			}
			for (r in 1:nrow(raster)) {
				x <- putRasterData(transient, values(readRow(raster, r)), band, c((r-1), 0)) 
			}
		}

#        if (!is.na(mvFlag)) {
#            transient_b <- getRasterBand(dataset = transient, band = band)
#            .Call("RGDAL_SetNoDataValue", transient_b, as.double(mvFlag), PACKAGE = "rgdal")
#       }
    }
    saveDataset(transient, filename)
	GDAL.close(transient) 
	
#  do NOT do this, it removes the driver for future use!!! ????	
#	GDAL.close(driverobj) 

	outras <- rasterFromFile(filename)
	outras@data@min <- raster@data@min
	outras@data@max <- raster@data@max
	if (!is.na((outras@data@min))) { outras@data@haveminmax <- TRUE }
	
	return(outras)
}


writeGDALrow <- function(raster, filename, gdalfiletype = "GTiff", rownr, overwrite=FALSE, ForceIntOutput = FALSE ) {
	if (rownr == 1) {
		mvFlag = NA
		options = NULL
		transient <- .getGDALtransient(raster, filename, gdalfiletype, mvFlag, options, overwrite, ForceIntOutput)
		attr(raster, "transient") <- transient
	}	
	
    for (band in 1:nlayers(raster)) {
		x <- putRasterData(raster@transient, values(raster, rownr), band, c((rownr-1), 0)) 
	}	

#        if (!is.na(mvFlag)) {
#            transient_b <- getRasterBand(dataset = transient, band = band)
#            .Call("RGDAL_SetNoDataValue", transient_b, as.double(mvFlag), PACKAGE = "rgdal")
#       }
    
	if (rownr == nrow(raster)) {
		saveDataset(raster@transient, filename)
		GDAL.close(raster@transient) 
#  do NOT do this, it removes the driver for future use!!! ????	
#	GDAL.close(driverobj) 
		outras <- rasterFromFile(filename)
		outras@data@min <- raster@data@min
		outras@data@max <- raster@data@max
		if (!is.na((outras@data@min))) { outras@data@haveminmax <- TRUE }
		return(outras)
	} else {
		return(raster)
	}
}

