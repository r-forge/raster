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


.getGDALtransient <- function(raster, gdalfiletype, overwrite, ForceIntOutput, mvFlag,  options)  {
	.isSupportedGDALdriver(gdalfiletype)
	
# this is a RasterLayer hence nbands = 1:
    nbands = nlayers(raster)
# but we keep this for later (stack, brick)

	raster <- setFilename(raster, trim(filename(raster)))
	if (filename(raster) == "") {	stop('first provide a filename. E.g.: raster <- setFilename(raster, "c:/myfile")')	}

	if (file.exists( filename(raster) )) {
		if (!overwrite) {
			stop("filename exists; use overwrite=TRUE")
		} else if (!file.remove( filename(raster) )) {
			stop("cannot delete existing file. permissin denied.")
		}
	}	

#.GDALDataTypes <- c('Unknown', 'Byte', 'UInt16', 'Int16', 'UInt32','Int32', 'Float32', 'Float64', 'CInt16', 'CInt32',   'CFloat32', 'CFloat64')	
# this needs to get fancier; depending on object and the abilties of the drivers
	if (dataType(raster) == 'integer' | ForceIntOutput) {
		dataformat <- 'Int32'
		if (raster@data@haveminmax) {
			if (minValue(raster) > -32768 & maxValue(raster) <= 32767) {
				dataformat <- 'Int16'
			} # also check for the need for INT64
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
	
	outras <- setRaster(raster, filename)
	transient <- .getGDALtransient(raster, gdalfiletype, overwrite, ForceIntOutput, mvFlag, options)
	
    for (band in 1:nbands) {
		if (dataContent(raster)=='all') {
#			if (!is.na(mvFlag)) vals[is.na(vals)] = mvFlag
# This would work, but could potentially lead to memory problems (making a copy of the values before writing)
#			x <- putRasterData(transient, t(values(raster, format='matrix')), band, c(0, 0)) 
			if (ForceIntOutput) {
				for (r in 1:nrow(raster)) {
					x <- putRasterData(transient, as.integer(round(valuesRow(raster, r))), band, c((r-1), 0)) 
				}
			} else {
				for (r in 1:nrow(raster)) {
					x <- putRasterData(transient, valuesRow(raster, r), band, c((r-1), 0)) 
				}
			}
		} else {
			if (dataSource(raster)=='ram') {
				stop("No data on disk, and not all values in memory. Cannot write the file")
			}
			if (ForceIntOutput) {
				for (r in 1:nrow(raster)) {
					x <- putRasterData(transient, as.integer(round(values(readRow(raster, r)))), band, c((r-1), 0)) 
				}
			} else {
				for (r in 1:nrow(raster)) {
					x <- putRasterData(transient, values(readRow(raster, r)), band, c((r-1), 0)) 
				}
			}
		}

#        if (!is.na(mvFlag)) {
#            transient_b <- getRasterBand(dataset = transient, band = band)
#            .Call("RGDAL_SetNoDataValue", transient_b, as.double(mvFlag), PACKAGE = "rgdal")
#       }
    }
    saveDataset(transient, outras@filename)
	GDAL.close(transient) 
	
#  do NOT do this, it removes the driver for future use!!! ????	
#	GDAL.close(driverobj) 
	outras@data@min <- raster@data@min
	outras@data@max <- raster@data@max
	if (!is.na((outras@data@min))) { outras@data@haveminmax <- TRUE }
	return(outras)
}






.writeGDALrow <- function(raster, gdalfiletype, overwrite, ForceIntOutput, mvFlag, options ) {
	
	rownr <- rowFromCell(raster, dataIndices(raster)[1])
	if ( rownr == 1) {
		transient <- .getGDALtransient(raster, gdalfiletype, overwrite, ForceIntOutput, mvFlag, options)
		attr(raster, "transient") <- transient
		raster@data@min <- 3e34
		raster@data@max <- -3e34
		raster@data@haveminmax <- FALSE
		raster@file@driver <- 'gdal'
		raster@file@gdalhandle <- list()
	}	
    for (band in 1:nlayers(raster)) {
		x <- putRasterData(raster@transient, values(raster, rownr), band, c((rownr-1), 0)) 
		
		rsd <- na.omit(raster@data@values) # min and max values; perhaps not worth it, as they won't be saved to file
		if (length(rsd) > 0) {
			raster@data@min <- min(raster@data@min, min(rsd))
			raster@data@max <- max(raster@data@max, max(rsd))
		}	
	}	
#        if (!is.na(mvFlag)) {
#            transient_b <- getRasterBand(dataset = transient, band = band)
#            .Call("RGDAL_SetNoDataValue", transient_b, as.double(mvFlag), PACKAGE = "rgdal")
#       }
	if ( rownr == nrow(raster)) {
		saveDataset(raster@transient, filename(raster) )
		GDAL.close(raster@transient) 
		raster <- rasterFromFile(filename(raster))

		raster@data@haveminmax <- TRUE
		raster@data@source <- 'disk'
		raster@data@content <- 'nodata'
		raster@data@values <- vector(length=0)
	}
	return(raster)
}


.writeGDALall <- function(raster, gdalfiletype, overwrite, ForceIntOutput, mvFlag, options) {
	
	transient <- .getGDALtransient(raster, gdalfiletype, overwrite, ForceIntOutput, mvFlag, options)
    for (band in 1:nlayers(raster)) {
		x <- putRasterData(transient, t(values(raster, format='matrix')), band, c(0, 0)) 
	}	
#        if (!is.na(mvFlag)) {
#            transient_b <- getRasterBand(dataset = transient, band = band)
#            .Call("RGDAL_SetNoDataValue", transient_b, as.double(mvFlag), PACKAGE = "rgdal")
#       }
	saveDataset(transient, filename(raster) )
	GDAL.close(transient) 
	tempras <- rasterFromFile(filename(raster) )
	raster@file@gdalhandle <- tempras@file@gdalhandle
	return(raster)
}

