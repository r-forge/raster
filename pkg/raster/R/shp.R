# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.1
# Licence GPL v3


if (!isGeneric("shapefile")) {
	setGeneric("shapefile", function(x, ...)
		standardGeneric("shapefile"))
}	


setMethod('shapefile', signature(x='character'), 
	function(x, stringsAsFactors=FALSE, verbose=FALSE, ...) {
		.requireRgdal() 
		stopifnot(file.exists(extension(x, '.shp')))
		stopifnot(file.exists(extension(x, '.shx')))
		stopifnot(file.exists(extension(x, '.dbf')))
		fn <- extension(basename(x), '')
		readOGR(dirname(x), fn, stringsAsFactors=stringsAsFactors, verbose=verbose, ...) 		
	}
)


setMethod('shapefile', signature(x='Spatial'), 
	function(x, filename='', overwrite=FALSE, ...) {
		.requireRgdal() 
		stopifnot(filename != '')
		extension(filename) <- '.shp'
		if (file.exists(filename)) {
			if (!overwrite) {
				stop('file exists, use overwrite=TRUE to overwrite it')
			}
		}
		layer <- basename(filename)
		extension(layer) <- ''
		if (!inherits(object, 'Spatial')) {
			stop('To write a shapefile you need to provide an object of class Spatial*')
		} else {
			if (inherits(object, 'SpatialPixels')) {
				if (.hasSlot(object, 'data')) {
					object <- as(object, 'SpatialPointsDataFrame')
				} else {
					object <- as(object, 'SpatialPoints')				
				}
			} else if (inherits(object, 'SpatialGrid')| inherits(object, 'SpatialPixels')) {
				stop('These data cannot be written to a shapefile')
			}
			
			if (!.hasSlot(object, 'data')) {
				if (inherits(object, 'SpatialPolygons')) {
					object <- SpatialPolygonsDataFrame(object, data.frame(ID=1:length(row.names(object))))
				} else if (inherits(object, 'SpatialLines')) {
					object <- SpatialLinesDataFrame(object, data.frame(ID=1:length(row.names(object))))
				} else if (inherits(object, 'SpatialPoints')) {
					object <- SpatialPointsDataFrame(object, data.frame(ID=1:length(row.names(object))))
				} else {
					stop('These data cannot be written to a shapefile')
				}
			}
		}
		writeOGR(object, filename, layer, driver='ESRI Shapefile', verbose=verbose, overwrite_layer=overwrite, ...)
	}
)


