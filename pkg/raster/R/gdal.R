

.gd_SetNoDataValue <- function(object, NAflag) {
	.Call("RGDAL_SetNoDataValue", object, as.double(NAflag), PACKAGE="rgdal")
}


.gd_SetGeoTransform <- function(object, geotrans) {
   .Call("RGDAL_SetGeoTransform", object, geotrans, PACKAGE="rgdal")
}


.gd_SetProject <- function(object, proj4string) {
    .Call("RGDAL_SetProject", object, proj4string, PACKAGE="rgdal")
}


.gd_SetStatistics <- function(object, statistics) {
	.Call("RGDAL_SetStatistics", object, as.double(statistics), PACKAGE="rgdal")
}

.gd_transform <- function(projfrom, projto, n, x, y) {
	.Call("transform", projfrom, projto, n, x, y, PACKAGE="rgdal")
}


# gdalinfo: colortables, blocksizes