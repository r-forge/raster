
exportGDAL <- function(raster, driver="HFA", filename, overwrite=FALSE, ForceIntOutput = FALSE) { 
	if (file.exists(filename) & !overwrite) {
		stop("filename exists; use overwrite=TRUE")
	}	

	# this needs to get fancier; depending on raster and the abilties of the drivers
	dataformat <- 'Int32'
	if (dataType(raster) == 'numeric' & !ForceIntOutput) {
		dataformat <- 'Float32'
	} 
	driver <- new("GDALDriver", driver) 
	export <- new("GDALTransientDataset", driver, nrow(raster), ncol(raster), 1, dataformat) 

	for (r in 1:nrow(raster)) {
		x <- putRasterData(export, values(readRow(raster, r)), 1, c((r-1), 0)) 
	}	

	saveDataset(export, filename) 
	GDAL.close(export) 
	GDAL.close(driver) 
}	

