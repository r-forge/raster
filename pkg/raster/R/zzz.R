
.onLoad <- function(lib, pkg)  {
	
	pkg.info <- utils::packageDescription('raster') 
	packageStartupMessage(paste("raster ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))

	tst <- try( removeTmpFiles( .tmptime() ), silent=TRUE ) 

# for testing purposes	

#	rasterOptions(format='GTiff')
#	rasterOptions(format='big.matrix')
#	rasterOptions(format='CDF')
#	rasterOptions(overwrite=TRUE)
#	rasterOptions(todisk=TRUE)
	
	return(invisible(0))
}

