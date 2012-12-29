
.onLoad <- function(lib, pkg)  {
	
	pkg.info <- utils::packageDescription('raster') 
	packageStartupMessage(paste("raster ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))

	tst <- try( removeTmpFiles( .tmptime() ), silent=TRUE ) 
	wd <- getwd()
	options('startup.working.directory'=wd)
	fn <- paste(wd, '/rasterOptions_', pkg.info[["Version"]], sep='')
	.loadOptions(fn)
	return(invisible(0))
}

