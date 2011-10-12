
.onLoad <- function(lib, pkg)  {
	pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package=pkg), fields=c("Version","Date")))
	
	packageStartupMessage(paste(pkg, " version ", pkg.info["Version"], " (", pkg.info["Date"], ")", sep=""))
	tst <- try( removeTmpFiles(), silent=TRUE )

	library.dynam("raster", pkg, lib)	
	return(invisible(0))
}

