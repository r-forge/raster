# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.8
# Licence GPL v3



if (!isGeneric("CDF")) {
	setGeneric("CDF", function(x, y, ...)
		standardGeneric("CDF"))
}	


setMethod('CDF', signature(x='RasterLayer', y='character'), 
	function(x, y){ 
	    overwrite <- TRUE

		if (!require(RNetCDF)) { stop() }	
		xvar <- (xmin(x) + 1:ncol(x) * xres(x)) - 0.5 * xres(x)
		yvar <- (ymin(x) + 1:nrow(x) * yres(x)) - 0.5 * yres(x)
		if (dataContent(x) != 'all') {
			x <- readAll(x)
		}
		zvar <- values(x)
		if (class(zvar) == 'numeric') {
			dtype <- 'NC_DOUBLE'
		} else {
			dtype <- 'NC_INT' 
		}
		zvar <- t(matrix(zvar, ncol=ncol(x), nrow=nrow(x)))
		zvar <- t(zvar[nrow(zvar):1, ])
		nc <- create.nc(y, clobber=overwrite)
		dim.def.nc(nc, "x", ncol(x))
		dim.def.nc(nc, "y", nrow(x))
		
		var.def.nc(nc, "x", 'NC_DOUBLE', 0)
		var.def.nc(nc, "y", 'NC_DOUBLE', 1)
		var.def.nc(nc, "z", 'NC_DOUBLE', c(1,0))
		
		var.put.nc(nc, "x", xvar)
		var.put.nc(nc, "y", yvar)
		var.put.nc(nc, "z", zvar)

		att.put.nc(nc, "z", "missing_value", "NC_DOUBLE", NAvalue(x))
 	    att.put.nc(nc, "z", "long_name", "NC_CHAR", layerNames(x))
		
		att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Data from the R raster package")
		att.put.nc(nc, "NC_GLOBAL", "history", "NC_CHAR", paste("Created on", date()))

		close.nc(nc)
	}
)

 #CDF(r, 'c:/test.nc')
 #y <- raster('c:/test.nc')
 #plot(y)


setMethod('CDF', signature(x='RasterStack', y='character'), 
	function(x, y){ 
		if (!require(RNetCDF)) { stop() }	
	
	}
)


