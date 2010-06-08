# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.9
# Licence GPL v3


.isNetCDF <- function(x) {
	on.exit(options('warn'= getOption('warn')))
	options('warn'=-1) 
	fcon <- file(x, "rb")
	tst <- try( w <- readBin(fcon, what='character', n=1), silent=TRUE)
	close(fcon)
	if ( isTRUE((substr(w, 1, 3) == "CDF" ))) { return(TRUE) 
	} else { return(FALSE)
	}
}


.getxvar <- function(xvar, vars) {
	if (xvar == '') {
		if ('x' %in% vars) { xvar <- 'x'
		} else if ('lon' %in% vars) { xvar <- 'lon' 
		} else if ('long' %in% vars) { xvar <- 'long' 
		} else if ('longitude' %in% vars) { xvar <- 'longitude' 
		} else { stop('Cannot find an obvious xvar in file. Select one from:\n', paste(vars, collapse=", "))  
		}
	} else if (!(xvar %in% vars)) { stop( paste('Cannot find "xvar" in file. Select one from:\n', paste(vars, collapse=", "))) }	
	return(xvar)
}

.getyvar <- function(yvar, vars) {
	if (yvar == '') { if ('y' %in% vars){ yvar <- 'y'
		} else if ('lat' %in% vars) { yvar <- 'lat' 
		} else if ('latitude' %in% vars) { yvar <- 'latitude' 
		} else { stop('Cannot find an obvious yvar in file. Select one from:\n', paste(vars, collapse=", "))  
		}
	} else if (!(yvar %in% vars)) { stop( paste('Cannot find "yvar" in file. Select one from:\n', paste(vars, collapse=", "))) }	
	return(yvar)
}

.getzvar <- function(zvar, vars) {
	if (zvar == '') { zvar <- 'z' }
	if (!(zvar %in% vars)) { stop ( 'Cannot find an obvious "zvar" in file. Select one from:\n', paste(vars, collapse=", ") ) }
	return(zvar)
}



.rasterObjectFromCDF <- function(filename, type='RasterLayer', xvar='', yvar='', zvar='', time=NA, ...) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package first') }

	nc <- open.nc(filename)

	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
	zvar <- .getzvar(zvar, vars) 

	varinfo <- try(var.inq.nc(nc, zvar))
	dims <- varinfo$ndims
	if (dims== 1) { 
		stop('zvar only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims > 3) { 
		stop('zvar has ', length(dims), ' dimensions, I do not know how to process these data')
	}
	
	xvar <- .getxvar(xvar, vars) 
	yvar <- .getyvar(yvar, vars) 

	# to do: also consider "lat_bnds", "lat_bnds and time_bnds"
	
	
	ncols <- dim.inq.nc(nc, xvar)$length
	nrows <- dim.inq.nc(nc, yvar)$length

	xx <- as.vector(var.get.nc(nc, xvar))
	rs <- xx[-length(xx)] - xx[-1]
	if (! isTRUE ( all.equal( min(rs), max(rs), scale= min(rs)/100 ) ) ) {
		stop('cells are not equally spaced; extract as points') }
	xrange <- c(min(xx), max(xx))
	resx <- (xrange[2] - xrange[1]) / (ncols-1)
	rm(xx)

	yy <- as.vector(var.get.nc(nc, yvar))
	rs <- yy[-length(yy)] - yy[-1]
	if (! isTRUE ( all.equal( min(rs), max(rs), scale= min(rs)/100 ) ) ) {
		stop('cells are not equally spaced; you should extract values as points') }
	yrange <- c(min(yy), max(yy))
	resy <- (yrange[2] - yrange[1]) / (nrows-1)

	if (yy[1] > yy[length(yy)]) { toptobottom  <- FALSE
	} else { toptobottom <- TRUE }

	rm(yy)

	xrange[1] <- xrange[1] - 0.5 * resx
	xrange[2] <- xrange[2] + 0.5 * resx
	yrange[1] <- yrange[1] - 0.5 * resy
	yrange[2] <- yrange[2] + 0.5 * resy
 
	att <- var.inq.nc(nc, variable=zvar)
	add_offset <- 0
	scale_factor <- 1
	long_name <- zvar
	missing_value <- NA
	projection <- NA
	if (att$natts > 0) {
		for (i in 0:(att$natts-1)) {
			if (att.inq.nc(nc, zvar, i)$name == "add_offset") {
				add_offset <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "scale_factor") {
				scale_factor <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "long_name") {
				long_name <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "missing_value") {
				missing_value <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "grid_mapping") {
				projection <- att.get.nc(nc, zvar, i)
			}
		}
	}
	
	prj = list()
	if (!is.na(projection)) {
		att <- var.inq.nc(nc, 'projection')
		if (att$natts > 0) {
			for (i in 0:(att$natts-1)) {
				prj[[i+1]] <- att.get.nc(nc, projection, i)
				names(prj)[i+1] <- att.inq.nc(nc, projection, i)$name
			}
		# now what?
		# projection(r) <- ...
		} 
	}
	
	if (type == 'RasterLayer') {
		r <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)
	} else {
		r <- brick(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)
	}
	
	if (xrange[1] < -181 | xrange[2] > 181 | yrange[1] < -91 | yrange[2] > 91) {
		projection(r) <- NA
	}
	r@file@name <- filename
	r@file@toptobottom <- toptobottom
	r <- .enforceGoodLayerNames(r, long_name)

	attr(r@data, "xvar") <- xvar
	attr(r@data, "yvar") <- yvar
	attr(r@data, "zvar") <- zvar
	attr(r@data, "add_offset") <- add_offset
	attr(r@data, "scale_factor") <- scale_factor
	
	attr(r, "prj") <- prj 
	r@file@driver <- "netcdf"	
	r@file@nodatavalue <- missing_value
	r@data@source <- 'disk'
	r@file@nbands <- as.integer(dim.inq.nc(nc, var.inq.nc(nc, zvar)$dimids[3])$length)
#			dim.inq.nc(x, 'time')$length

	if (type == 'RasterLayer') {
		if (is.na(time) | is.null(time)) {
			if (length(dims)== 3) { 
				stop('zvar has three dimensions, provide a value for "time", between 1 and ', dims[3])
			} 
		} else {
			if (length(time) > 1) {
				stop('A RasterLayer can only have a single time step. You can use a RasterBrick instead')
			}		
			r@data@band <- as.integer(time)
		} 

	} else {
		if (length(dims)== 2) { 
			stop('cannot make a RasterStack or Brick from a data that has only two dimensions (no time step), use raster() instead, and then make a stack or brick from that')	
		} 
		r@data@nlayers <- r@file@nbands
	}
	close.nc(nc)
	return(r)
}

#f = "G:/cmip/ipcc/20c3m/atm/mo/pr/bccr_bcm2_0/run1/pr_A1_1.nc"
#p = .rasterObjectFromCDF(f, zvar='pr', type='RasterLayer', time=10)

