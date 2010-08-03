# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.9
# Licence GPL v3


#.getxvar <- function(xvar, vars) {
#	if (xvar == '') {
#		if ('x' %in% vars) { xvar <- 'x'
#		} else if ('lon' %in% vars) { xvar <- 'lon' 
#		} else if ('long' %in% vars) { xvar <- 'long' 
#		} else if ('longitude' %in% vars) { xvar <- 'longitude' 
#		} else if ('Longitude' %in% vars) { xvar <- 'Longitude' 
#		} else { stop('Cannot find an obvious xvar in file. Select one from:\n', paste(vars, collapse=", "))  
#		}
#	} else if ( !(xvar %in% vars) ) { stop( paste(xvar, ' does not exist in the file. Select one from:\n', paste(vars, collapse=", "))) }	
#	return(xvar)
#}


#.getyvar <- function(yvar, vars) {
#	if (yvar == '') { 
#		if ('y' %in% vars){ yvar <- 'y'
#		} else if ('lat' %in% vars) { yvar <- 'lat' 
#		} else if ('latitude' %in% vars) { yvar <- 'latitude' 
#		} else if ('Latitude' %in% vars) { yvar <- 'Latitude' 
#		} else { stop('Cannot find an obvious yvar in file. Select one from:\n', paste(vars, collapse=", "))  
#		}
#	} else if (!(yvar %in% vars)) { stop( paste(yvar, ' does not exist in the file. Select one from:\n', paste(vars, collapse=", "))) }	
#	return(yvar)
#}



.dimNames <- function(nc) {
	n <- nc$dim
	nams <- vector(length=n)
	if (n > 0) {
		for (i in 1:n) {
			nams[i] <- nc$dim[[i]]$name
		}
	}
	return(nams)
}


.varIDX <- function(nc, varname='') {
	n <- nc$nvars
	vars <- vector(length=n)
	if (n > 0) {
		for (i in 1:n) {
			vars[i] <- nc$var[[i]]$name
		}
	}

	if (varname=='') { 
		nv <- length(vars)
		if (nv == 0) {
			stop()
		} 
		
		if (nv  == 1) {
			varname <- vars
		} else {
			# should also check its dimensions with those of x and y 
			a=NULL
			for (i in 1:nv) { 
				a = c(a, nc$var[[i]]$ndims) 
			}
			varname <- vars[which.max(a)]
			warning('varname used is: ', varname, '\nIf that is not correct, set it to one of: ', paste(vars, collapse=", ") )
		}
	}

	vix <- which(varname == vars)
	if (length(vix) == 0) {
		stop('varname: ', varname, ' does not exist in the file. Select one from:\n', paste(vars, collapse=", ") )
	}
	return(vix)
}


.rasterObjectFromCDF <- function(filename, x='', y='', varname='', band=NA, type='RasterLayer', ...) {

	if (!require(ncdf)) { stop('You need to install the ncdf package first') }
	nc <- open.ncdf(filename)
	on.exit( close.ncdf(nc) )
	
	conv <- 'CF'
	
	
	conv <- att.get.ncdf(nc, 0,  "Conventions")
	if (substr(conv$value, 1, 3) == 'RST') {
		close.ncdf(nc)
		return( .rasterObjectFromCDFrst(filename, band=band, type='RasterLayer', ...) )
	} else {
		# assuming "CF-1.0"
	}
	
	vix <- .varIDX(nc, varname)
	zvar <- nc$var[[vix]]$name
	
	datatype <- .getRasterDTypeFromCDF( nc$var[[vix]]$prec )
	
	
	dims <- nc$var[[vix]]$ndims
	if (dims== 1) { 
		stop('"varname" only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims > 3) { 
		stop('"varname" has ', length(dims), ' dimensions, I do not know how to process this')
	}
	
	ncols <- nc$var[[vix]]$dim[[1]]$len
	nrows <- nc$var[[vix]]$dim[[2]]$len

	xx <- nc$var[[vix]]$dim[[1]]$vals
	rs <- xx[-length(xx)] - xx[-1]
	
	if (! isTRUE ( all.equal( min(rs), max(rs), scale= min(rs)/100 ) ) ) {
		stop('cells are not equally spaced; perhaps consider using these data as points') 
	}
	
	xrange <- c(min(xx), max(xx))
	resx <- (xrange[2] - xrange[1]) / (ncols-1)
	rm(xx)

	yy <- nc$var[[vix]]$dim[[2]]$vals
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
 
	add_offset <- 0
	scale_factor <- 1
	long_name <- zvar
	missing_value <- NA
	projection <- NA
	a <- att.get.ncdf(nc, vix, "add_offset")
	if (a$hasatt) { add_offset <- a$value }
	a <- att.get.ncdf(nc, vix, "scale_factor")
	if (a$hasatt) { scale_factor <- a$value }
	a <- att.get.ncdf(nc, vix, "long_name")
	if (a$hasatt) { long_name <- a$value }
	a <- att.get.ncdf(nc, vix, "missing_value")
	if (a$hasatt) { missing_value <- a$value }
	a <- att.get.ncdf(nc, vix, "projection")
	if (a$hasatt ) { projection  <- a$value }
	
	prj = list()
	if (!is.na(projection)) {
	# TO DO
#		att <- var.inq.nc(nc, projection)
#		if (att$natts > 0) {
#			for (i in 0:(att$natts-1)) {
#				prj[[i+1]] <- att.get.nc(nc, projection, i)
#				names(prj)[i+1] <- att.inq.nc(nc, projection, i)$name
#			}
		# now what?
		# projection(r) <- ...
#		} 
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

#	attr(r@data, "xvar") <- xvar
#	attr(r@data, "yvar") <- yvar
	attr(r@data, "zvar") <- zvar
	attr(r@data, "add_offset") <- add_offset
	attr(r@data, "scale_factor") <- scale_factor
	
	attr(r, "prj") <- prj 
	r@file@driver <- "netcdf"	
	if (! is.na(missing_value)) {
		r@file@nodatavalue <- missing_value
	}
	r@data@fromdisk <- TRUE
	
	if (dims == 2) {
		nbands = 1
	} else {
		r@file@nbands <- nc$var[[vix]]$dim[[3]]$len
	}

	if (type == 'RasterLayer') {
		if (is.na(band) | is.null(band)) {
			if (dims == 3) { 
				stop(zvar, 'has three dimensions, provide a "band" value between 1 and ', dims[3])
			} 
		} else {
			if (length(band) > 1) {
				stop('A RasterLayer can only have a single band. You can use a RasterBrick instead')
			}		
			if (is.na(band)) {
				r@data@band <- as.integer(1)
			} else {
				r@data@band <- as.integer( min(max(1, band), r@file@nbands) )
			}
		} 

	} else {
		if (length(dims)== 2) { 
			stop('cannot make a RasterStack or Brick from a data that has only two dimensions (no time step), use raster() instead, and then make a stack or brick from that')	
		} 
		r@data@nlayers <- r@file@nbands
	}
	return(r)
}

#f = "G:/cmip/ipcc/20c3m/atm/mo/pr/bccr_bcm2_0/run1/pr_A1_1.nc"
#p = .rasterObjectFromCDF(f, zvar='pr', type='RasterLayer', time=10)

