# Download geographic data and return as R object
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.9
# October 2008


.unzip <- function(zipf, files, remove=TRUE) {
	path <- dirname(zipf)
	if (path=='') { path <- getwd() }
	for (f in files) {
		zip.file.extract(file=f, zipname=zipf, dir=path)
	}
	if (remove) { file.remove(zipf) }
}


getData <- function(name='GADM', download=TRUE, path='', ...) {
	if (name=='GADM') {
		.GADM(download=download, path=path, ...)
	} else if (name=='SRTM') {
		.SRTM(download=download, path=path, ...)
	} else if (name=='alt') {
		.raster(name=name, download=download, path=path, ...)
	} else if (name=='worldclim') {
		.worldclim(download=download, path=path, ...)
	}
}


.getDataPath <- function(path) {
	path <- trim(path)
	if (path=='') {
		path <- .dataloc()
		if (is.null(path) | isTRUE(path=='')) {
			path <- getwd()
		}
	} else {
		if (substr(path, nchar(path)-1, nchar(path)) == '//' ) {
			p <- substr(path, 1, nchar(path)-2)		
		} else if (substr(path, nchar(path), nchar(path)) == '/'  | substr(path, nchar(path), nchar(path)) == '\\') {
			p <- substr(path, 1, nchar(path)-1)
		} else {
			p <- path
		}
		if (!file.exists(p)) {
			stop('path does not exist: ', p)
		}
	}
	if (substr(path, nchar(path), nchar(path)) != '/' & substr(path, nchar(path), nchar(path)) != '\\') {
		path <- paste(path, "/", sep="")
	}
	return(path)
}


.GADM <- function(country, level, download, path) {
	path <- .getDataPath(path)
#	if (!file.exists(path)) {  dir.create(path, recursive=T)  }

	if (missing(country)) {
		stop('provide a 3 letter ISO country code')
	}
	if (missing(level)) {
		stop('provide a "level=" argument; levels can be 0, 1, or 2 for most countries, and higer for some')
	}
	filename <- paste(path, country, '_adm', level, ".RData", sep="")
#	theurl <- paste("http://www.r-gis.org/rgis/data/adm/", country, '_adm', level, ".RData", sep="")
	if (!file.exists(filename)) {
		if (download) {
			theurl <- paste("http://gadm.org/data/rda/", country, '_adm', level, ".RData", sep="")
			download.file(url=theurl, destfile=filename, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
			if (!file.exists(filename))
				{ cat("\nCould not download file -- perhaps it does not exist \n") }
		} else {
			cat("\nFile not available locally. Use 'download = TRUE'\n")
		}
	}	
	if (file.exists(filename)) {
		thisenvir = new.env()
		data <- get(load(filename, thisenvir), thisenvir)
		return(data)
	} 
}

.worldclim <- function(res, var,  x=0, y=0, download=TRUE, path='') {
	stop('not yet implemented')
	if (!res %in% c(0.5, 2.5, 5, 10)) {
		stop('resolution should be one of: 0.5, 2.5, 5, 10')
	}
	if (!var %in% c('tmin', 'tmax', 'prec', 'bio')) {
		stop('var should be one of: tmin, tmax, prec, bio')
	}
	if (res > 0.5) {
	
	} else {
		# 30s tile
		
	}
}


.raster <- function(country, name, path, mask=TRUE, download=TRUE, ...) {
#	path <- .getDataPath(path)
	if (mask) {
		mskname <- '_msk_'
		mskpath <- 'msk_'
	} else {
		mskname<-'_'
		mskpath <- ''		
	}
	filename <- paste(path, country, mskname, name, ".grd", sep="")
#	theurl <- paste("http://www.r-gis.org/rgis/data/adm/", country, '_adm', level, ".RData", sep="")

	#http://diva-gis.org/data/msk_alt/MEX_msk_alt.zip
	if (!file.exists(filename)) {
		zipfilename <- filename
		ext(zipfilename) <- '.zip'
		if (!file.exists(zipfilename)) {
			if (download) {
				theurl <- paste("http://diva-gis.org/data/", mskpath, name, "/", country, mskname, name, ".zip", sep="")
				download.file(url=theurl, destfile=zipfilename, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
				if (!file.exists(zipfilename))	{ cat("\nCould not download file -- perhaps it does not exist \n") }
			} else {
				cat("\nFile not available locally. Use 'download = TRUE'\n")
			}
		}
		f <- filename
		ext(f) <- '.gri'
		f <- c(f, filename)
		.unzip(zipfilename, f)
	}	
	if (file.exists(filename)) { 
		rs <- raster(filename)
		projection(rs) <- "+proj=longlat +datum=WGS84"
		return(rs)
	}	
}


.SRTM <- function(x=106, y=-6, download=TRUE, path='') {
	path <- .getDataPath()
	
	x <- min(180, max(-180, x))
	y <- min(60, max(-60, y))
	rs <- raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
	row <- rowFromY(rs, y)
	col <- colFromX(rs, x)
	if (row < 10) { row <- paste('0', row, sep='') }
	if (col < 10) { col <- paste('0', col, sep='') }
	
	f <- paste('srtm_', col, '_', row, sep="")
	zipfilename <- paste(path, "/", f, ".ZIP", sep="")
	tiffilename <- paste(path, "/", f, ".TIF", sep="")
	
	if (!file.exists(tiffilename)) {
		if (!file.exists(zipfilename)) {
			if (download) { 
				theurl <- paste("http://hypersphere.telascience.org/elevation/cgiar_srtm_v4/tiff/zip/", f, ".ZIP", sep="")
				download.file(url=theurl, destfile=zipfilename, method="auto", quiet = FALSE, mode = 'wb', cacheOK = TRUE)
			} else {cat('file not available locally, use download=TRUE\n') }	
		}
		if (file.exists(zipfilename)) { 
			.unzip(zipfilename, tiffilename)
		}	
	}
	if (file.exists(tiffilename)) { 
		rs <- raster(tiffilename)
		projection(rs) <- "+proj=longlat +datum=WGS84"
		return(rs)
	}	
}

