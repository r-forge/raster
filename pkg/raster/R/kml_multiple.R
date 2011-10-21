# Derived from functions GE_SpatialGrid and kmlOverlay 
# in the maptools package by Duncan Golicher, David Forrest and Roger Bivand 
# Adaptation for the raster packcage by Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2011
# Version 0.9
# Licence GPL v3



setMethod('KML', signature(x='RasterStackBrick'), 

function (x, filename, time=NULL, col=rev(terrain.colors(255)), maxpixels=100000, zip='', ...) {

    if (! .couldBeLonLat(x)) { 
        stop("CRS of x must be longitude/latitude")
	}
	stopifnot(hasValues(x))
	showname <- FALSE
	if (missing(filename)) { 
		filename <- extension(basename(rasterTmpFile()), '.kml')
		showname <- TRUE
	}
	
	nl <- nlayers(x)
	if (is.null(time)) { 
		dotime <- FALSE
	} else {
		dotime <- TRUE
		if (length(time) != nl+1) {
			stop('length(time) should equaly nlayers(x)+1')
		}
	}

	x <- sampleRegular(x, size=maxpixels, asRaster = TRUE, useGDAL=TRUE)
	kmlfile <- filename
	extension(kmlfile) <- '.kml'
	
	name <- layerNames(x)
	begin <- time[-length(time)]
	end <- time[-1]

    kml <- c('<?xml version="1.0" encoding="UTF-8"?>', '<kml xmlns="http://www.opengis.net/kml/2.2">')
    kml <- c(kml, c("<Folder>", paste("<name>", extension(basename(filename), ''), "</name>", sep='')))
    e <- extent(x)
    latlonbox <- c("\t<LatLonBox>", paste("\t\t<north>", e@ymax, "</north><south>",  e@ymin, "</south><east>", 
						e@xmax, "</east><west>", e@xmin, "</west>", sep = ""), "\t</LatLonBox>", "</GroundOverlay>")

	imagefile <- paste(extension(filename, ''), "_", 1:nl, ".png", sep="")
	for (i in 1:nl) {
		png(filename = imagefile[i], width=max(480, ncol(x)), height=max(480, nrow(x)), bg="transparent")
		par(mar=c(0,0,0,0))
		if (R.Version()$minor >= 13) {
			image(x[[i]], col=col, axes=FALSE, useRaster=TRUE)
		} else {
			image(x[[i]], col=col, axes=FALSE)
		}
		dev.off()
		a <- c("<GroundOverlay>", paste("\t<name>", name[i], "</name>", sep=''))
		if (dotime) {
			time <- c(a, "\t<TimeSpan>", 
					paste("\t\t<begin>", begin[i], "</begin>", sep=''), 
					paste("\t\t<end>", end[i], "</end>", sep=''), "\t</TimeSpan>")
		}
		kml <- c(kml, a, time, paste("\t<Icon><href>", basename(imagefile[i]), "</href></Icon>", sep=''), latlonbox)
	}
	
    kml <- c(kml, "</Folder>", "</kml>")
	
    cat(paste(kml, sep = "", collapse = "\n"), file = kmlfile, sep = "")
	
	if (showname) {
		cat('kml file created: ', kmlfile, '\n')
	}

	if (zip == "") {
		zip <- Sys.getenv('R_ZIPCMD', 'zip')
	}
	
	if (zip!= "") {
		wd <- getwd()
		on.exit( setwd(wd) )
		setwd(dirname(kmlfile))
		kmlfile <- basename(kmlfile)
		kmzfile <- extension(kmlfile, '.kmz')
		imagefile <- basename(imagefile)
		if (zip=='7z') {
			kmzzip <- extension(kmzfile, '.zip')
			cmd <- paste(zip, 'a',  kmzzip, kmlfile, imagefile, collapse=" ")
			file.rename(kmzzip, kmzfile)
		} else {
			cmd <- paste(c(zip, kmzfile, kmlfile, imagefile), collapse=" ")
		}
		sss <- try( system(cmd, intern=TRUE) , silent = TRUE )
		if (file.exists(kmzfile)) {
			x <- file.remove(kmlfile, imagefile)
		}
	} 

	
}
)

