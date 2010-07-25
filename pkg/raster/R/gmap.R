# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  July 2010
# Version 1.0
# Licence GPL v3

# Based on functions in R pacakge 'RgoogleMaps' 
# by Markus Loecher, Sense Networks <markus at sensenetworks.com>


gmap <- function (x , exp=1, maptype='terrain', filename='', key, ...) {

    size = c(640, 640)

	mxzoom <- function (latrange, lonrange, size = c(640, 640)) {
		SinPhi = sin(latrange * pi/180)
		normX = lonrange/180
		normY = (0.5 * log(abs((1 + SinPhi)/(1 - SinPhi))))/pi
		MaxZoom.lon <- floor(1 + log2(abs(size[1]/256/diff(normX))))
		MaxZoom.lat <- floor(1 + log2(abs(size[2]/256/diff(normY))))
		return(c(MaxZoom.lat = MaxZoom.lat, MaxZoom.lon = MaxZoom.lon))
	}

	ll2XY <- function (lat, lon, zoom) {
		SinPhi = sin(lat * pi/180)
		normX = lon/180
		normY = (0.5 * log((1 + SinPhi)/(1 - SinPhi)))/pi
		Y = (2^zoom) * ((1 - normY)/2)
		X = (2^zoom) * ((normX + 1)/2)
		x = 256 * (X - floor(X))
		y = 256 * (Y - floor(Y))
		return(list(Tile = cbind(X = floor(X), Y = floor(Y)), Coords = cbind(x = x,  y = y)))
	}

	xy2ll <- function (MyMap, X, Y) {
		lat.center <- MyMap[[1]]
		lon.center <- MyMap[[2]]
		zoom <- MyMap[[3]]
		mycenter <- ll2XY(lat.center, lon.center, zoom)
		x <- mycenter$Tile[, "X"] + (X + mycenter$Coords[, "x"])/256
		y <- mycenter$Tile[, "Y"] - (Y - mycenter$Coords[, "y"])/256
		ytilde <- 1 - y/2^(zoom - 1)
		yy = (exp(2 * pi * ytilde) - 1)/(exp(2 * pi * ytilde) + 1)
		ShiftLat <- function(yy) {
			n = c(-1, 0, 1)
			lat = 2 * pi * (n) + asin(yy)
			lat <- lat[which(lat <= pi/2 & lat > -pi/2)]
			lat <- 180 * lat/pi
			return(lat)
		}
		lat <- sapply(yy, ShiftLat)
		lon = 180 * (x/2^(zoom - 1) - 1)
		return(cbind(lat = lat, lon = lon))
	}

	tile2r <- function (points, center) {
		X <- 256 * (points$Tile[, "X"] - center$Tile[, "X"]) + (points$Coords[, "x"] - center$Coords[, "x"])
		Y <- -256 * (points$Tile[, "Y"] - center$Tile[, "Y"]) - (points$Coords[, "y"] - center$Coords[, "y"])
		return(list(X = X, Y = Y))
	}

	if (inherits(x, 'Raster')) {
		# check if long/lat, if not project
	}
	x <- extent(x)
	
	e <- x * exp
	e@xmin <- max(-180, e@xmin)
	e@xmax <- min(180, e@xmax)
	e@ymax <- min(89, e@ymax)
	e@ymin <- max(-89, e@ymin)
	
    lonR <- c(e@xmin, e@xmax)
	latR <- c(e@ymin, e@ymax)

    zoom <- min(mxzoom(latR, lonR, size))
	center = c(mean(latR), mean(lonR))
 	
	
    ll <- ll2XY(latR[1], lonR[1], zoom)
    ur <- ll2XY(latR[2], lonR[2], zoom)
    cr <- ll2XY(center[1], center[2], zoom)
    ll.Rcoords <- tile2r(ll, cr)
    ur.Rcoords <- tile2r(ur, cr)
    size[1] <- 2 * max(c(ceiling(abs(ll.Rcoords$X)), ceiling(abs(ur.Rcoords$X)))) +   1
    size[2] <- 2 * max(c(ceiling(abs(ll.Rcoords$Y)), ceiling(abs(ur.Rcoords$Y)))) +   1

    if (length(size) < 2) {
        s <- paste(size, size, sep = "x")
    } else {
        s <- paste(size, collapse = "x")
    }


	if (missing(key)) {
		KeyFile <- paste(Sys.getenv("HOME"), "/API.key.txt", sep = "")
		if (file.exists(KeyFile)) {
			key <- scan(KeyFile, what = "")
		} else {
			key <- "ABQIAAAAx_Zq0CG7Dz9YNSzDR0PYtxT2yXp_ZAY8_ufC3CFXhHIE1NvwkxQ9M3z-hbUeB-0ItTVP2WPiFXA8PA"
		}
	}


	if (trim(filename) == '') filename <- rasterTmpFile()
	ext(filename) <- 'gif'
	ctr <- paste(center, collapse = ",")
	
	gurl <- "http://maps.google.com/staticmap?"
    gurl <- paste(gurl, "center=", ctr, "&zoom=", zoom, "&size=", s, "&maptype=", maptype, "&format=gif", "&key=", key, "&sensor=false", sep = "")
	download.file(gurl, filename, mode = "wb", quiet = TRUE)
   
	MyMap <- list(lat.center = center[1], lon.center = center[2], zoom = zoom)
	bb <- list(ll = xy2ll(MyMap, X = -size[1]/2 + 0.5, Y = -size[2]/2 - 0.5), ur = xy2ll(MyMap, X = size[1]/2 +  0.5, Y = size[2]/2 - 0.5))

	r <- raster(filename)
	
	ext <- extent(bb$ll[2], bb$ur[2], bb$ll[1], bb$ur[1])
	p <- t(bbox(raster(ext))) *  pi/180
	rad = 6378137	
    y <- log(tan(p[, 2]) + (1/cos(p[, 2]))) * rad
    x <- p[, 1] * rad
    xy <- cbind(x, y)
	extent(r) <- extent(as.vector(xy))
	projection(r) = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
    return(r)
}

#e = c( -121.9531 , -120.3897 , 35.36 , 36.61956 )
#r = gmap(e)
#plotCT(r)

#projmerc <- function(p) {
#    p <- p * pi / 180
#    p[, 2] <- log(tan(p[, 2]) + (1/cos(p[, 2])))
#    return( p * 6378137 )
#}
