#Author/copyright: Bjarke Christensen	
#License: ?

	
.split.SpatialPointsDataFrame <- function(x, f, drop=F) {
    stopifnot(class(x)=="SpatialPointsDataFrame")
    stopifnot(length(f) == nrow(x))
    if (!is.factor(f)) f <- factor(f)
    res <- lapply(levels(f), function(l) x[f==l, , drop=drop])
    names(res) <- levels(f)
    res
}

.split.SpatialPolygonsDataFrame <- function(x, f, drop=F) {
    stopifnot(class(x)=="SpatialPolygonsDataFrame")
    stopifnot(length(f) == nrow(x))
    if (!is.factor(f)) f <- factor(f)
    res <- lapply(levels(f), function(l) x[f==l, , drop=drop])
    names(res) <- levels(f)
    res
}

.split.SpatialLinesDataFrame <- function(x, f, drop=F) {
    stopifnot(class(x)=="SpatialLinesDataFrame")
    stopifnot(length(f) == nrow(x))
    if (!is.factor(f)) f <- factor(f)
    res <- lapply(levels(f), function(l) x[f==l, , drop=drop])
    names(res) <- levels(f)
    res
}



.KMLstyle <- function(tree, id, href="http://maps.google.com/mapfiles/kml/shapes/placemark_square.png", scale="0.5", color="FF0000FF") {
      tree$addNode("Style", attrs=c(id=id), close=F)
      tree$addNode("IconStyle", close=F)
      tree$addNode("color", color)
      tree$addNode("scale", scale)
      tree$addNode("Icon", close=F)
      tree$addNode("href", href)
      tree$closeTag() #Icon
      tree$closeTag() #IconStyle
      tree$addNode("PolyStyle", close=F)
      tree$addNode("color", color)
      tree$addNode("colorMode", "normal")
      tree$addNode("fill", 1)
      tree$addNode("outline", 1)
      tree$closeTag() #PolyStyle
      tree$closeTag() #Style
      return(tree)
    }

	
.KMLpolygons <- function(tree, object, style, foldername, altitude="0", ...) {
      dataframe <- ("data" %in% slotNames(object))
      colnm <- switch(dataframe,
                        T = paste("<b>", names(slot(object, 'data')), ":</b>", sep=""),
                        F = NULL)
      polys <- lapply(slot(object, 'polygons'), function(x) lapply(slot(x, 'Polygons'), function(y) slot(y, 'coords')) )
      if (length(altitude)==length(polys)) {
        alt <- altitude
      } else {
        alt <- rep(altitude, length.out=length(polys))
      }
      print(alt)
      for (ctr in 1:length(polys))
        polys[[ctr]] <- lapply(polys[[ctr]],
                                      function(y) paste(apply(
                                                  y,
                                                  1,
                                                  function(z) paste(c(z, alt[[ctr]]), collapse=',')
                                                  ), collapse=" \n")
                                      )

      tree$addNode("Folder", close=F)
        tree$addNode("name", foldername)
        tree$addNode("open", 0)
        for (i in 1:length(polys)) {
          tree$addNode("Placemark", close=F)
            if (dataframe) {
              tree$addNode("description", close=F)
                tree$addCData(paste(colnm, format(slot(object, 'data')[i,], scientific=F), sep=" ", collapse="<br />"))
              tree$closeTag() #Description
            }
            tree$addNode("styleUrl", style)
            tree$addNode("MultiGeometry", close=F)
              for (j in 1:length(polys[[i]])) {
                tree$addNode("Polygon", close=F)
                  tree$addNode("altitudeMode", 'absolute')
                  tree$addNode("tessellate", 1)
                  tree$addNode("outerBoundaryIs", close=F)
                    tree$addNode("LinearRing", close=F)
                      tree$addNode('coordinates', polys[[i]][[j]])
                    tree$closeTag() #LinearRing
                  tree$closeTag() #outerBoundaryIs
                tree$closeTag() #Polygon
              } #for j
            tree$closeTag() #MultiGeometry
          tree$closeTag() #Placemark
        }
      tree$closeTag() #Folder
      return(tree)
    }


	
.KMLlines <- function(tree, object, style, foldername, altitude="0", ...) {
      colnm <- paste("<b>", names(slot(object, 'data')), ":</b>", sep="")
      lines <- coordinates(object)
      if (length(altitude)==length(lines)) {
        alt <- altitude
      } else {
        alt <- rep(altitude, length.out=length(lines))
      }
      print(alt)
      for (ctr in 1:length(lines))
        lines[[ctr]] <- lapply(lines[[ctr]],
                                      function(y) paste(apply(
                                                  y,
                                                  1,
                                                  function(z) paste(c(z, alt[[ctr]]), collapse=',')
                                                  ), collapse=" \n")
                                      )
      tree$addNode("Folder", close=F)
        tree$addNode("name", foldername)
        tree$addNode("open", 0)
        for (i in 1:nrow(slot(object, 'data'))) {
          tree$addNode("Placemark", close=F)
            tree$addNode("description", close=F)
              tree$addCData(paste(colnm, format(slot(object, 'data')[i,], scientific=F), sep=" ", collapse="<br />"))
            tree$closeTag() #Description
            tree$addNode("styleUrl", style)
            tree$addNode("MultiGeometry", close=F)
              for (j in 1:length(lines[[i]])) {
                tree$addNode("LineString", close=F)
                  tree$addNode("altitudeMode", 'absolute')
                  tree$addNode("extrude", 0)
                  tree$addNode("tessellate", 1)
                  tree$addNode('coordinates', lines[[i]][[j]])
                tree$closeTag() #LineString
              } #for j
            tree$closeTag() #MultiGeometry
          tree$closeTag() #Placemark
        }
      tree$closeTag() #Folder
      return(tree)
    }


.KMLpoints <- function(tree, object, style, foldername, ...) {
      colnm <- paste("<b>", names(slot(object, 'data')), ":</b>", sep="")
      tree$addNode("Folder", close=F)
        tree$addNode("name", foldername)
        tree$addNode("open", 0)
        for (i in 1:nrow(slot(object, 'data'))) {
          tree$addNode("Placemark", close=F)
            tree$addNode("description", close=F)
              tree$addCData(paste(colnm, format(slot(object, 'data')[i,], scientific=F), sep=" ", collapse="<br />"))
            tree$closeTag() #Description
            tree$addNode("styleUrl", style)
            tree$addNode("Point", close=F)
              tree$addNode('coordinates', paste(c(coordinates(object)[i,], "0"), collapse=","))
            tree$closeTag() #Point
          tree$closeTag() #Placemark
        }
      tree$closeTag() #Folder
      return(tree)
    }

.KMLcolor <- function(colors, alpha="ff", ...) {
      padhex <- function(x) ifelse(x<16, paste("0", as.character(as.hexmode(x)), sep=""), as.character(as.hexmode(x)))
        rgbcolor <- col2rgb(colors)
        apply(rgbcolor, 2, function(x) paste(alpha, padhex(x[3]), padhex(x[2]), padhex(x[1]), sep=""))
    }

.KMLreproject <- function(object) {
	require(rgdal)
	return(spTransform(object, CRS("+proj=longlat")))
}

	
.KML <- function(object, name=substitute(object), colors=palette(), file, altitude="0", ...) {
	
	require(XML)
    colors <- .KMLcolor(colors, ...)
    
	tr <- xmlTree("kml",
                      namespaces=c("http://www.opengis.net/kml/2.2", gx="http://www.google.com/kml/ext/2.2", kml="http://www.opengis.net/kml/2.2", atom="http://www.w3.org/2005/Atom"))
       tr$addNode("Document", close=F)
         tr$addNode("name", name)
         if (class(object) %in% c('SpatialPointsDataFrame', 'SpatialPolygonsDataFrame', 'SpatialLinesDataFrame', 'SpatialPoints', 'SpatialPolygons', 'SpatialLines')) {
            object <- .KMLreproject(object)
            tr <- .KMLstyle(tr, 'style1', color=colors[1])
            tr <- switch(class(object),
              'SpatialPointsDataFrame' = .KMLpoints(tr, object, style='#style1', foldername=name, ...),
              'SpatialPolygonsDataFrame' = .KMLpolygons(tr, object, style='#style1', foldername=name, altitude=altitude, ...),
              'SpatialLinesDataFrame' = .KMLlines(tr, object, style='#style1', foldername=name, altitude=altitude, ...),
              'SpatialPoints' = .KMLpoints(tr, object, style='#style1', foldername=name, ...),
              'SpatialPolygons' = .KMLpolygons(tr, object, style='#style1', foldername=name, altitude=altitude, ...),
              'SpatialLines' = .KMLlines(tr, object, style='#style1', foldername=name, altitude=altitude, ...)
              )
         } else {
            object <- lapply(object, .KMLreproject)
            for (i in 1:length(object)) tr <- .KMLstyle(tr, paste('style', i, sep=""), color=colors[i])
            for (i in 1:length(object)) {
              tr <- switch(class(object[[i]]),
                'SpatialPointsDataFrame' = .KMLpoints(tr, object[[i]], style=paste('#style', i, sep=""), foldername=names(object)[[i]], ...),
                'SpatialPolygonsDataFrame' = .KMLpolygons(tr, object[[i]], style=paste('#style', i, sep=""), foldername=names(object)[[i]], altitude=altitude[i], ...),
                'SpatialLinesDataFrame' = .KMLlines(tr, object[[i]], style=paste('#style', i, sep=""), foldername=names(object)[[i]], altitude=altitude[i], ...),
                'SpatialPoints' = .KMLpoints(tr, object[[i]], style=paste('#style', i, sep=""), foldername=names(object)[[i]], ...),
                'SpatialPolygons' = .KMLpolygons(tr, object[[i]], style=paste('#style', i, sep=""), foldername=names(object)[[i]], altitude=altitude[i], ...),
                'SpatialLines' = .KMLlines(tr, object[[i]], style=paste('#style', i, sep=""), foldername=names(object)[[i]], altitude=altitude[i], ...)
                )
            }
         }
       tr$closeTag() #Document
       xml <- saveXML(tr, encoding = "UTF-8")
       if (!missing(file)) cat(xml, file=file)
       invisible(xml)
 }

    #a <- data.frame(letter = letters, num = round(runif(26, 1, 30), 2))
    #coordinates(a) <- matrix(c(runif(26, 9, 10), runif(26, 55, 56)), ncol=2)
    #KML(a)
    #summary(KMLreproject(a))
    #KML(split(a, is.odd(1:nrow(a))), colors=c("red", "green"), file="c:/test.kml")