\name{raster}

\docType{methods}

\alias{raster}
\alias{raster,BoundingBox-method}
\alias{raster,character-method}
\alias{raster,missing-method}
\alias{raster,Raster-method}

\title{Create a RasterLayer object}

\description{
  Create a new RasterLayer object from a filename, from scratch, a BoundingBox, or a Raster* object.
  The created object does normally not contain any cell (pixel) values, it only has the parameters that describe the RasterLayer.
}


\arguments{
  \item{x}{a filename, BoundingBox, Raster* object, or missing}
  \item{...}{ additional argumens }  
}

\usage{
raster(x, ...)
}

\section{Methods}{
\describe{

1) Create a RasterLayer object from a file

\code{raster(x, values=FALSE, band=1)}


\tabular{rll}{
\tab \code{x} \tab character. name of raster file  \cr
\tab \code{band} \tab band number in case of a file of multiple bands, default = 1 \cr
\tab \code{values} \tab logical. If \code{TRUE}, RasterLayer values are read into memory with \code{\link[raster]{readAll}} \cr
}

If \code{x} is a character value, it should be a filename of a file that the raster package can read. Supported file types are the 
'native' raster package format and those that can be read via rgdal. See \code{\link[gdal]{readGDAL}} help for supported file types.



2) Create a RasterLayer object from scratch

\code{raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projs="+proj=longlat +datum=WGS84")}

\tabular{rll}{
 \tab \code{nrows} \tab number of rows on raster \cr
 \tab \code{ncols} \tab number of columns on raster \cr
 \tab \code{xmn} \tab minimum x coordinate or raster (left border) \cr
 \tab \code{xmx} \tab maximum x coordinate of raster (right border) \cr
 \tab \code{ymn} \tab minimum y coordinate or raster (bottom border) \cr
 \tab \code{ymx} \tab maximum y coordinate of raster (top border) \cr
 \tab \code{projs} \tab character. PROJ4 type description of a map projection \cr
 \tab \code{bndbox} \tab bounding box used to crop a raster. Any object that is a BoundingBox object or contains one (such as Raster* objects) \cr
}

(item \code{x} is 'missing' in this case)


  
3) Create a RasterLayer object from a BoundingBox object
  
\code{raster(x, nrows=10, ncols=10, projs='NA')}

\tabular{rll}{
 \tab \code{x} \tab BoundingBox object \cr
 \tab \code{nrows} \tab number of rows on raster  \cr
 \tab \code{ncols} \tab number of columns on raster \cr
 \tab \code{projs} \tab PROJ4 type description of a map projection \cr
}



4) Create a RasterLayer object from a Raster* object. This copies the parameters of a Raster* object to a new RasterLayer, 
but does not copy the filename nor the data associated with the original Raster* object. 


\code{raster(x, filename="", values=NULL)}

\tabular{rll}{
 \tab \code{x} \tab a Raster* object \cr
 \tab \code{filename} \tab new filename for new RasterLayer \cr
 \tab \code{values} \tab If not \code{NULL} it is used to set values to new object. Either pass a vector of length == ncells(x), or a single number \cr
}

}}


\details{
New RasterLayer objects normally have no values in memory. 
You can get values into memory with \code{\link[raster]{readRow}}, and \code{\link[raster]{readAll}}; extract values with \code{\link[raster]{xyValues}};
Or set values with \code{\link[raster]{Replacement}} functions.
}


\value{
RasterLayer object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{stack},  \link[raster]{values}, \link[gdal]{readGDAL} } }


\examples{
#from file
r <- raster(system.file("external/test.ag", package="sp"))
logo <- raster(system.file("pictures/Rlogo.jpg", package="rgdal"), values=TRUE)

#from scratch
r1 <- raster(nrows=108, ncols=21, xmn=0, xmx=10)

#from a BoundingBox
bb <- getBbox(r)
r2 <- raster(bb)
r2[] <- runif(ncell(r2))

#from another Raster* object
r3 <- raster(r)
s <- stack(r, r)
r3 <- raster(s)
}

\keyword{methods}
\keyword{spatial}

