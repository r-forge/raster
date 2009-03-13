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

1) Create a RasterLayer object from a file

\code{raster(x, values=FALSE, band=1)}

\item{\code{x}}{character. name of raster file  }
\item{\code{band}}{band number in case of a file of multiple bands, default = 1 }
\item{\code{values}}{logical. If \code{TRUE}, RasterLayer values will be read into memory with 'readAll()'}

If \code{x} is a character value, it should be a filename of a file that the raster package can read. Supported file types are the 
'native' raster package format and those that can be read via rgdal. See ?readGDAL help for supported file types.


2) Create a RasterLayer object from scratch

\code{raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projstring="+proj=longlat +datum=WGS84")}

\item{nrows}{number of rows on raster }
\item{ncols}{number of columns on raster }
\item{xmn}{minimum x coordinate or raster (left border) }
\item{xmx}{maximum x coordinate of raster (right border) }
\item{ymn}{minimum y coordinate or raster (bottom border) }
\item{ymx}{maximum y coordinate of raster (top border) }
\item{projstring}{PROJ4 type description of a map projection}
\item{bndbox}{bounding box used to crop a raster. Any object that is a BoundingBox object or contains one (such as Raster* objects)}

(item \code{x} is 'missing' in this case)

  
3) Create a RasterLayer object from a BoundingBox object
  
\code{raster(x, nrows=10, ncols=10)}

\item{x}{BoundingBox object}
\item{nrows}{number of rows on raster }
\item{ncols}{number of columns on raster}


4) Create a RasterLayer object from a Raster* object. This copies the parameters of a Raster* object to a new RasterLayer, 
but does not copy the filename nor the data associated with the original raster. Use it when creating new RasterLayer objects from existing objects.

\code{raster(x, filename="", values=NULL)}

\item{\code{x}}{a Raster* object}
\item{\code{filename}}{new filename for new RasterLayer }
\item{\code{values}}{ use to set values to new object. Either pass a vector of length == ncells(x), or a single number }
}


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
bb <- getBbox(r1)
r2 <- raster(bb)
r2[] <- runif(ncell(r2))

#from another Raster* object
r3 <- raster(r)
}

\keyword{methods}
\keyword{spatial}

