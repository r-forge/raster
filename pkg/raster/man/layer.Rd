\name{layer}

\docType{methods}

\alias{layer}
\alias{layer,Extent-method}
\alias{layer,character-method}
\alias{layer,missing-method}
\alias{layer,Raster-method}
\alias{layer,RasterStack-method}
\alias{layer,RasterBrick-method}
\alias{layer,Spatial-method}
\alias{layer,SpatialGrid-method}
\alias{layer,SpatialPixels-method}
\alias{layer,matrix-method}

\title{Create a RasterLayer object}

\description{

NOTE: 'layer' is equivalent to 'raster'. It _may_ replace 'raster' because of a name collision with that function name in R v 2.11.1. 

Methods to create a RasterLayer object. RasterLayer objects can be created from scratch, a filename, a Extent object, a matrix, or a Raster* or Spatial* object.

In many cases, e.g. when a RasterLayer is created from a file, it does (initially) not contain any cell (pixel) values, it only has the parameters that describe the RasterLayer.

If you are new to the raster package, first have a look here: \code{\link{raster-package}}.
}


\arguments{
  \item{x}{a filename, Extent, Raster*, SpatialPixels*, SpatialGrid* object, matrix, or missing}
  \item{...}{ additional argumens, see below }  
}

\usage{
layer(x, ...)
}

\section{Methods}{
\describe{

1) Create a RasterLayer object from a file

\code{layer(x, values=FALSE, band=1, ...)}


\tabular{rll}{
\tab \code{x} \tab character. Name of raster file  \cr
\tab \code{values} \tab logical. If \code{TRUE}, RasterLayer values are read into memory with \code{\link[raster]{readAll}} \cr
\tab \code{band} \tab integer. Band number in case of a file of multiple bands, default = 1 \cr
\tab \code{...} \tab additional arguments.  \cr
}

Additional arguments:
\tabular{rll}{
\tab \code{native} \tab Logical.  Default is \code{FALSE} except when package rgdal is missing. If \code{TRUE}, reading and writing of IDRISI, BIL, BSQ, BIP, and ArcAscii files is done with native (raster package) drivers, rather then via rgdal. 'raster', RSAGA, and netcdf format files are always read with native drivers.
}

More additional arguments, that can be used for netCDF files only:
\tabular{rll}{
\tab \code{xvar} \tab character. The x variable (e.g. 'longitude' or 'x') \cr
\tab \code{yvar} \tab character. The y variable (e.g. 'latitude' or 'y')\cr
\tab \code{zvar} \tab character. The z variable (e.g. 'altitide' or 'precipitation')\cr
\tab \code{time} \tab integer > 0. The 'time' variable (if there are any) (default=NA)\cr
}

If \code{x} is a character value, it should be a filename of a file that the raster package can read. Supported file types are the 'native' raster package format and those that can be read via rgdal. See \code{\link[rgdal]{readGDAL}} help for supported file types.

For netCDF files, the function will try to guess values for the \code{xvar}, \code{yvar}, and \code{zvar} if they are not supplied. For this function to work, the RNetCDF package needs to have been installed. Note that currently, a raster object made from a netCDF file will load all values from file at creation (unlike other raster objects), and that \code{read*} functions are therefore not applicable. 
netCDF data can only be read if the data are stored in 2 (x and y) or 3 (x, y, and time) dimensions. If the data has three dimensions, a value must be supplied for \code{time}


2) Create a RasterLayer object from scratch

\code{layer(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projs="+proj=longlat +datum=WGS84")}

\tabular{rll}{
 \tab \code{nrows} \tab number of rows  \cr
 \tab \code{ncols} \tab number of columns \cr
 \tab \code{xmn} \tab minimum x coordinate (left border) \cr
 \tab \code{xmx} \tab maximum x coordinate (right border) \cr
 \tab \code{ymn} \tab minimum y coordinate (bottom border) \cr
 \tab \code{ymx} \tab maximum y coordinate (top border) \cr
 \tab \code{projs} \tab Character. PROJ4 type description of a Coordinate Reference System (map projection) \cr
}
(item \code{x} is 'missing' in this case)

 
3) Create a RasterLayer object from an Extent object
  
\code{layer(x, nrows=10, ncols=10, projs=NA)}

\tabular{rll}{
 \tab \code{x} \tab Extent object \cr
 \tab \code{nrows} \tab number of rows  \cr
 \tab \code{ncols} \tab number of columns  \cr
 \tab \code{projs} \tab PROJ4 type description of a map projection \cr
}



4) Create a RasterLayer object from a RasterLayer object. This copies the parameters of a RasterLayer object to a new RasterLayer, 
but does not copy the filename nor the data associated with the original Raster object. 

\code{layer(x, filename="", values=NULL)}

\tabular{rll}{
 \tab \code{x} \tab a Raster* object \cr
 \tab \code{filename} \tab new filename for new RasterLayer \cr
 \tab \code{values} \tab If not \code{NULL} it is used to set values to new object. Either pass a vector of length == ncells(x), or a single number \cr
}


5) Create a RasterLayer object from a RasterStack or RasterBrick object.

\code{layer(x, index=0)}

\tabular{rll}{
 \tab \code{x} \tab a RasterStack, SpatialPixels* or SpatialGrid* object \cr
 \tab \code{layer} \tab Integer. The layer from which to copy values to the new RasterLayer, if \code{layer > 0}.  \cr
}


6) Create a RasterLayer object from a SpatialPixels* or SpatialGrid* object. 

\code{layer(x, index=0)}

\tabular{rll}{
 \tab \code{x} \tab a RasterStack, SpatialPixels* or SpatialGrid* object \cr
 \tab \code{layer} \tab Integer. the layer from which to copy values to the new RasterLayer, if \code{layer > 0}.\cr
}


7) Create a RasterLayer object from a matrix. The default extent is set to be between 0 and 1 in the x and y direction but can be changed at creation of the RasterLayer object or later. You can also provide a projection.  

\code{function(x, xmn=0, xmx=1, ymn=0, ymx=1, projs=NA)}

\tabular{rll}{
 \tab \code{x} \tab a matrix \cr
 \tab \code{xmn} \tab minimum x coordinate (left border) \cr
 \tab \code{xmx} \tab maximum x coordinate (right border) \cr
 \tab \code{ymn} \tab minimum y coordinate (bottom border) \cr
 \tab \code{ymx} \tab maximum y coordinate (top border) \cr
 \tab \code{projs} \tab PROJ4 type description of a map projection \cr
}

}}


\details{
New RasterLayer objects normally have no values in memory. 
If they are created from a file on disk, you can access values with \code{\link[raster]{getValues}), \link[raster]{xyValues}} and related functions.
You can assign new values with  \code{\link[raster]{setValues}} and with \code{\link[raster]{replacement}}.
}


\value{
RasterLayer object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{stack},  \link[raster]{getValues}}  }


\examples{
#from file
r <- layer(system.file("external/test.grd", package="raster"))
logo <- layer(system.file("pictures/Rlogo.jpg", package="rgdal"), values=TRUE) 

#from scratch
r1 <- layer(nrows=108, ncols=21, xmn=0, xmx=10)

#from an Extent object
e <- extent(r)
r2 <- layer(e)

#from another Raster* object
r3 <- layer(r)
s <- stack(r, r)
r3 <- layer(s)
}

\keyword{methods}
\keyword{spatial}

