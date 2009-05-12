\name{crop}
\alias{crop}

\title{Crop}

\description{
crop returns a geographic subset of the input RasterLayer as specified by a bounding box (or object that has a bounding box). 
Areas in the bounding box outside of the intput RasterLayer are ignored, and the BoundingBox is aligned to the cells of the input RasterLayer. 
}

\usage{
crop(raster, bndbox, filename="", overwrite=FALSE, filetype='raster', datatype=dataType(raster), track=-1) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{bndbox}{BoundingBox object, or any object that is or has a bounding box (see below)}
  \item{filename}{Character. Output filename for a new raster}
  \item{overwrite}{If \code{TRUE}, the file will be overwritten if it exists}
  \item{filetype}{Output file type. Either 'raster', 'ascii' or a supported GDAL format see \code{\link[raster]{writeRaster}}}
  \item{datatype}{Output data type; see \code{\link[raster]{dataType}}}
  \item{track}{Vector of row numbers for which the function will report that they have been processed}  
}

\details{
 Objects that have a BoundingBox object include RasterLayer, RasterStack. 
 They can also be extracted from objects descending from the Spatial class in the sp package. 
 You can check this with \code{extent}. New BoundingBox objects can be created with \code{newExtent}.
 
 If no filename is specified, and the resulting RasterLayer is too large to hold in memory, it is saved to a temporary file.  
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\seealso{ 
\code{\link[raster]{expand}}, \code{\link[raster]{merge}}
}

\examples{
r <- raster(nrow=45, ncol=90)
r[] <- 1:ncell(r)
bb <- newExtent(-160, 10, 30, 60)
rc <- crop(r, bb)	
}

\keyword{spatial}
