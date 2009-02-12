\name{crop}
\alias{crop}

\title{Crop the extent of a RasterLayer}
\description{
crop returns a subset of the input RasterLayer as specified by a the supplied bounding box (or object that has a bounding box). Areas in the bounding box outside of the intput RasterLayer are ignored. 
}
\usage{
crop(raster, bndbox, filename="", overwrite=FALSE, filetype='raster', track=-1) 
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{bndbox}{BoundingBox object, or any object that is or has a bounding box (see below)}
  \item{filename}{character. Output filename for a new raster}
  \item{overwrite}{if \code{TRUE}, the file will be overwritten if it exists}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}  
}

\details{
 Objects that have a BoundingBox object include RasterLayer, RasterStack. They can also be extracted from objects descending from the Spatial class in the sp package. 
 You can check this with getBbox(object). New BoundingBox objects can be reated with the function newBbox().
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans}

\seealso{ 
\code{\link[raster]{expand}}, \code{\link[raster]{merge}}
}

\examples{
r <- raster(nrow=45, ncol=90)
r[] <- 1:ncell(r)
bb <- newBbox(-160, 10, 30, 60)
rc <- crop(r, bb)	
}

\keyword{spatial}
