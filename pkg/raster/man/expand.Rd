\name{expand}

\alias{expand}

\title{Expand}

\description{
Expand returns an RasterLayer with a larger spatial extent. 
The output RasterLayer has the extreme minimum and maximum coordinates of the input RasterLayer and input BoundingBox arguments. 
Thus, all of the cells of the original raster are included.
}

\usage{
expand(raster, bndbox, filename=NULL, filetype='raster', overwrite=FALSE, track=-1)
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{bndbox}{BoundingBox object, or any object that is or has a bounding box (see below)}
  \item{filename}{filename for the output RasterLayer}
  \item{overwrite}{if TRUE, "filename" will be overwritten if it exists}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}
}


\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\seealso{\code{\link[raster]{crop}}, \code{\link[raster]{merge}}}


\examples{
r <- raster(xmn=-150, xmx=-120, ymx=60, ymn=30, ncol=36, nrow=18)
r[] <- 1:ncell(r)
bb <- newBbox(-180, 0, 0, 90)
re <- expand(r, bb)
}

\keyword{spatial}
