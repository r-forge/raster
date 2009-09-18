\name{expand}

\alias{expand}

\title{Expand}

\description{
Expand returns an RasterLayer with a larger spatial extent. 
The output RasterLayer has the extreme minimum and maximum coordinates of the input RasterLayer and input Extent arguments. 
Thus, all of the cells of the original raster are included.
}

\usage{
expand(raster, bndbox, filename=NULL, ...)
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{bndbox}{Extent object, or any object that is or has a bounding box (see below)}
  \item{filename}{filename for the output RasterLayer}
  \item{...}{additional arguments. See Details.}  
}


\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab overwrite \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab filetype \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab datatype \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab track \tab vector of row numbers for which the function will report that they have been processed  \cr
 }
}

\author{Robert J. Hijmans}

\seealso{\code{\link[raster]{crop}}, \code{\link[raster]{merge}}}


\examples{
r <- raster(xmn=-150, xmx=-120, ymx=60, ymn=30, ncol=36, nrow=18)
r[] <- 1:ncell(r)
bb <- newExtent(-180, 0, 0, 90)
re <- expand(r, bb)
}

\keyword{spatial}
