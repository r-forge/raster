\name{expand}
\alias{expand}

\title{Expand the extent of a RasterLayer}
\description{
expand returns an elarged RasterLayer. The new RasterLayer has the extreme minimum and maximum coordinates of the input RasterLayer and BoundingBox object. Thus, all of the cells of the original raster are inlcluded.
}
\usage{
expand(raster, bndbox, filename="", overwrite=FALSE)
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{bndbox}{BoundingBox object, or any object that is or has a bounding box (see below)}
  \item{filename}{character. Output filename for a new raster}
  \item{overwrite}{if \code{TRUE}, the file will be overwritten if it exists}
}

\details{
 Objects that have a BoundingBox object include RasterLayer, RasterStack. They can also be extracted from objects descending from the Spatial class in the sp package. 
 You can check this with getBbox(object). New BoundingBox objects can be reated with the function newBbox().
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans}

\seealso{\code{\link[raster]{crop}}, \code{\link[raster]{Merge}}}


\examples{
r <- raster(xmn=-150, xmx=-120, ymx=60, ymn=30, nrow=30, ncol=30)
r[] <- 1:ncell(r)
bb <- newBbox(-180, 0, 0, 90)
re <- expand(r, bb)
}

\keyword{spatial}
