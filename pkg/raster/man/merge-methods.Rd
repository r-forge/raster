\name{merge}

\docType{methods}
\alias{merge}
\alias{merge,RasterLayer,RasterLayer-method}
\title{ 
Merge RasterLayers

}

\description{
Merge RasterLayers to form a new RasterLayer with a larger spatial extent

This is a method for the generic function in Package `base'

}

\section{Methods}{
\describe{
\code{merge(x, y, ..., tolerance=0.05, filename="", overwrite=FALSE) }

\item{x,y}{"RasterLayer"}{
  \item{...}{additional RasterLayer objects}
  \item{tolerance}{difference permissable (relative to the cell resolution) for objects to be 'equal'. See ?all.equal}
  \item{filename}{output filename for a new raster}
  \item{overwrite}{if \code{TRUE}, the file will be overwritten if it exists}
}} }

\details{
The RasterLayer objects must have the same origin and resolution. In areas where the RasterLayers overlap, the values of the RasterLayer that is first in the sequence will be retained. 
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{crop}}, \code{\link[raster]{expand}}}

\examples{
r1 <- raster(xmx=-150, ymn=60, ncols=30, nrows=30)
r1[] <- 1:ncell(r1)
r2 <- raster(xmn=-100, xmx=-50, ymx=50, ymn=30)
r2 <- setRes(r2, xres(r1), yres(r1))
r2[] <- 1:ncell(r2)
rm <- merge(r1, r2)
}

\keyword{methods}
\keyword{spatial}



