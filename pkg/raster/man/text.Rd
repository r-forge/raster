\name{text}

\docType{methods}

\alias{text}
\alias{text,RasterLayer-method}

\title{Plot a Raster* object}

\description{
Plots a textual (rather than color) representation of values of the cells on top anoter plot. 
}

\usage{
\S4method{text}{RasterLayer}(x, digits=0, fun=NULL, ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{digits}{Integer. how many digits should be used?}
  \item{fun}{Function to subset the values plotted (as in \code{\link{rasterToPoints}}}
  \item{...}{Additional arguments to pass to graphics function \code{\link[graphics]{text}} }
}

\seealso{
\code{\link[raster]{plot}}
}

\author{Robert J. Hijmans}

\examples{ 
r <- raster(nrows=4, ncols=4)
r <- setValues(r, 1:ncell(r))
plot(r)
text(r)

plot(r, col=bpy.colors(5))
text(r, fun=function(x){x<5 | x>12}, col=c('red', 'white'), vfont=c("sans serif", "bold"), cex=2)
}

\keyword{methods}
\keyword{spatial}

