\name{distance-methods}
\docType{methods}
\alias{distance-methods}
\alias{distance,RasterLayer-method}
\title{ Calculate distance}
\description{
 ~~ Methods for function \code{distance}  ~~
}
\section{Methods}{
\describe{

\item{raster = "RasterLayer"}{ ~~describe this method here }
}}
\examples{
r1 <- newRaster(ncol=36,nrow=18)
data <- rep(NA, times=ncell(r1))
data[345:355] <- 1
r1 <- setValues(r1,data)
distmap <- distance(r1)
map(distmap)
}
\keyword{methods}
\keyword{spatial}
