\name{distance-methods}
\docType{methods}
\alias{distance-methods}
\alias{distance}
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
r1[345:355] <- 1
distmap <- distance(r1) 
#plot(distmap)
}
\keyword{methods}
\keyword{spatial}
