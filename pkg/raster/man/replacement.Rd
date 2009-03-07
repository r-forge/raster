\name{replaceMethods}
\docType{methods}

\alias{[,RasterLayer-method}
\alias{[,RasterLayer,ANY,ANY-method}
\alias{[,RasterLayer,ANY,missing-method}

\alias{[[,RasterLayer,ANY,ANY-method}

\alias{[<-,RasterLayer-method}
\alias{[<-,RasterLayer,ANY,missing-method}

\title{ Replace methods }

\description{
  \code{[} is sometimes equivalent to setValues() 
}

\section{Methods}{
\describe{
  if r is a RasterLayer, r[] is valid with a single index (cell number)
  e.g.: r[1], r[6:15]
  r[[]] is valid with two indices
  e.g.: r[[1,1]], r[[1,]]

}}

\examples{
r <- raster(ncol=10, nrow=5)
r[] <- 1:ncell(r) * 2
r[1]

r[1:10]
r[3:8] <- NA
r[1:10]

#r[[,1]]
#r[[1,]]

}

\keyword{methods}
\keyword{spatial}
