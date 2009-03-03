\name{replaceMethods}
\docType{methods}

\alias{[,RasterLayer-method}
\alias{[<-,RasterLayer-method}
\alias{[[,RasterLayer,ANY,ANY-method}
\alias{[[<-,RasterLayer,ANY,ANY-method}
\alias{[,RasterStack-method}

\title{ Replace methods }

\description{
  \code{[} is sometimes equivalent to setValues() 
}

\section{Methods}{
\describe{


}}

\examples{
r <- raster(ncol=10, nrow=5)
r[] <- 1:ncell(r) * 2
r[1]
r[[1,1]]
r[50]
r[[5,10]]

r[1:10]
r[3:8] <- NA
r[1:10]

}

\keyword{methods}
\keyword{spatial}
