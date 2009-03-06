\name{replaceMethods}
\docType{methods}

\alias{[,RasterLayer-method}
\alias{[,RasterStack-method}

\alias{[<-,RasterLayer,ANY,missing-method}
\alias{[<-,RasterLayer,missing,missing-method}

\alias{[[,RasterLayer,ANY,ANY-method}
\alias{[[<-,RasterLayer,ANY,ANY-method}

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
r[,1]
r[1,]

r[1:10]
r[3:8] <- NA
r[1:10]

}

\keyword{methods}
\keyword{spatial}
