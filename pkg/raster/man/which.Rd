\name{which}

\docType{methods}

\alias{which}
\alias{which,RasterLayer-method}

\title{which}

\description{
\code{which} returns a RasterLayer with TRUE or FALSE (NA is set to FALSE)
}


\usage{
which(x, arr.ind=FALSE) 
}

\arguments{
  \item{x}{A RasterLayer object or a logical expression resulting in a RasterLayer object}
  \item{arr.ind}{Logical. If \code{FALSE}, a (logical) RasterLayer is returned, if \code{TRUE}, the cellnumbers are returned for which the condition is \code{TRUE}. Note that this behavior is somewhat different from the generic 'which' in the base package.} 
}

\seealso{ \code{\link[base]{which}} }

\value{
a RasterLayer object
}

\author{Robert J.Hijmans}

\examples{
r <- raster(ncol=10, nrow=10)
r[] <- runif(ncell(r))
r[r< 0.2] <- NA
s <- which(r > 0.5)
}

\keyword{spatial}
