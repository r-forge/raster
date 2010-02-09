\name{hist}

\docType{methods}

\alias{hist}
\alias{hist,RasterLayer-method}
\alias{hist,RasterStackBrick-method}

\title{Histogram}

\description{
Create a histogram of the values of a RasterLayer. NA values are ignored. Large datasets are sampled using \code{maxsamp}.
}

\usage{
hist(x, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{...}{Additional arguments. See under Methods and at \code{\link[graphics]{hist}}}  
}

\section{Methods}{
\describe{
A full call as implemented here:

\code{hist(x, maxsamp=100000, main=NA, plot=TRUE, ...)}

maxsamp is the maximum number of (randomly sampled) cells to be used for creating the histogram

see \code{\link[graphics]{hist}} for the other arguments
}}


\seealso{ \code{\link[raster]{plot}} }

\examples{ 
r1 <- raster(nrows=50, ncols=50)
r1 <- setValues(r1, runif(ncell(r1)))
r2 <- setValues(r1, runif(ncell(r1)))
rs <- r1 + r2
rp <- r1 * r2
par(mfrow=c(2,2))
plot(rs, main='sum')
plot(rp, main='product')
hist(rs)
a = hist(rp)
a
}

\keyword{methods}
\keyword{spatial}

