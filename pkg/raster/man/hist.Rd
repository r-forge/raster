\name{hist}

\docType{methods}

\alias{hist}
\alias{hist,Raster-method}
\alias{hist,RasterStack-method}


\title{Histogram}

\description{
Create a histogram of the values of a RasterLayer. NA values are ignored. 
}


\usage{
hist(x, ...) 
}


\arguments{
  \item{x}{A Raster* object}
  \item{...}{Additional arguments. See \code{?hist}}  
}


\section{Methods}{
\describe{

\code{hist(x, maxsamp=100000, ...)}

maxsamp is the maximum number of (randomly selected) grid cells to use for creating the histogram

}}


\seealso{ \code{\link[raster]{plot}} }

\examples{ 
r1 <- raster(nrows=25, ncols=25)
r1 <- setValues(r1, runif(ncell(r1)))
r2 <- setValues(r1, runif(ncell(r1)))
rs <- r1 + r2
rp <- r1 * r2
par(mfrow=c(2,2))
plot(rs, main='sum')
plot(rp, main='product')
hist(rs, main='')
hist(rp, main='')
}


\keyword{methods}
\keyword{spatial}

