\name{morph}

\docType{methods}

\alias{morph}

\title{ 
Morph a Raster* object into another
}

\description{
Morph Raster* objects to the parameters of another Raster* object.
}

\usage{
morph(from, to, method='bilinear', filename='', ...) 
}

\arguments{
  \item{from}{A Raster* object}
  \item{to}{A Raster* object with the same number of layers as \code{x}}
  \item{method}{ object}
  \item{filename}{ object}
  \item{...}{Additional Raster* objects and other arguments. See below, under Methods}  
}

\details{
yadiya
}

\value{
A Raster* object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{mosaic}}, \code{\link[raster]{crop}}, \code{\link[raster]{expand}}}

\examples{
r1 <- raster()
r1[] <- 1:ncell(r1)
r2 <- raster(xmn=-100, xmx=-50, ymx=50, ymn=30)
res(r2) <- c(5, 5)
r2[] <- 1:ncell(r2)
rm <- morph(r1, r2)
}

\keyword{methods}
\keyword{spatial}

