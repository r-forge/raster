\name{morphMerge}

\docType{methods}

\alias{morphMerge}

\title{ 
Morph & merge Raster* object into a new one
}

\description{
Morph Raster* objects to a merged Raster* object.
}

\usage{
morphMerge(x, y, ..., crs, res, fun, method='bilinear', filename='')
}

\arguments{
  \item{x}{A Raster* object}
  \item{y}{A Raster* object with the same number of layers as \code{x}}
  \item{...}{Additional Raster* objects and other arguments. See below, under Methods}  
  \item{crs}{ object}
  \item{res}{ object}
  \item{fun}{ A function }
  \item{method}{ object}
  \item{filename}{ object}
}


\details{
yadiya
}

\value{
A Raster* object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{morph}}, \code{\link[raster]{mosaic}}, \code{\link[raster]{merge}}, \code{\link[raster]{mosaic}} }


\keyword{methods}
\keyword{spatial}

