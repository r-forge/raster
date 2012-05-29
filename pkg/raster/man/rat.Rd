\name{RAT}

\docType{methods}

\alias{ratToLayer}
\alias{ratify}
\alias{rats}
\alias{rats<-}
   
  
\title{Raster Attributes}

\description{
A Raster Attribute Table (RAT) is a data.frame that links the (integer) cell values to one or more variables. Thus the cell values are an index, whereas the actual values of interest are in the RAT (and these can include character and factor variables). This approach can be advantageous when you want to store multiple attributes for raster 'zones'.

Function 'rats' returns the RAT for inspection. It can be modfied and set using 'rats <-'  (but use caution as it is easy to mess things up). Function 'ratToLayer' creates a single layer for each variable in the RAT table. Function 'ratify' creates a RasterLayer with a RAT table.

The first two columns in the RAT table, VALUE and COUNT, are fixed and should not be manipulated.
}

\usage{
rats(x)
rats(x) <- value
ratToLayer(x, att=NULL, filename='', ...) 
ratify(x, filename='', ...)
}

\arguments{
  \item{x}{A Raster* object}
  \item{value}{data.frame representing a RAT}
  \item{att}{numeric or character. Which variable(s) in the RAT table should be de-ratified? If \code{NULL}, all variables are extracted}
  \item{filename}{Character. Optional}  
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}    
}


\value{
Raster* object
}


\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=10, ncol=10)
r[] <- (runif(ncell(r)) * 10)
is.factor(r)
r <- round(r)
f <- asFactor(r)
is.factor(f)
}

\keyword{methods}
\keyword{spatial}

