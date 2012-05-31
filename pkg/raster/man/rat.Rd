\name{RAT}

\docType{methods}

\alias{ratToLayer}
\alias{ratify}
\alias{rats}
\alias{rats<-}
   
  
\title{Raster Attributes}

\description{
A Raster Attribute Table (RAT) is a data.frame that links the (integer) cell values to one or more variables. Thus the cell values are an index, whereas the actual values of interest are in the RAT (and these can include character and factor variables). This approach can be advantageous when you want to store multiple attributes for raster 'zones'. However, in the contexgt of the raster package it is not recommended to use this approach. The functions documented here are mainly availbe such that files with a RAT can be read and processed. In stead of a RAT you can use a seperate layer to represent each variable.

Function 'rats' returns the RAT for inspection. It can be modfied and set using \code{rats <-}  (but use caution as it is easy to mess things up). Function 'ratToLayer' creates a single layer for each variable in the RAT table. Function 'ratify' creates a RasterLayer with a RAT table.

The first two columns in the RAT table, VALUE and COUNT, are fixed and should not be manipulated.
}

\usage{
rats(x)
rats(x) <- value
ratToLayer(x, att=NULL, filename='', ...) 
ratify(x, filename='', ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{value}{data.frame representing a RAT}
  \item{att}{numeric or character. Which variable(s) in the RAT table should be de-ratified? If \code{NULL}, all variables are extracted}
  \item{filename}{Character. Optional}  
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}    
}


\value{
data.frame or Raster* object
}


\author{Robert J. Hijmans}

\examples{

library(raster)
r <- raster(nrow=10, ncol=10)
r[] = 1
r[51:100] = 2
r[3:6, 1:5] = 3
r <- ratify(r)

rat <- rats(r)
rat$landcover = c('Pine', 'Oak', 'Meadow')
rat$lc_code = c(12,25,30)
rats(r) <- rat
r

rr <- writeRaster(r, 'test.grd', overwrite=TRUE)
rr

x <- ratToLayer(r, 'landcover')
x
is.factor(x)
labels(x)

}

\keyword{methods}
\keyword{spatial}

