\name{RAT}

\docType{methods}

\alias{ratToLayer}
\alias{ratValues}
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
ratToLayer(x, att=NULL, filename='', ...) 
ratValues(x, v)
ratify(x, filename='', ...)
rats(x)
rats(x) <- value
}

\arguments{
  \item{x}{RasterLayer}
  \item{v}{index cell values}
  \item{att}{numeric or character. Which variable(s) in the RAT table should be de-ratified? If \code{NULL}, all variables are extracted}
  \item{filename}{Character. Optional}  
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}    
  \item{value}{data.frame representing a RAT}
}


\value{
data.frame or Raster* object
}


\author{Robert J. Hijmans}

\examples{

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

# extract values for some cells
i <- extract(r, c(1,2, 25,100))
# get the attribute values for these cells
ratValues(r, i)

# write to file:
rr <- writeRaster(r, 'test.grd', overwrite=TRUE)
rr

# create a layer for a variable
x <- ratToLayer(r, 'landcover')
x
is.factor(x)
labels(x)
}

\keyword{methods}
\keyword{spatial}

