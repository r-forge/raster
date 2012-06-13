\name{factors}

\docType{methods}

\alias{is.factor}
\alias{is.factor,Raster-method}
\alias{is.factor,RasterStack-method}
\alias{as.factor}
\alias{as.factor,RasterLayer-method}
\alias{levels}
\alias{levels,Raster-method}
\alias{levels,RasterStack-method}
\alias{levels<-}
\alias{levels<-,Raster-method}
   
      
\alias{factorValues}
\alias{ratify}
\alias{deratify}
  
\title{Factors}

\description{
Factors are categorical variables. These functions allow for defining a layer as a categorical variable. This creates a layer with integer values that are linked to factor values via a Raster Attribute Table (RAT). The RAT is a data.frame that links the (integer) cell values to one or more variables. Thus the cell values are an index, whereas the actual values of interest are in the RAT. The RAT has three or more columns. The first two columns in the RAT table, VALUE and COUNT, are fixed and should not be changed.

You can use multiple factor variables in a single RAT (to store multiple attributes for raster 'zones'). However, in the context of the raster package it is not recommended to do that. The functions documented here are mainly availbe such that files with a RAT can be read and processed. 

Function 'levels' returns the RAT for inspection. It can be modfied and set using \code{levels <- value}  (but use caution as it is easy to mess things up). 

\code{as.factor} and \code{ratify} create a layer with a RAT table. Function 'deratify' creates a single layer for each variable in the RAT table. 

This feature is not used much in the package. Whether a layer is defined as a factor or not is currently ignored by all, except the 'extract' functions (when used with option df=TRUE).
}

\usage{
is.factor(x)
as.factor(x)
levels(x)

factorValues(x, v, layer=1, att=NULL)

ratify(x, filename='', ...)
deratify(x, att=NULL, layer=1, complete=FALSE, drop=TRUE, filename='', ...) 
}


\arguments{
  \item{x}{Raster* object}
  \item{v}{index cell values}
  \item{layer}{index cell values}
  \item{att}{numeric or character. Which variable(s) in the RAT table should be used. If \code{NULL}, all variables are extracted. If using a numeric, skip the first two default columns}
  \item{filename}{character. Optional}  
  \item{...}{additional arguments as for \code{\link{writeRaster}}}    
  \item{complete}{logical. If \code{TRUE}, the layer returned is no longer a factor}  
  \item{drop}{logical. If \code{TRUE} a factor is converted to a numerical value if possible}  
}  



\section{Methods}{
\describe{
factor type layers can be created with \code{as.factor} (RasterLayer objects only) or \code{levels(x) <- value}. \code{value} can be a factor or vector for RasterLayer objects. For multi-layer objects, it should be a list of the same lenght as nlayers(x). Each element of the list should be either NULL or a vector or factor. 
}}


\value{
Raster* object
}


\author{Robert J. Hijmans}

\examples{

set.seed(0)
r <- raster(nrow=10, ncol=10)
r[] <- runif(ncell(r)) * 10
is.factor(r)

r <- round(r)
f <- as.factor(r)
is.factor(f)

x <- levels(f)[[1]]
x
x$code = letters[10:20]
levels(f) <- x
levels(f)
f


r <- raster(nrow=10, ncol=10)
r[] = 1
r[51:100] = 2
r[3:6, 1:5] = 3
r <- ratify(r)

rat <- levels(r)[[1]]
rat$landcover = c('Pine', 'Oak', 'Meadow')
rat$lc_code = c(12,25,30)
levels(r) <- rat
r

# extract values for some cells
i <- extract(r, c(1,2, 25,100))
i
# get the attribute values for these cells
factorValues(r, i)

# write to file:
rr <- writeRaster(r, 'test.grd', overwrite=TRUE)
rr

# create a single-layer factor 
x <- deratify(r, 'landcover')
x
is.factor(x)
levels(x)
}

\keyword{methods}
\keyword{spatial}

