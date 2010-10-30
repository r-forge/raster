\name{indexing}

\docType{methods}

\alias{[,Raster-method}
\alias{[,Raster,ANY,missing-method}
\alias{[,Raster,Spatial,missing-method}

\alias{[[,RasterLayer,ANY,ANY-method}
\alias{[[,RasterStackBrick,ANY,ANY-method}

\alias{[<-,RasterLayer-method}
\alias{[<-,RasterLayer,ANY,missing-method}

\title{Get or replace values of Raster objects}

\description{
 \code{object[[i]]} can be used toGet or set values of a Raster* object, when \code{i} is a vector of cell numbers or a Spatial* object (which is a wrapper around \code{\link{extract}}). 
  
 \code{object[[r,c]]} can be used to access values of a RasterLayer object, using row and column numbers as index. 
}


\section{Methods}{
\describe{

\code{object[x]}

\code{object[x] <- value}

\code{object[[r,c]]}

Arguments
\tabular{rll}{
\tab \code{object} \tab a RasterLayer object \cr
\tab \code{x} \tab cell number(s) \cr
\tab \code{r} \tab row number(s) \cr
\tab \code{c} \tab column number(s) \cr
\tab \code{value} \tab value(s) \cr
}

}}

\seealso{ \code{\link{getValues}, \link{setValues}, \link{extract}} }


\examples{
r <- raster(ncol=10, nrow=5)
r[] <- 1:ncell(r) * 2
getValues(r)
r[1]
r[1:10]
r[3:8] <- NA
r[1:10]

#equivalent to
getValues(r, format='matrix')
r[[1,1]]
r[[1:2,1:2]]
#first column
r[[,1]]
# first row
r[[1,]]
r[[]]

s <- stack(r,sqrt(r))
s[1:3]
}

\keyword{methods}
\keyword{spatial}
