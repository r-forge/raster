\name{RasterLayer-class}
\docType{class}
\alias{RasterLayer-class}
\alias{show,RasterLayer-method}

\title{Class "RasterLayer" }
\description{	Class for handling RasterLayer (grid) data  }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RasterLayer", ...)}.
	 }
\section{Slots}{
	 \describe{
	\item{\code{title}:}{Object of class \code{"character"}. Short name for raster } 
	\item{\code{file}:}{ Object of class \code{"RasterFile"}} 
	\item{\code{data}:}{ Object of class \code{"RasterData"}} 
	\item{\code{history}:}{Object of class \code{"character"}} 
    \item{\code{ncols}:}{Object of class \code{"integer"} number of columns }
    \item{\code{nrows}:}{Object of class \code{"integer"} number of rows }
    \item{\code{bbox}:}{Object of class \code{"maxtrix"} min and max x and y coordiantes }
    \item{\code{proj4string}:}{Object of class \code{"CRS"} }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "RasterLayer")}: ... }
    \item{hist}{\code{signature(object = "RasterLayer")}: ... }
	
 	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\note{  }

\examples{
showClass("RasterLayer")
}
\keyword{classes}
\keyword{spatial}
