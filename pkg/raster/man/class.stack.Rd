\name{RasterStack-class}
\docType{class}
\alias{RasterStack-class}
\alias{show,RasterStack-method}

\title{Class "RasterStack" }
\description{	Class for handling "Stacks" of rasters.  }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RasterStack", ...)}.
}

\section{Slots}{
	 \describe{
    \item{\code{filename}:}{Object of class \code{"character"} filename of the stack (if saved to disk) }
    \item{\code{nlayers}:}{Object of class \code{"integer"} number of RasterLayer objects in the stack }
    \item{\code{rasters}:}{Object of class \code{"list"} list of the RasterLayer objects }
    \item{\code{projection}:}{Object of class \code{"character"} proj4 projection string }
    \item{\code{ncols}:}{Object of class \code{"integer"} number of columns of the rasters }
    \item{\code{nrows}:}{Object of class \code{"integer"} number of rows of the rasters }
    \item{\code{ncells}:}{Object of class \code{"integer"} number of cells of the rasters }
    \item{\code{xmin}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xmax}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ymin}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ymax}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xres}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{yres}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{data}:}{Object of class \code{"matrix"} slot to hold the data values (after read ) }	
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "RasterStack")}: ... }
	 }
}
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}

\seealso{ \code{\link[Raster]{Raster}} }

\examples{
showClass("RasterStack")
}
\keyword{classes}
\keyword{ spatial }
