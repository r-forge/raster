\name{Extent-class}

\docType{class}

\alias{Extent-class}
\alias{show,Extent-method}

\title{Class "Extent" }

\description{Object of class Extent are used to define the geographic extenmt (extremes) of a Raster* object}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Extent", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{xmin}:}{minimum x coordinate}
    \item{\code{xmax}:}{maximum x coordinate}
    \item{\code{ymin}:}{minumum y coordinate}
    \item{\code{ymax}:}{maximum y coordinate}
  }
}
\section{Methods}{
  \describe{
 \item{show}{\code{signature(object = "Extent")}: display values of a Extent object }
 }
}
\author{ Robert J. Hijmans}


\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{setExtent}} }

\examples{
showClass("Extent")
}

\keyword{classes}
\keyword{spatial}
