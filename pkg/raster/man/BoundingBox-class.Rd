\name{BoundingBox-class}
\docType{class}
\alias{BoundingBox-class}
\alias{show,BoundingBox-method}

\title{Class "BoundingBox" }
\description{ The BoundingBox class described the geographic extremes of a Raster* object}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BoundingBox", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{xmin}:}{minumum x coordinate}
    \item{\code{xmax}:}{maximum x coordinate}
    \item{\code{ymin}:}{minumum y coordinate}
    \item{\code{ymax}:}{maximum y coordinate}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "BoundingBox")}: display values of a BoundingBox object }
	 }
}
\author{ Robert J. Hijmans}


\seealso{ \code{\link[raster]{getBoundingBox}}, \code{\link[raster]{setBoundingBox}} }

\examples{
showClass("BoundingBox")
}
\keyword{classes}
\keyword{spatial}
