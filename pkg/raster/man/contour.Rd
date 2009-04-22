\name{contour}

\docType{methods}

\alias{contour,RasterLayer-method}

\title{Contour plot of a RasterLayer}

\description{
Contour plot of a RasterLayer.
}

\section{Methods}{
\describe{

\code{contour(x, ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[graphics]{contour}} (graphics package) \cr
}
}}


\details{

}


\author{ Robert J. Hijmans }

\examples{ 
r <- raster(nrows=36, ncols=18)
r <- setValues(r, runif(ncell(r)))
plot(r)
}

\keyword{methods}
\keyword{spatial}

