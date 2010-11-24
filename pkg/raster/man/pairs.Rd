\name{pairs}

\docType{methods}

\alias{pairs}
\alias{pairs,RasterStackBrick-method}

\title{
Pair plots
}

\description{
Pair plots of layers in a RasterStack or RasterBrick
}


\usage{
pairs(x, ...)
}

\arguments{
\item{x}{RasterBrick or RasterStack}
\item{...}{Additional arguments. See Methods}
}


\section{Methods}{
\describe{
	\code{pairs(x, hist=TRUE, cor=TRUE, maxpixels=100000)}

\tabular{rll}{
\tab \code{hist} \tab Logical. If \code{TRUE} a histogram of the values is shown on the diagonal\cr
\tab \code{cor} \tab Logical. If \code{TRUE} the correlation coefficient is shown in the upper panels \cr
\tab \code{maxpixels} \tab Integer. Number of pixels to sample from each layer of large Raster objects \cr
}		
}
}

\seealso{
\code{\link{boxplot}}
}

\examples{
r <- raster(system.file("external/test.grd", package="raster") )
s <- stack(r, 1/r, sqrt(r))
pairs(s)
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

