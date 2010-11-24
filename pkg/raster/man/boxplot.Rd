\name{boxplot}

\docType{methods}

\alias{boxplot}
\alias{boxplot,Raster-method}

\title{
Box plot or Raster objects
}

\description{
Box plots of layers in a Raster objects
}


\usage{
boxplot(x, ...)
}

\arguments{
\item{x}{Raster* object}
\item{...}{Additional arguments. See Methods}
}


\section{Methods}{
\describe{
	\code{boxplot(x, maxpixels=100000, ...)}

\tabular{rll}{
\tab \code{maxpixels} \tab Integer. Number of pixels to sample from each layer of large Raster objects \cr
\tab \code{...} \tab Argument passed to \code{graphics::\link{boxplot}} \cr
}		
}
}

\seealso{
\code{\link{boxplot}}
}

\examples{
r1 <- r2 <- r3 <- raster(ncol=10, nrow=10)
r1[] <- rnorm(ncell(r1), 100, 50)
r2[] <- rnorm(ncell(r1), 80, 20)
r3[] <- rnorm(ncell(r1), 120, 30)
s <- stack(r1, r2, r3)

boxplot(s, notch=TRUE)
}

\author{
Robert J. Hijmans
}

\keyword{spatial}

