\name{round}
\docType{methods}
\alias{round,RasterLayer-method}
\alias{trunc,RasterLayer-method}
\alias{ceiling,RasterLayer-method}
\alias{floor,RasterLayer-method}


\title{ Functions to create integer values for a RasterLayer }

\description{
These functions take a single RasterLayer argument \code{x}

ceiling returns a RasterLayer with the smallest integers not less than the corresponding values of x. 

floor returns a RasterLayer with the largest integers not greater than the corresponding values of x. 

trunc returns a RasterLayer with the integers formed by truncating the values in x toward 0. 

round returns a RasterLayer with values rounded to the specified number of digits (decimal places; default 0). 
}


\section{Methods}{
\describe{
ceiling(x)
floor(x)
trunc(x, ...)
round(x, digits = 0)


\item{x}{a RasterLayer object}
\item{digits}{integer indicating the precision to be used}
\item{...}{arguments to be passed to methods}
}
}


\details{
see ?base::round

}
\value{
a RasterLayer object
}
\author{Robert J.Hijmans}
\examples{
r <- raster(ncol=10, nrow=10)
r[] <- runif(ncell(r)) * 10
s <- round(r)
}
\keyword{spatial}
