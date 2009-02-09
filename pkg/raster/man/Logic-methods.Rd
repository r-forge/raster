\name{Logic-methods}
\docType{methods}
\alias{Logic-methods}
\alias{Logic,RasterLayer,RasterLayer-method}
\alias{is.na,RasterLayer-method}
\alias{is.nan,RasterLayer-method}
\alias{is.finite,RasterLayer-method}
\alias{is.infinite,RasterLayer-method}
\alias{!,RasterLayer-method}


\title{ Methods for logical functions for RasterLayers' }
\description{
The following boolean operators are available: '&', '|', and '!'
With r being a RasterLayer, and the result a RasterLayer with logical values:
	r <- r < 3 | r < 6
	r <- !r
These are also available :
	is.na(r) 
	is.nan(r)
	is.finite(r)
	is.infinite(object)
}

\keyword{methods}
\keyword{math}
