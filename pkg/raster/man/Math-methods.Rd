\name{Math-methods}
\docType{methods}
\alias{Math-methods}
\alias{Math,RasterLayer-method}

\title{ Mathematical function for RasterLayer objects }
\description{
  The following standard mathematical functions can be used for computation with a single RasterLayer object
 "abs", "sign", "sqrt", "ceiling", "floor", "trunc", "cummax", "cummin", "cumprod", "cumsum", "log", "log10", "log2", "log1p", "acos", "acosh", "asin", "asinh", "atan", "atanh", "exp", "expm1", "cos", "cosh", "sin", "sinh", "tan", "tanh", "gamma", "lgamma", "digamma", "trigamma" 
}
\section{Note}{
 The RasterLayer object must have either all data in memory or on disk. If the data do not into memory you should use the calc() function instead.
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
r1 <- newRaster()
r1 <- (setValues(r1, runif(ncells(r1))) - 0.5 ) * 10
r2 <- abs(r1)
r3 <- sqrt(r1)
}

\keyword{methods}
\keyword{math)
}
