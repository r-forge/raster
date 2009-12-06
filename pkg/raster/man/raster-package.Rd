\name{raster-package}
\alias{raster-package}

\docType{package}

\title{
Management, analysis and modeling of raster data
}

\description{
This packages provides classes and functions to manipulate geographic (spatial) data in 'raster' form.
Raster data divides the world in cells (rectangles; pixels) of equal size (in units of the projection). 
Such data are also referred to as 'grid' data.

The package should be particularly useful when using very large datasets that can not be loaded into the computer's memory. Functions will
work correctly, because they will read, process, and write blocks of data, without loading all values into memory. 

See the vignette for more information: \code{vignette('raster', 'raster')}
}

\details{
\tabular{ll}{
Package: \tab raster\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2009-12-05\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}

}
\author{
Robert J. Hijmans and Jacob van Etten

Maintainer: Robert J. Hijmans \email{r.hijmans@gmail.com}
}
\keyword{package}
\keyword{spatial}
