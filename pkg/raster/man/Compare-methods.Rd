\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{==,Raster,Raster-method}
\alias{!=,Raster,Raster-method}

\title{ Methods to compare Raster* objects}
\description{
 These methods compare the location and resolution of Raster*(i.e. their bounding boxes, projection, and rows and columns. They do not compare the values associated with the objects; nor if they are of the same class (but they must be decendants from Raster' 
 The following methods have been implemented: '==' and '!='
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}


\examples{
r1 <- newRaster()
r2 <- r1
r2 == r1
b <- newBbox(0, 360, 0, 180)
r3 <- setBbox(r2, b)
r2 != r3
}


\keyword{methods}
\keyword{math}
