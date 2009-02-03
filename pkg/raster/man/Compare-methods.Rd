\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{==,BasicRaster,BasicRaster-method}
\alias{!=,BasicRaster,BasicRaster-method}

\title{ Methods to compare Raster* objects}
\description{
 These methods compare the location and resolution of Raster* objects. That is, they compare their bounding boxes, projection, and rows and columns. 
 For BasicRaster objects you can use "==" and "!=", the values returned is a single logical value \code{TRUE} or \code{FALSE}
 For RasterLayer objects, they also compare the values associated with the objects, and the result is a logical (Boolean) RasterLayer object.
 The following methods have been implemented for RasterLayer objects: "==", "!=", ">", "<",  "<=", ">="
}

\author{Robert J. Hijmans }


\examples{
r1 <- raster()
r1 <- setValues(r1, round(10 * runif(ncell(r1))))
r2 <- setValues(r1, round(10 * runif(ncell(r1))))
as(r1, "BasicRaster") == as(r2, "BasicRaster")
r3 <- r1 > r2

b <- newBbox(0, 360, 0, 180)
r4 <- setBbox(r2, b)
as(r2, "BasicRaster") != as(r4, "BasicRaster")
# The following would give an error. You cannot compare RasterLayer object that do not have the same BasicRaster properties.
#r3 <- r1 > r4
}


\keyword{methods}
\keyword{math}
