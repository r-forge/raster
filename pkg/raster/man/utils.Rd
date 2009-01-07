\name{utilities}
\alias{compare}
\alias{roundCoords}
\alias{newCRS}

  
\title{ Compare rasters }
\description{
  compare evaluates whether a list of rasters has the same origin, resolution, number of rows and columns, and projection (or a subset thereof).
  roundCoords rounds the coordinates to a number of digits specified. 
  newCRS is a helper function to create a CRS map projection object.
}

\usage{
compare(rasters, origin=TRUE, resolution=TRUE, rowcol=TRUE, projection=TRUE, slack=0.01, stopiffalse=TRUE) 
roundCoords(object, digits=0)
newCRS(projstring)

}

\arguments{
  \item{rasters} { vector of RasterLayer objects}
  \item{origin} { logical. If \code{TRUE}, origins of rasters are compared}
  \item{resolution}{logical. If \code{TRUE}, resolution of rasters are compared }
  \item{rowcol}{logical. If \code{TRUE}, number of rows and columns of rasters are compared}
  \item{projection}{ logical. If \code{TRUE} projections are compared.}
  \item{slack} { fraction difference allowed in comparing origin and resolution }
  \item{stopiffalse}{logical. If \code{TRUE}, an error will be reported if rasters are not the same}
  \item{object} { a Raster* object }
  \item{digits} { integer indicating the precision to be used} 
  \item{projstring}{ a PROJ4 type character string describing projection and datum}
 }
  
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}
\examples{
	crsproj <- newCRS("+proj=longlat +datum=WGS84")
	r1 <- newRaster()
	r2 <- r1
	compare(c(r1, r2))
	r3 <- setRowCol(r1, 10)
#	compare(c(r1, r3))
	compare(c(r1, r3), stopiffalse=FALSE)
	compare(c(r1, r3), resolution=FALSE, rowcol=FALSE)
}
\keyword{ spatial }
