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
compare(objects, bb=TRUE, rowcol=TRUE, prj=TRUE, res=FALSE, orig=FALSE, tolerance=0.05, stopiffalse=TRUE, showwarning=FALSE) 
roundCoords(object, digits=0)
newCRS(projstring)
}

\arguments{
  \item{objects} { vector or list of Raster* objects }
  \item{bb} { logical. If \code{TRUE}, bounding boxes are compared}
  \item{rowcol}{logical. If \code{TRUE}, number of rows and columns of the objects are compared}
  \item{prj}{ logical. If \code{TRUE} projections are compared.}
  \item{res} { logical. If \code{TRUE}, resolutions are compared}
  \item{orig} { logical. If \code{TRUE}, origins are compared}
  \item{tolerance} { difference permissable (relative to the cell resolution) for objects to be 'equal', for non integer numbers in origin and resolution. See ?all.equal }
  \item{stopiffalse}{logical. If \code{TRUE}, an error will occur if the objects are not the same}
  \item{showwarning}{logical. If \code{TRUE}, an warning will be given if objects are not the same. Only relevant when \code{stopiffalse} is \code{TRUE}}
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
	compare(c(r1, r3), rowcol=FALSE)
}
\keyword{ spatial }

