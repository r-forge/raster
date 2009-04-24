\name{compare}

\alias{compare}
  
\title{ Compare RasterLayer objects }

\description{
  compare evaluates whether a list of rasters has the same origin, resolution, number of rows and columns, and projection (or a subset of these comparisons).
}

\usage{
compare(objects, bb=TRUE, rowcol=TRUE, prj=TRUE, res=FALSE, orig=FALSE, tolerance=0.05, stopiffalse=TRUE, showwarning=FALSE) 
}

\arguments{
  \item{objects}{vector or list of Raster* objects }
  \item{bb}{logical. If \code{TRUE}, bounding boxes are compared}
  \item{rowcol}{logical. If \code{TRUE}, number of rows and columns of the objects are compared}
  \item{prj}{logical. If \code{TRUE} projections are compared.}
  \item{res}{logical. If \code{TRUE}, resolutions are compared}
  \item{orig}{logical. If \code{TRUE}, origins are compared}
  \item{tolerance}{difference permissable (relative to the cell resolution) for objects to be 'equal', for non integer numbers in origin and resolution. See ?all.equal }
  \item{stopiffalse}{logical. If \code{TRUE}, an error will occur if the objects are not the same}
  \item{showwarning}{logical. If \code{TRUE}, an warning will be given if objects are not the same. Only relevant when \code{stopiffalse} is \code{TRUE}}
 }
  
\author{Robert J. Hijmans}

\examples{
	r1 <- raster()
	r2 <- r1
	compare(c(r1, r2))
	r3 <- r1
	nrow(r3) <- 10
#	compare(c(r1, r3))
	compare(c(r1, r3), stopiffalse=FALSE)
	compare(c(r1, r3), rowcol=FALSE)
}

\keyword{ spatial }

