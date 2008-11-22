\name{raster.utilities}
\alias{compare}
\alias{new.CRS}
\alias{new.boundingbox}

  
\title{ Compare rasters }
\description{
  compare evaluates whether a list of rasters has the same origin, resolution, number of rows and columns, and projection (or a subset thereof).
  new.CRS is a helper function to create a CRS map projection object.
  new.boundingbox creates a new bounding box (a "Spatial" object) that can be used to....
}

\usage{
compare(rasters, origin=TRUE, resolution=TRUE, rowcol=TRUE, projection=TRUE, slack=0.01, stopiffalse=TRUE) 
new.CRS(projection)
new.boundingbox(xmn, xmx, ymn, ymx, projection="")

}

\arguments{
  \item{rasters} {}
  \item{origin} {}
  \item{resolution}{}
  \item{rowcol}{}
  \item{projection}{ logical in raster.compare. If \code{TRUE} rasters projections are compared. In create.CRS it is a proj4 type string. }
  \item{slack} {}
  \item{stopiffalse}{}
  
  \item{xmn}{ the minimum x coordinate of the bounding box }
  \item{xmx}{ the maximum x coordinate of the bounding box }
  \item{ymn}{ the minimum y coordinate of the bounding box }
  \item{ymx}{ the maximum y coordinate of the bounding box }
 }
  
\value{
}

\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}
\examples{
}
\keyword{ spatial }
