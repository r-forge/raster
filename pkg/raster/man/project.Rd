\name{project}
\alias{projectRaster}
\alias{projectBbox}

\title{Change the projection of a RasterLayer}
\description{
	Change the projection of a RasterLayer
}
\usage{
projectRaster(from, to, method="nngb", overwrite=FALSE)
projectBbox(object, projstring)
}

\arguments{
  \item{from}{a RasterLayer object}
  \item{to}{a BasicRaster object (or any Raster* object}
  \item{method}{}
  \item{overwrite}{}
  \item{object}{}
  \item{projstring}{}
}

\details{
 first create projected bbox (in BasicRaster object). Then project. 
}

\value{
  a new RasterLayer object (in the R environment), with, in some cases, the side-effect of the raster values written to disk 
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
# create a new (not projected) RasterLayer with cellnumbers as values
r <- newRaster(-110, -90, 40, 60, ncols=40, nrows=30)
r <- setValues(r, 1:ncell(r))
# proj.4 projection description
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100"
# create a new RasterLayer with a BoundingBox with the new projection
projras <- projectBbox(r, newproj)
# Adjust the cell size 
projras <- setRes(projras, 100000)
# project the values of RasterLayer 'r' to the new RasterLayer 'projras'
projras <- projectRaster(r, projras)
# inverse projection, back to the properties of 'r'
inverse <- projectRaster(projras, r)

}

\keyword{spatial}

