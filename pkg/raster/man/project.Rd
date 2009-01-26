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
r <- newRaster(-120, -80, 30, 60, ncols=40, nrows=30)
r <- setValues(r, 1:ncell(r))
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100"
bb <- projectBbox(r, newproj)
bb <- setRes(bb, 100000)
rp <- projectRaster(r, bb)
}

\keyword{spatial}

