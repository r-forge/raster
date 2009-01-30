\name{project}
\alias{projectRaster}
\alias{projectBbox}
\alias{makeProj}

\title{Change the projection of a RasterLayer}
\description{
	Change the projection of a RasterLayer. 
}
\usage{
projectRaster(from, to, method="nngb", overwrite=FALSE)
projectBbox(object, projstring)
makeProj(projection='longlat', ..., ellipsoid="", datum="", asText=TRUE) 
}

\arguments{
  \item{from}{a RasterLayer object}
  \item{to}{a BasicRaster object (or any Raster* object}
  \item{method}{}
  \item{overwrite}{}
  \item{object}{}
  \item{projstring}{}
  \item{projection}{}
  \item{...}{parameters for the projection}  
  \item{ellipsoid}{}
  \item{datum}{}  
  \item{asText}{logical. If \code{FALSE}, an object of class 'CRS' will be returned.}  
}

\details{
 First create projected bbox (in BasicRaster object). Then project. 
 
 makeProj is a helper function to create a valid PROJ.4 string or CRS object. 
 
 See projInfo('proj'), projInfo('ellps'), and projInfo('datum') for valud choices. Specify either an ellipsoid, or a datum (datum is ignored if ellipsoid != "")
 
 Many projections take additioanl paramters to the projection name (see the example below). Supply these after the projection argument. 
 
 You can consult this webpage: \url{http://www.remotesensing.org/geotiff/proj_list/} to find the parameter options and names for your projection.
 
 Projection is performed using the PROJ.4 library accesed through the rgdal package.  Also see ?CRS
}

\value{
  a new RasterLayer object (in the R environment), with, in some cases, the side-effect of the raster values written to disk 
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
# create a new (not projected) RasterLayer with cellnumbers as values
r <- raster(-110, -90, 40, 60, ncols=40, nrows=30)
r <- setValues(r, 1:ncell(r))
# proj.4 projection description
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100"
# this is equivalent to
newproj2 <- makeProj("lcc", "lat_1=48", "lat_2=33", "lon_0=-100", ellp="WGS84", datum="")

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

