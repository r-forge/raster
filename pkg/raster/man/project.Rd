\name{project}
\alias{projectRaster}
\alias{projectBbox}

\title{Change the projection of a RasterLayer}
\description{
	Change the projection of a RasterLayer. 
}
\usage{
projectRaster(from, to, method="nngb", overwrite=FALSE)
projectBbox(object, projstring)
}

\arguments{
  \item{from}{a RasterLayer object}
  \item{to}{a BasicRaster object (or any Raster* object}
  \item{method}{character. Only 'nngb', or neirest neighbor, has been implmented sofar}
  \item{overwrite}{if \code{TRUE}, the file will be overwritten if it exists}
  \item{object}{BoundingBox or Raster* object}
  \item{projstring}{a character string describing a projection and datum in PROJ4 format }
}

\details{
 First create projected bbox (in BasicRaster object). Then project. 
  
 See \code{projInfo('proj')}, \code{projInfo('ellps')}, and \code{projInfo('datum')} for options for valid PROJ.4 strings. 
 
 You can consult this page: \url{http://www.remotesensing.org/geotiff/proj_list/} to find the parameter options and names for projections.
 
 Projection is performed using the PROJ.4 library accesed through the rgdal package. Also see \code{?CRS}
}

\value{
  a new RasterLayer object (in the R environment), with, in some cases, the side-effect of a raster file written to disk 
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[rgdal]{CRS}}, \code{\link[rgdal]{projInfo}}}

\examples{
# create a new (not projected) RasterLayer with cellnumbers as values
r <- raster(-110, -90, 40, 60, ncols=40, nrows=30)
r <- setValues(r, 1:ncell(r))
# proj.4 projection description
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100"
# for debugging R-Forge...
 drop(read.dcf(file=system.file("DESCRIPTION", package="rgdal"), fields=c("Version","Date")))

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

