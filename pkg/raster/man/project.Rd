\name{projectRaster}

\alias{projectRaster}
\alias{projectBbox}

\title{project a RasterLayer}

\description{
Change the projection of a RasterLayer. 
}

\usage{
projectRaster(from, to, method="ngb", filename=NULL, filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1) 
projectBbox(object, projstring)
}

\arguments{
  \item{from}{a RasterLayer object}
  \item{to}{a Raster* object}
  \item{method}{method used to compute values for the new RasterLayer}
  \item{filename}{character. output filename}  
  \item{overwrite}{Logical. If \code{TRUE}, the output file is overwritten if it already exists}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{setDatatype}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}  
  \item{object}{BoundingBox or Raster* object}
  \item{projstring}{a character string describing a projection and datum in PROJ4 format}

}

\details{
First create a RasterLayer with a projected BoundingBox; also set the number of rows and columns. 
Then use that object to project the input RasterLayer to. 
  
Valid values for \code{method} are \code{'ngb'} (nearest neighbor) and \code{'bilinear'} (bilinear interpolation).

See \code{projInfo('proj')}, \code{projInfo('ellps')}, and \code{projInfo('datum')} for options for valid PROJ.4 strings. 
 
You can consult this page: \url{http://www.remotesensing.org/geotiff/proj_list/} to find the parameter options and names for projections.
 
Projection is performed using the PROJ.4 library accesed through the rgdal package. Also see \code{?CRS}
}

\value{
a RasterLayer object, and, in some cases, the side-effect of a raster file written to disk 
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[rgdal]{CRS-class}}, \code{\link[rgdal]{projInfo}}}

\examples{
# create a new (not projected) RasterLayer with cellnumbers as values
r <- raster(-110, -90, 40, 60, ncols=40, nrows=30)
r <- setValues(r, 1:ncell(r))
# proj.4 projection description
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"

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

