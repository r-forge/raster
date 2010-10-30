\name{projectRaster}

\alias{projectRaster}
\alias{projectExtent}

\title{Project a Raster object}

\description{
Project the values of a Raster* object to a new Raster* object with another coordinate reference system ('projection'). 

\code{projecExtent} returns a RasterLayer with a projected extent, but without any values. This RasterLayer can then 
be adjusted (e.g. by setting its resolution) and used as a template \code{'to'} Raster object in \code{projectRaster}. 
\code{projectRaster} computes values for the cells of the new Raster object.
}

\usage{
projectRaster(from, to, res, crs, method="bilinear", filename="", ...) 
projectExtent(object, crs)
}

\arguments{
  \item{from}{Raster* object}
  \item{to}{Raster* object with the parameters to which 'from' should be projected}
  \item{res}{Single or (vector of) two numerics. To, optionally, set the output resolution if 'to' is missing}
  \item{crs}{Character or object of class 'CRS'. PROJ.4 description of the coordinate reference system. In projectRaster this is used to set the output CRS if 'to' is missing, or if 'to' has no valid CRS}
  \item{method}{Method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor), which is useful for categorical variables, or (the default value) 'bilinear' (bilinear interpolation), which is appropriate for continuous variables.}
  \item{filename}{Character. output filename}  
  \item{...}{Additional arguments. See Details.}    
  \item{object}{Raster* object}

}

\details{
There are two approaches you can follow to project the values of a Raster object.
1) First create a template Raster with the CRS you want to project to. To do so, you can use \code{projectExtent} for this
or an existing Raster* object. Also set the number of rows and columns (or the resolution), and perhaps adjust the extent. The resolution of the output raster should normally be similar to that of the input raster. Then use that object to project the input Raster object to. 
This is the preferred method because you have most control. For example you can assure that the resulting Raster object lines up with other Raster objects.

2) Do not provide a "to" argument, but provide a "crs" argument, and, optionally, a 'res' argument.

 

Projection is performed using the PROJ.4 library accesed through the rgdal package. 

One of the best places to find PROJ.4 coordinate reference system descriptions is \url{http://www.spatialreference.org}. 

You can also consult this page: \url{http://www.remotesensing.org/geotiff/proj_list/} to find the parameter options and names for projections.

Also see \code{projInfo('proj')}, \code{projInfo('ellps')}, and \code{projInfo('datum')} for valid PROJ.4 values. 

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\note{
Vector (points, lines, polygons) can be transformed with \code{\link[rgdal]{spTransform}}.
}

\value{
A RasterLayer or RasterBrick object, and, in some cases, the side-effect of a raster file written to disk.
}

\author{Robert J. Hijmans}

\seealso{ \code{resample} \code{\link[rgdal]{CRS-class}}, \code{\link[rgdal]{projInfo}}}


\examples{
# create a new (not projected) RasterLayer with cellnumbers as values
r <- raster(xmn=-110, xmx=-90, ymn=40, ymx=60, ncols=40, nrows=40)
r <- setValues(r, 1:ncell(r))
# proj.4 projection description
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"

# create a new RasterLayer with a projected extent (no values are transferred)
if (require(rgdal)) {
pr <- projectExtent(r, newproj)
# Adjust the cell size 
res(pr) <- 200000

# project the values of RasterLayer 'r' to the new RasterLayer 'projras'
pr <- projectRaster(r, pr)
# alternatively
pr2 <- projectRaster(r, crs=newproj, res=20000)
# or
# pr3 <- projectRaster(r, crs=newproj)

# inverse projection, back to the properties of 'r'
inv <- projectRaster(pr, r)

## using a higher resolution
\dontrun{
res(pr) <- 10000
pr <- projectRaster(r, pr, method='bilinear')
inv <- projectRaster(pr, r, method='bilinear')
dif <- r - inv
plot(dif)

# Meuse data to lon/lat
meuse <- raster(system.file("external/test.grd", package="raster"))
meusell <- projectRaster(meuse, res=0.0008333333, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
}

}
}

\keyword{spatial}

