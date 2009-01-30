\name{change extent}
\alias{crop}
\alias{Merge}
\alias{expand}

\title{Change raster database}
\description{
	Use existing raster(s) to create a new raster with different spatial extent or resolution
}
\usage{
crop(raster, bndbox, filename="", overwrite=FALSE) 
expand(raster, bndbox, filename="", overwrite=FALSE)
Merge(rasters, tolerance=0.05, filename="", overwrite=FALSE) 
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{rasters}{a list of RasterLayer objects}
  \item{bndbox}{bounding box used to crop a raster. Any object that is or has a bounding box (see below)}
  \item{filename}{output filename for a new raster}
  \item{overwrite}{if TRUE, "filename" will be overwritten if it exists}
  \item{tolerance}{difference permissable (relative to the cell resolution) for objects to be 'equal'. See ?all.equal}
}

\details{
 Objects that have bounding boxes include RasterLayer, and other objects descending from the Spatial class in the sp package. 
 You can check this with getBbox(object)
 They can be created with the function newBbox
 
}

\value{
  a new RasterLayer object (in the R environment) which is in some cases written to disk 
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
	rs <- raster()
	rs <- setValues(rs, runif(ncell(rs)))
#	map(rsa)
	bb1 <- newBbox(-180, -150, 60, 90)
	rsc1 <- crop(rs, bb1)	
	bb2 <- newBbox(0, 50, 0, 90)
	rsc2 <- crop(rs, bb2)	
	rsc <- Merge(c(rsc1, rsc2))
#	map(rsc)
#	rs provides the bounding box for expand
	rse <- expand(rsc, rs)
}

\keyword{spatial}

