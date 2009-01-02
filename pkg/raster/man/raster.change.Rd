\name{change}
\alias{Aggregate}
\alias{disaggregate}
\alias{crop}
\alias{Merge}
\alias{expand}

\title{ Change raster database }
\description{
	Use existing raster(s) to create a new raster with different spatial extent or resolution
}
\usage{
Aggregate(raster, fact = 2, fun = mean, expand = TRUE, rm.NA = TRUE, ForceIntOutput = FALSE, filename="", overwrite=FALSE) 
disaggregate(raster, fact=2, filename="", overwrite=FALSE) 
crop(raster, boundingbox, filename="", overwrite=FALSE) 
expand(raster, boundingbox, filename="", overwrite=FALSE)
Merge(rasters, slack=0.01, filename="", overwrite=FALSE) 
}

\arguments{
  \item{raster}{ a RasterLayer object }
  \item{rasters}{ a list of RasterLayer objects }
  \item{boundingbox} {bounding box used to crop a raster. Any object that is or has a bounding box (see below) }
  \item{filename}{ output filename for a new raster }
  \item{fun}{ function used to Aggregate values. It should return one number. E.g. mean or max }
  \item{fact}{ degree of aggregation or disaggregation expressed as number of cells (horizontally and vertically). See details }
  \item{expand}{ if \code{expand == TRUE} grids will get larger if \code{factor} if the number of the division of the columns or rows with factor does not return an integer }
  \item{rm.NA}{ if \code{rm.NA == TRUE}, remove NA cells from calculations }
  \item{ForceIntOutput}{ logical. If \code{TRUE} the values will be rounded and stored as integer }
  \item{overwrite}{ if TRUE, "filename" will be overwritten if it exists }
  \item{slack}{ }
}

\details{
 Objects that have bounding boxes include RasterLayer, and other objects descending from the Spatial class in the sp package. 
 You can check this with boundingbox(object)
 They can be created with the function newBbox
 In aggregation \code{fact = 2} will result in a grid with 2 x 2 = 4 times fewer cells, while in disaggregation 4 times more cells will be created.
 In disaggregation fact can be a single integer or two integers c(x,y), in which case the first one is the horizontal disaggregation factor and y the vertical disaggreation factor
 
 
}

\value{
  a new raster object (in the R environment) which is in some cases written to disk 
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{
	rs <- newRaster()
	rs <- setValues(rs, runif(ncells(rs)))
#	map(rs)
	rsa <- Aggregate(rs, fact=10, fun=max)
#	map(rsa)
	bb1 <- newBbox(-180, -150, 60, 90)
	rsc1 <- crop(rsa, bb1)	
	bb2 <- newBbox(0, 50, 0, 90)
	rsc2 <- crop(rsa, bb2)	
	rsc <- Merge(c(rsc1, rsc2))
#	map(rsc)
#	rs provides the bounding box for expand
	rse <- expand(rsc, rs)
	rsd <- disaggregate(rse, 3)
}

\keyword{ spatial }

