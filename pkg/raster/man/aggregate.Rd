\name{aggregate}
\alias{aggregate,RasterLayer-method}
alias{aggregate}
\alias{disaggregate}

\title{Change raster database}
\description{
	(Dis)aggregate a raster to create a new raster 
}
\usage{
aggregate(raster, fact = 2, fun = mean, expand = TRUE, rm.NA = TRUE, filename="", overwrite=FALSE, asInt = FALSE) 
disaggregate(raster, fact=2, filename="", overwrite=FALSE) 
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{filename}{output filename for a new raster}
  \item{fun}{function used to aggregate values. It should return one number. E.g. mean or max}
  \item{fact}{degree of aggregation or disaggregation expressed as number of cells (horizontally and vertically). See details}
  \item{expand}{if \code{expand == TRUE} grids will get larger if \code{factor} if the number of the division of the columns or rows with factor does not return an integer}
  \item{rm.NA}{if \code{rm.NA == TRUE}, remove NA cells from calculations}
  \item{asInt}{logical. If \code{TRUE} the values will be rounded and stored as integer}
  \item{overwrite}{if TRUE, "filename" will be overwritten if it exists}
}

\details{
 In aggregation \code{fact = 2} will result in a grid with 2 x 2 = 4 times fewer cells, while in disaggregation 4 times more cells will be created.
 In disaggregation fact can be a single integer or two integers c(x,y), in which case the first one is the horizontal disaggregation factor and y the vertical disaggreation factor
 
 
}

\value{
  a new RasterLayer object (in the R environment) which is in some cases written to disk 
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
	r <- raster()
	r <- setValues(r, runif(ncell(r)))
	ra <- aggregate(r, fact=10, fun=max)
	rd <- disaggregate(ra, 3)
}

\keyword{spatial}

