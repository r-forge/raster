\name{disaggregate}
\alias{disaggregate}

\title{Disaggregate}
\description{
	Disaggregate a raster to create a new raster 
}
\usage{
disaggregate(raster, fact=2, filename="", overwrite=FALSE) 
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{filename}{output filename for a new raster}
  \item{fact}{degree of aggregation or disaggregation expressed as number of cells (horizontally and vertically). See details}
  \item{overwrite}{if TRUE, "filename" will be overwritten if it exists}
}

\details{
 Fact can be a single integer or two integers c(x,y), in which case the first one is the horizontal disaggregation factor and y the vertical disaggreation factor
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
	r <- raster(ncols=10, nrows=10)
	rd <- disaggregate(r, fact=c(10, 5))
	ncol(rd)
	nrow(rd)	
}

\keyword{spatial}

