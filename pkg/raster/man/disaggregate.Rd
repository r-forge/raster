\name{disaggregate}
\alias{disaggregate}

\title{Disaggregate}
\description{
Disaggregate a raster to create a new raster 
}
\usage{
disaggregate(raster, fact=2, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1) 
}

\arguments{
  \item{raster}{a RasterLayer object}
  \item{filename}{output filename for a new raster}
  \item{fact}{degree of aggregation or disaggregation expressed as number of cells (horizontally and vertically). See details}
  \item{overwrite}{if TRUE, "filename" will be overwritten if it exists}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{setDatatype}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}   
}

\details{
Fact can be a single integer or two integers c(x,y), in which case the first one is the horizontal disaggregation factor and y the vertical disaggreation factor. If a single integer values is supplied, cells are disaggregated with the same factor in x and y direction.
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=10, nrows=10)
rd <- disaggregate(r, fact=c(10, 5))
ncol(rd)
nrow(rd)
}

\keyword{spatial}

