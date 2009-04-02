\name{aggregate}

\docType{methods}

\alias{aggregate}
\alias{aggregate,RasterLayer-method}

\title{aggregate}

\description{
Aggregate a RasterLayer to create a new RasterLayer with a lower resolution (larger cells). 
Aggregation groups rectangular areas of RasterLayer cells to create a new RasterLayer with larger cells. 
A new value is computed for the resulting cells according to a specified function (default value = \code{mean}). 
}

\usage{
aggregate(x, ...) 
}


\arguments{
  \item{x}{A RasterLayer object}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
A full call to the aggregate method for a RasterLayer is:

\code{aggregate(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename=NULL, filetype='raster', datatype='FLT4S', overwrite=FALSE, track=-1) }
 
 
\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{fun} \tab function used to aggregate values \cr
\tab \code{fact} \tab integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertial aggregation fcator). See details \cr
\tab \code{expand} \tab logical. If \code{TRUE} the output RasterLayer will get larger if a division of the number of columns or rows with \code{factor} does not return an integer \cr
\tab \code{na.rm} \tab logical. If \code{TRUE}, remove NA cells from calculations \cr
\tab \code{filename} \tab character. output filename \cr
\tab \code{overwrite} \tab logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
\tab \code{filetype} \tab character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{rasterFormats}} \cr
\tab \code{datatype} \tab character. output data type; see \code{\link[raster]{setDatatype}} \cr
\tab \code{track} \tab vector of row numbers for which the function will report that they have been processed \cr
}

}
}

\details{
 Aggregation will result in a RasterLayer with \code{fact*fact} fewer cells; sometimes adjusted according to the values of \code{expand}.
 For example, \code{fact=2} will result in a new RasterLayer with \code{2*2=4} times fewer cells. 
 If two numbers are supplied, e.g., \code{fact=c(2,3)}, the first will be used for aggregating in the horizontal direction, 
 and the second for aggregating in the vertical direction, and the new RasterLayer will have \code{2*3=6} times fewer cells.  
 
 Aggregation starts at the upper-left end of a raster. If a division of the number of columns or rows with \code{factor} does not 
 return an integer, the extent of the resulting RasterLayer will either be somewhat smaller or somewhat larger then the original RasterLayer.
 For example, the input RasterLayer has 100 columns, and \code{fact=12}, the output RasterLayer will have either 8 columns (\code{expand=FALSE}) 
 (using \code{8 x 12 = 96} of the original columns) or 9 columns (\code{expand=TRUE}). The maximum x coordinate of the output RasterLayer will, 
 of course, also be adjusted.
  
 The function \code{fun} should take multiple numbers, and return a single number. For example \code{mean}, \code{modal}, \code{min} or \code{max}. 
 
 If no filename is specified, and the resulting RasterLayer is too large to hold in memory, it is saved to a temporary file.  
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\seealso{ \code{\link[raster]{disaggregate}} }

\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r <- raster()
# a new aggregated raster, no values
ra <- aggregate(r, fact=10)
r <- setValues(r, runif(ncell(r)))
ra <- aggregate(r, fact=10, fun=max)
# a new aggregated raster, max of the values
}

\keyword{methods}
\keyword{spatial}

