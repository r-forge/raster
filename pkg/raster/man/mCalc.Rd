\name{mCalc}
\alias{mCalc}

\title{ multiple layers calculate }
\description{
  Calculate values for a new RasterLayer from the layers in a RasterStack
}

\usage{
mCalc(object, fun=sum, filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1)
}

\arguments{
  \item{object}{ a RasterStack object }
  \item{fun}{ the function to be applied }
  \item{filename}{ output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot }
  \item{overwrite}{ logical to indicate whether an existing output file should be overwritten }
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{setDatatype}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed. }
}

\details{
	\code{fun} should be a function that takes a one dimensional vector of value as input, and returns a single value.
}

\value{
	a new RasterLayer 
}
\author{ Robert J. Hijmans}

\examples{


}
\keyword{ spatial }
