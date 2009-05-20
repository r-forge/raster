\name{predict}

\docType{methods}

\alias{predict}
\alias{predict,RasterStack-method}

\title{Predict}

\description{
 Predict values for a fitted model object and a RasterStack as independent variables
}

\usage{
predict(object, ...) 
}


\arguments{
  \item{object}{a RasterStack object}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
 
\code{predict(object, ...,  model, filename='', overwrite=FALSE, filetype='raster', datatype='FLT4S')}
  
\tabular{rll}{  
  \tab  \code{object} \tab a RasterStack object \cr
  \tab  \code{model}  \tab A fitted model\cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab  \code{overwrite}  \tab Logical to indicate whether an existing output file should be overwritten \cr
  \tab  \code{filetype}  \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab  \code{datatype}  \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab  \code{track}  \tab vector of row numbers for which the function will report that they have been processed \cr
}

}}

\details{
}

\value{
a RasterLayer object
}

\author{ Robert J. Hijmans}

\examples{
}

\keyword{methods}
\keyword{spatial}
