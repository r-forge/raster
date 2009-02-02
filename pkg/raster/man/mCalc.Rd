\name{mCalc}
\alias{mCalc}

\title{ multiple layers calculate }
\description{
  Calculate values for a new RasterLayer from the layers in a RasterStack
}

\usage{
mCalc(object, fun=sum, filename="", overwrite=FALSE, asInt=FALSE)
}

\arguments{
  \item{object}{ a RasterStack object }
  \item{fun}{ the function to be applied }
  \item{filename}{ output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot }
  \item{overwrite}{ logical to indicate whether an existing output file should be overwritten }
  \item{asInt}{ logical. If \code{TRUE}, output will be integer values }
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
