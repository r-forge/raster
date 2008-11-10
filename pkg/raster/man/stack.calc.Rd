\name{rasterstack.calc}
\alias{s.calc}
\title{ Cell based calculations for a stack }
\description{
  Cell based calculations for a stack (a collection of 2 or more rasters of the same extent and resolution), resulting in a single raster. 
  Any function that takes a one dimensional vector of value as input, and returns a single value, can be passed as function.
}
\usage{
s.calc(rstack, fun, filename="", overwrite=FALSE, ForceIntOutput = FALSE)
}
\arguments{
  \item{rstack}{ a \code{RasterStack} object }
  \item{fun}{ \code{fun} function to be applied }
  \item{filename}{ \code{filename} output raster filename. If NA, no file is written and result return in the data slot of the raster object }
  \item{overwrite}{ if \code{overwrite==TRUE} the output file will be overwritten if it exists}
  \item{ForceIntOutput}{ if \code{ForceIntOutput==TRUE} the output will always be rounded to integer values}
}

\value{the function returns a raster object and (if \code{filename != NA}) a raster file on disk}

\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}

\seealso{ \code{\link{r.calc}} is a simular function for Raster objects }
\examples{

}
\keyword{ spatial }
