\name{corr}

\docType{methods}

\alias{corr}
\alias{corr,RasterLayer,RasterLayer-method}
\alias{corr,RasterStackBrick,missing-method}
\alias{corrLocal}
\alias{corrLocal,RasterLayer,RasterLayer-method}


\title{Pearson correlation coefficient}

\description{
Pearson correlation coefficient between cell values of two RasterLayers or all layers in a RasterStack or RasterBrick
}

\usage{
\S4method{corr}{RasterLayer,RasterLayer}(x, y, n=Inf, ...)

\S4method{corr}{RasterStackBrick,missing}(x, y, n=Inf, ...)

\S4method{corrLocal}{RasterLayer,RasterLayer}(x, y, ngb=5, method = c("pearson", "kendall", "spearman"), filename='', ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{RasterLayer object if \code{x} is also a RasterLayer object; otherwise missing}
  \item{n}{positive integer. Maximum sample size. If \code{n < ncell(x)}, \code{\link{sampleRegular}} is used for sampling}
  \item{ngb}{Neighborhood size. Either a single integer or a vector of two integers c(nrow, ncol)}
  \item{method}{character indicating which correlation coefficient is to be used. One of \code{"pearson"}, \code{"kendall"}, or \code{"spearman"}}
  \item{filename}{character. Output filename (optional)}
  \item{...}{additional arguments. None implemented for corr. For corrLocal they are as for \code{\link{writeRaster}}}
}

\note{
This function will attempt to compute the correlation coefficient on all cell values. This is rarely useful for very large rasters as a reasonably large sample would suffice. 

Cell values that are \code{NA} in any of the layers are removed from the computation.
}

\value{
numeric
}

\seealso{ \code{\link{cor}} }


\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))
corr(b)
 
# for more options, use the cor function
x <- as.matrix(b)
cor(x, method="spearman")
 
# use sampleRegular for large datasets
x <- sampleRegular(b, 1000)
cor.test(x[,1], x[,2])
}

\keyword{methods}
\keyword{spatial}

