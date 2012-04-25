\name{layerStats}

\alias{layerStats}


\title{Weighted Covariance Matrices and Means for Rasters}


\description{
Compute summary statistics for multi-layer Raster objects. Like \code{cellStats} this function returns a single value, or a matrix of values, and not a Raster* object (see \code{\link{Summary-methods}} for that). 
}


\usage{
layerStats(x, stat, w, na.rm=TRUE, ...)
}


\arguments{
  \item{x}{RasterStack or RasterBrick for which to compute a statistic}

  \item{stat}{Character. The statistic to compute: either 'cov', or 'weighted.cov'}

  \item{w}{RasterLayer with the weights (should have the same extent, resolution and number of layers as \code{x}) to compute the weighted covariance}

  \item{na.rm}{Logial. Should missing values be removed?}
  
  \item{...}{Additional arguments (none implemetned)} 
}


\value{
List containing the (weighted) covariance matrix and the (weighted, per layer) means.
}


\author{
Mort Canty (original code) and Jonathan A. Greenberg (R port).
}


\references{
\itemize{
\item {Canty, M.J. and A.A. Nielsen. 2008. Automatic radiometric normalization of multitemporal satellite imagery with the iteratively re-weighted MAD transformation. Remote Sensing of Environment 112:1025-1036.}
\item {Nielsen, A.A. 2007. The regularized iteratively reweighted MAD method for change detection in multi- and hyperspectral data. IEEE Transactions on Image Processing 16(2):463-478.} 
}
}

\seealso{
 \code{\link{cellStats}}, \code{\link{cov.wt}}, \code{\link[raster]{weighted.mean}}
}

