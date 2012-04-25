#' Weighted Covariance Matrices and Means for Rasters
#' 
#' Calculates the weighted covariance and (optionally) weighted means of bands in an Raster.
#' 
#' @param dm A multiband Raster* to use in calculating weighted covariance matrices.
#' @param wt A Raster* object of the weights (should have the same extent as dm).
#' @param return_means Logical. Return the weighted per-band means?
#' @return Returns either a covariance matrix of dimensions nlayers(dm) x nlayers(dm) or a list containing the covariance matrix and the weighted per-band means.
#' @author Mort Canty (original code) and Jonathan A. Greenberg (R port).
#' @seealso \code{\link{cov.wt}}, \code{\link{weighted.mean}}
#' @references
#' \itemize{
#' \item {Canty, M.J. and A.A. Nielsen. 2008. Automatic radiometric normalization of multitemporal satellite imagery with the iteratively re-weighted MAD transformation. Remote Sensing of Environment 112:1025-1036.}
#' \item {Nielsen, A.A. 2007. The regularized iteratively reweighted MAD method for change detection in multi- and hyperspectral data. IEEE Transactions on Image Processing 16(2):463-478.}
#' }
# @keywords {weighted covariance matrix}
# {weighted means}
# @examples
# \dontrun{
# } 
#' @export


layerStats <- function(x, stat, w, ...) {
	
	stat <- tolower(stat)
	stopifnot(stat %in% c('cov', 'weighted.cov'))
	
	if (stat == 'weighted.cov') {
		if(missing(w))	{
			stop('to compute weighted covariance a weights layer should be provided')
		}

		nl <- nlayers(x)
		n <- ncell(w)
		sumw <- cellStats(w,stat='sum', na.rm=TRUE)
		means <- cellStats(x, stat='mean', na.rm=TRUE) / sumw
				
		x <- (x - means)*sqrt(w)
		
		# We should do this more efficiently but...
		covmat=matrix(NA, nrow=nl,ncol=nl)
		for(i in 1:nl) {
			for(j in (i+1):nl) {
				r <- raster(x, layer=i) * raster(x,layer=j)
				covmat[i,j] <- cellStats(r,stat='sum',na.rm=TRUE)/sumw	
			}
		}
		cov.w <- list(covmat,means)
		names(cov.w) <- c("weigthed covariance", "weighted means")
		return(cov.w) 
		
	} else if (stat == 'cov') {

		nl <- nlayers(x)
		means <- cellStats(x, stat='mean', na.rm=TRUE) / sumw
	
		x <- (x - means)
		
		covmat=matrix(NA, nrow=nl,ncol=nl)
		for(i in 1:nl) {
			for(j in (i+1):nl) {
				r <- raster(x, layer=i) * raster(x,layer=j)
				covmat[i,j] <- cellStats(r,stat='sum',na.rm=TRUE)/sumw	
			}
		}
		covar <- list(covmat,means)
		names(covar) <- c("covariance", "means")
		return(covar)
	}
}



