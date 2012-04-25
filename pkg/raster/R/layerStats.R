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


layerStats <- function(x, stat, w, na.rm=TRUE, ...) {
	
	stat <- tolower(stat)
	stopifnot(stat %in% c('cov', 'weighted.cov'))

	nl <- nlayers(x)
	n <- ncell(x)
	
	if (stat == 'weighted.cov') {
		if (missing(w))	{
			stop('to compute weighted covariance a weights layer should be provided')
		}

		if (nl != 1 & nlayers(w) != nl) {
			stop('nlayers(w) should be 1 or equal to nlayers(s)')
		}

		if (na.rm) {
		# a cell is set to NA if it is NA in any layer. That is not ideal, but easier and quicker
			nas <- calc(x, function(i) sum(i)) * w
			x <- mask(x, nas)
			w <- mask(w, nas)
		}

		sumw <- cellStats(w, stat='sum', na.rm=na.rm)
		means <- cellStats(x, stat='mean', na.rm=na.rm) / sumw
		x <- (x - means) * sqrt(w)
		
		# We should do this more efficiently but...
		covmat=matrix(NA, nrow=nl,ncol=nl)
		for(i in 1:nl) {
			for(j in i:nl) {
				r <- raster(x, layer=i) * raster(x,layer=j)
				v <- cellStats(r, stat='sum', na.rm=na.rm) / sumw
				covmat[j,i] <- covmat[i,j] <- v
				
			}
		}
		cov.w <- list(covmat,means)
		names(cov.w) <- c("weigthed covariance", "weighted means")
		
	} else if (stat == 'cov') {

		means <- cellStats(x, stat='mean', na.rm=na.rm) 
		notnas <- ncell(x) - cellStats(x, 'countNA')
		x <- (x - means)
		
		covmat=matrix(NA, nrow=nl,ncol=nl)
		for(i in 1:nl) {
			for(j in i:nl) {
				r <- raster(x, layer=i) * raster(x, layer=j)
				if (na.rm) {
					v <- cellStats(r, stat='sum', na.rm=na.rm) / (n - cellStats(r, stat='countNA'))
				} else {
					v <- cellStats(r, stat='sum', na.rm=na.rm) / n
				}
				covmat[j,i] <- covmat[i,j] <- v
			}
		}
		covar <- list(covmat,means)
		names(covar) <- c("covariance", "means")
	}

	return(covar)
	
}


