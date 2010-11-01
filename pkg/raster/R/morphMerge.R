# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2010
# Version 1,0
# Licence GPL v3

morphMerge <- function(x, y, ..., crs, res, fun, na.rm=TRUE, tolerance=0.05, method='bilinear', filename='') {

	warning('this function is still experimental, please provide feedback on odd behavior')
	
	if (missing(res)) { stop('provide a res argument') }
	if (missing(crs)) { stop('provide a crs argument') }
	if ( projection(crs) == 'NA' ) { stop('crs cannot be NA') }
	validObject(projection(crs, asText=FALSE))
	
	datatype <- .datatype(...)
	overwrite <- .overwrite(...)
	filetype <- .filetype(...)	
	
	rl <- .makeRasterList(x, y, list(...), unstack=FALSE)
	es <- sapply( rl, extent )
	ep <- sapply( es, function(x) projectExtent(x, crs) )
	e <- unionExtent(unlist(ep)) + max(res)
	r <- raster(e, crs=crs)
	res(r) <- res
	for (i in 1:length(rl)) {
		ex <- ep[[i]] + max(res)
		rc <- crop(r, ex) 
		
		test <- compare(rl[[i]], rc, extent=FALSE, rowcol=FALSE, prj=TRUE, res=TRUE, orig=TRUE, tolerance=tolerance, stopiffalse=TRUE)
		if (test) {
			# do nothing
		} else if ( projection(crs) != projection(rl[[i]]) ) {
			cat('running projectRaster\n')
			rl[[i]] <- projectRaster( rl[[i]], rc, method=method )
		} else {
			cat('running resample\n')
			rl[[i]] <- resample(rl[[i]], rc, method=method)
		}
	}
	if (missing(fun)) {
		r <- merge(rl,                        tolerance=tolerance, filename=filename, format=filetype, datatype=datatype, overwrite=overwrite)
	} else {
		r <- mosaic(rl, fun=fun, na.rm=na.rm, tolerance=tolerance, filename=filename, format=filetype, datatype=datatype, overwrite=overwrite)
	}
	return(r)
}

