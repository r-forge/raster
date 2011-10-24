
if (!isGeneric(".quad")) {
	setGeneric(".quad", function(x, ...)
		standardGeneric(".quad"))
}	



setMethod('.quad', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, nl=1, crs) {
		e <- extent(xmn, xmx, ymn, ymx)
		if (missing(crs)) {
			if (e@xmin > -400 & e@xmax < 400 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs ="+proj=longlat +datum=WGS84"
			} else {
				crs=NA
			}
		}
		b <- quad(e, nrows=nrows, ncols=ncols, crs=crs, nl=nl)
		return(b)
	}
)


setMethod('.quad', signature(x='Extent'), 
	function(x, nrows=10, ncols=10, crs=NA, nl=1) {
		bb <- extent(x)
		nr = as.integer(round(nrows))
		nc = as.integer(round(ncols))
		if (nc < 1) { stop("ncols should be > 0") }
		if (nr < 1) { stop("nrows should be > 0") }
		b <- new("RasterQuadBrick", extent=bb, ncols=nc, nrows=nr)
		projection(b) <- crs
		nl <- max(round(nl), 0)
		b@data@nlayers <- as.integer(nl)
		return(b) 
	}
)

