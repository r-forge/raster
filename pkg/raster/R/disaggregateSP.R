
setMethod('disaggregate', signature(x='SpatialPolygons', fact='ANY'), 
function(x, fact=NULL, ...) {
	require(rgeos)
	
	if (! .hasSlot(x, 'data') ) {
		if (version_GEOS0() < "3.3.0") {
			x <- gUnionCascaded(x)
		} else {
			x <- gUnaryUnion(x)
		}	
		return(x)
	
	} else {
		if (is.null(fact)) {
			if (version_GEOS0() < "3.3.0") {
				x <- gUnionCascaded(x)
			} else {
				x <- gUnaryUnion(x)
			}	
			x <- SpatialPolygonsDataFrame(x, data=data.frame(id=1))
			return(x)
		} else {
			crs <- x@proj4string
			dat <- x@data[,fact]
			dc <- apply(dat, 1, function(y) paste(as.character(y), collapse='_'))
			dc <- data.frame(oid=1:length(dc), v=dc)
			dc[,2] <- as.integer(dc[,2])
			ud <- data.frame(v=unique(dc[,2]))
			md <- merge(dc, ud, by='v')
			md <- md[order(md$oid), ]
			
			if (version_GEOS0() < "3.3.0") {
				x <- lapply(1:nrow(ud), function(y) gUnionCascaded(x[md[md$v==y,2],]))
			} else {
				x <- lapply(1:nrow(ud), function(y) gUnaryUnion(x[md[md$v==y,2],]))
			}	
			
			x <- sapply(1:length(x), function(y) x[[y]]@polygons[[1]]@Polygons)
			x <- SpatialPolygons(lapply(1:length(x), function(y) Polygons(x[[y]], y)))
			x@proj4string <- crs
			
			id <- md[!duplicated(md[,1]),]
			id <- id[order(id$v), ]
			dat <- dat[id[,2], ]
			rownames(dat) <- 1:nrow(dat)
			
			SpatialPolygonsDataFrame(x, data=dat)
		}
	}
}
)


