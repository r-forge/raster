
setMethod('aggregate', signature(x='SpatialPolygons'), 
function(x, var=colnames(x@data), sum=NULL, ...) {
	require(rgeos)
	
	if (! .hasSlot(x, 'data') ) {
		if (version_GEOS0() < "3.3.0") {
			x <- gUnionCascaded(x)
		} else {
			x <- gUnaryUnion(x)
		}	
		return(x)
	
	} else if (isTRUE(is.null(var)) | isTRUE(is.na(var))) {
		if (version_GEOS0() < "3.3.0") {
			x <- gUnionCascaded(x)
		} else {
			x <- gUnaryUnion(x)
		}	
		x <- SpatialPolygonsDataFrame(x, data=data.frame(id=1))
		return(x)
		
	} else {
		getVars <- function(v, cn) {
			vl <- length(v)
			v <- unique(v)
			if (is.numeric(v)) {
				v <- round(v)
				v <- v[v>0 & v <= ncol(x@data)]
				if (length(v) < 1) {
					stop('invalid column numbers')
				}
			} else if (is.character(v)) {
				v <- v[v %in% colnames(dat)]
				if (length(v) < 1) {
					stop('invalid column names')
				}
			}
			v
		}
		
		dat <- x@data
		cn <- colnames(dat)
		
		
		v <- getVars(var, cn)
		
		dat <- dat[,v, drop=FALSE]
		crs <- x@proj4string
		dc <- apply(dat, 1, function(y) paste(as.character(y), collapse='_'))
		dc <- data.frame(oid=1:length(dc), v=as.integer(as.factor(dc)))
		id <- dc[!duplicated(dc$v), ,drop=FALSE]
		id <- id[order(id$v), ]

		dat <- dat[id[,1], ,drop=FALSE]
		if (!is.null(sum)) {
			out <- list()
			for (i in 1:length(sum)) {
				if (length(sum[[i]]) != 2) {
					stop('argument "s" most of be list in which each element is a list of two (fun + varnames)')
				}
				fun = sum[[i]][[1]]
				if (!is.function(fun)) {
					if (is.character(fun)) {
						if (tolower(fun[1]) == 'first') {
							fun <- function(x) x[1]
						} else if  (tolower(fun[1]) == 'last') {
							fun <- function(x) x[length(x)]
						} 
					}
				}
				v <- getVars(sum[[i]][[2]], cn)
				ag <- aggregate(x@data[,v,drop=FALSE], by=list(dc$v), FUN=fun) 
				out[[i]] <- ag[,-1,drop=FALSE]
			}
			out <- do.call(cbind, out)
			dat <- cbind(dat, out)
		}
		
		if (version_GEOS0() < "3.3.0") {
			x <- lapply(1:nrow(id), function(y) spChFIDs(gUnionCascaded(x[dc[dc$v==y,1],]), as.character(y)))
		} else {
			x <- lapply(1:nrow(id), function(y) spChFIDs(gUnaryUnion(x[dc[dc$v==y,1],]), as.character(y)))
		}	
		
		x <- do.call(rbind, x)
		x@proj4string <- crs
		rownames(dat) <- as.character(id$v)	
		SpatialPolygonsDataFrame(x, data=dat)
	}
}
)


