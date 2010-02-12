# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='Raster'), 
	function(object, model, filename="", const=NULL, xy=FALSE, index=1, debug.level=1, progress=.progress(), ...) {
	
		if (class(model)[1] %in% c('Bioclim', 'Domain', 'Mahalanobis', 'MaxEnt', 'ConvexHull')) { return ( predict(model, object, filename=filename, ...) ) }
	
		predrast <- raster(object)
		filename <- trim(filename)
			
		dataclasses <- attr(model$terms, "dataClasses")[-1]	
			
		lyrnames <- layerNames(object)
		
		if (xy) {
			xylyrnames <- c('x', 'y', lyrnames)
		} else {
			xylyrnames <- lyrnames
		}

		varnames <- names(dataclasses)

		if ( length( unique(lyrnames[(lyrnames %in% varnames)] )) != length(lyrnames[(lyrnames %in% varnames)] )) {
			stop('duplicate names in Raster* object: ', lyrnames)
		}
			
		
		f <- names( which(dataclasses == 'factor') )
		if (length(f) > 0) { 
			haveFactor <- TRUE 
		} else {
			haveFactor <- FALSE
		}
		
		if (!canProcessInMemory(predrast) && filename == '') {
			filename <- rasterTmpFile()
			if (getOption('verbose')) { cat('writing raster to:', filename)	}						
		} 

		if (filename == '') {
			v <- matrix(NA, ncol=nrow(predrast), nrow=ncol(predrast))
		} 

		napred <- rep(NA, ncol(predrast))
		xyOnly <- FALSE
		if (xy) { 
			arow <- 1:ncol(object) 
			if (class(object) == 'RasterStack') {
				if (nlayers(object)==0) { xyOnly <- TRUE }
			} else {
				if (dataSource(object) == 'ram') {
					if (dataContent(object) != 'all') {
						xyOnly <- TRUE 
					}
				}				
			}
		} else {
			if (class(object) == 'RasterStack') {
				if (nlayers(object)==0) { stop('empty RasterStack') }
			} else {
				if (dataSource(object) == 'ram') {
					if (dataContent(object) != 'all') {
						{ stop('No values associated with this Raster object') }
					}
				}				
			}
		}
		
		if (inherits(model, "gstat")) { 
			gstatmod <- TRUE 
			if (!is.null(model$locations) && inherits(model$locations, "formula"))  {
				# should be ~x + y  ; need to check if it is ~lon + lat; or worse ~y+x
				sp <- FALSE
			} else {
				sp <- TRUE
			}
		} else { 
			gstatmod <- FALSE 
		}

		pb <- pbCreate(nrow(object), type=progress)
		
		#print(xy)
		#print(xyOnly)
		
		for (r in 1:nrow(object)) {
			if (xyOnly) {
				p <- xyFromCell(predrast, arow + (r-1) * ncol(predrast)) 
				rowvals <- data.frame(x=p[,1], y=p[,2])
			} else {
				rowvals <- as.data.frame( getValues(object, r) )
				colnames(rowvals) <- lyrnames
				if (haveFactor) {
					for (i in 1:length(f)) {
						rowvals[,f[i]] <- as.factor(rowvals[,f[i]])
					}
				}
				if (xy) {
					p <- xyFromCell(predrast, arow + (r-1) * ncol(predrast)) 
					rowvals <- cbind(data.frame( x=p[,1], y=p[,2]), rowvals) 
				}
				if (! is.null(const)) {
					rowvals = cbind(rowvals, const)
				}
			} 
			
			if (inherits(model, "gam")) {
				if ( sum(!is.na(rowvals)) == 0 ) {
					predv <- napred
				}
			} else {
			
				if (gstatmod) { 
					if (sp) { 
						row.names(p) <- 1:nrow(p)
						rowvals <- SpatialPointsDataFrame(coords=p, data = rowvals, proj4string = projection(predrast, asText = FALSE))
					}
					if (r == nrow(predrast)) { predv <- predict(model, rowvals, debug.level=debug.level, ...) 
					} else { predv <- predict(model, rowvals, debug.level=0, ...) }
					if (sp) { predv <- predv@data[,index] }
					else { predv <- predv[,index+2] }
				} else if (inherits(model, "Krig")) {  
					if (xyOnly) {
						predv <- predict(model, rowvals, ...)
					} else {
						rowv <- na.omit(rowvals)
						predv <- napred
						if (nrow(rowv) > 0) {
							naind <- as.vector(attr(rowv, "na.action"))
							if (!is.null(naind)) {
								predv[-naind] <- predict(model, rowv, ...)
							} else {
								predv[] <- predict(model, rowv, ...)
							}
						}
					}
		#		} else if (inherits(model, 'gbm')) {
		#		# gbm returns non NA predictions when there are NA values. 
		#			rowv <- na.omit(rowvals)
		#			predv <- napred
		#			if (nrow(rowv) > 0) {
		#				naind <- as.vector(attr(rowv, "na.action"))
		#				if (!is.null(naind)) {
		#					predv[-naind] <- predict(model, rowv, ...)
		#				} else {
		#					predv[] <- predict(model, rowv, ...)
		#				}
		#			}
				} else {
					predv <- predict(model, rowvals, ...)
				}
			
				if (length(predv) != nrow(rowvals)) {
				# perhaps no prediction for rows with NA ? 
				# this was a problem with predict.rf, now fixed
					rowvals <- na.omit( cbind(1:nrow(rowvals), rowvals) )
					indices <- rowvals[,1]
					if (length(indices) == length(predv)) {
						pr <- 1:ncol(object)
						pr[] <- NA
						pr[indices] <- predv
						predv <- pr
					} else {
						stop('length of predict vector does not match lenght of input vectors; please submit a bug report')
					}
				}
			}
			
			if (filename == '') {
				v[,r] <- predv
			} else {
				predrast <- setValues(predrast, as.numeric(predv), r)
				predrast <- writeRaster(predrast, filename=filename, ...)
			}
			pbStep(pb, r) 
		}
		pbClose(pb)
		
		if (filename == '') {
			predrast <- setValues(predrast, as.vector(v))
		}
		return(predrast)
	}
)


