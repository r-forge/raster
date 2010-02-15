# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='Raster'), 
	function(object, model, filename="", const=NULL, xy=FALSE, index=1, debug.level=1, se.fit=FALSE, progress=.progress(), ...) {
	
		filename <- trim(filename)
		if (class(model)[1] %in% c('Bioclim', 'Domain', 'Mahalanobis', 'MaxEnt', 'ConvexHull')) { return ( predict(model, object, filename=filename, ...) ) }
	
		predrast <- raster(object)
			
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


		tr <- blockSize(predrast, n=nlayers(object)+3)

		napred <- rep(NA, ncol(predrast))
		xyOnly <- FALSE
		if (xy) { 
			ablock <- 1:(ncol(object) * tr$size)
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
		
		
		pb <- pbCreate(tr$n,  type=progress)			
		
		if (filename != '') {
			predrast <- writeStart(predrast, filename=filename, ... )
		}

		for (i in 1:tr$n) {
		
			if (xyOnly) {
				p <- xyFromCell(predrast, ablock + (tr$rows[i]-1) * ncol(predrast)) 
				p <- na.omit(p)
				blockvals <- data.frame(x=p[,1], y=p[,2])
			} else {
				blockvals <- as.data.frame(getValuesBlock(object, row=tr$rows[i], nrows=tr$size))
				#colnames(blockvals) <- lyrnames
				if (haveFactor) {
					for (i in 1:length(f)) {
						blockvals[,f[i]] <- as.factor(blockvals[,f[i]])
					}
				}
				if (xy) {
					p <- xyFromCell(predrast, ablock + (tr$rows[i]-1) * ncol(predrast)) 
					blockvals <- cbind(data.frame( x=p[,1], y=p[,2]), blockvals) 
				}
				if (! is.null(const)) {
					blockvals = cbind(blockvals, const)
				}
			} 
			
			if (inherits(model, "gam")) {
				if ( sum(!is.na(blockvals)) == 0 ) {
					predv <- napred
				}
			} else {
			
				if (gstatmod) { 
					if (sp) { 
						row.names(p) <- 1:nrow(p)
						blockvals <- SpatialPointsDataFrame(coords=p, data = blockvals, proj4string = projection(predrast, asText = FALSE))
					}
					if (i == 1) { predv <- predict(model, blockvals, debug.level=debug.level, ...) 
					} else { predv <- predict(model, blockvals, debug.level=0, ...) }
					if (sp) { predv <- predv@data[,index] }
					else { predv <- predv[,index+2] }
					
				} else if (inherits(model, "Krig")) {  
					if (xyOnly) {
						predv <- predict(model, blockvals, ...)
					} else {
						rowv <- na.omit(blockvals)
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
		#			rowv <- na.omit(blockvals)
		#			predv <- napred
		#			if (nrow(rowv) > 0) {
		#				naind <- as.vector(attr(rowv, "na.action"))
		#				if (!is.null(naind)) {
		#					predv[-naind] <- predict(model, rowv, ...)
		#				} else {
		#					predv[] <- predict(model, rowv, ...)
		#				}
		#			}
				} else if (se.fit) {
					predv <- predict(model, blockvals, se.fit=TRUE, ...)
					predv <- as.vector(predv$se.fit)
				
				} else {
					predv <- predict(model, blockvals, ...)
				}
				
				if (class(predv)[1] == 'list') {
					predv = unlist(predv)
					if (length(predv) != nrow(blockvals)) {
						predv = matrix(predv, nrow=nrow(blockvals))
					}					
				}
				
				if (isTRUE(dim(predv)[2] > 1)) {
					predv=predv[,index]
				}
			
				# to change factor to numeric; should keep track of this to return a factor type RasterLayer
				predv = as.numeric(predv)
				
				if (length(predv) != nrow(blockvals)) {
				# perhaps no prediction for rows with NA ? 
				# this was a problem with predict.rf, now fixed
					blockvals <- na.omit( cbind(1:nrow(blockvals), blockvals) )
					indices <- blockvals[,1]
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
				predv = matrix(predv, nrow=ncol(predrast))
				cols = tr$rows[i]:(tr$rows[i]+dim(predv)[2]-1)
				v[,cols] <- predv
			} else {
				writeValues(predrast, predv, tr$rows[i])
			}
			pbStep(pb, i) 
		}
		pbClose(pb)
		
		if (filename == '') {
			predrast <- setValues(predrast, as.numeric(v))  # or as.vector
		} else {
			predrast <- writeStop(predrast)
		}
		
		return(predrast)
	}
)


