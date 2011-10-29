# Author: Robert J. Hijmans
# Date : October 2008
# Version 0.9
# Licence GPL v3

# redesinged for multiple row processing
# October 2011
# version 1

if (!isGeneric("merge")) {
	setGeneric("merge", function(x, y, ...)
		standardGeneric("merge"))
}	



setMethod('merge', signature(x='Raster', y='Raster'), 
function(x, y, ..., tolerance=0.05, filename="", format, datatype, overwrite, progress) { 
	x <- c(x, y, list(...))
	x <- x[ sapply(x, function(x) inherits(x, 'Raster')) ]
	if (length(x) < 2) {
		stop('merge needs at least 2 Raster* objects')
	}
	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) { overwrite <- .overwrite()	}
	if (missing(progress)) { progress <- .progress() }
	if (missing(datatype)) { datatype <- .commonDataType(sapply(x, dataType)) } 
	merge(x, tolerance=tolerance, filename=filename, format=format, datatype=datatype, overwrite=overwrite, progress=progress, check=FALSE)
} )



setMethod('merge', signature(x='list', y='missing'), 
function(x, y,..., tolerance=0.05, filename="", format, datatype, overwrite, progress, check=TRUE){ 

	if (check) {
		x <- x[ sapply(x, function(x) inherits(x, 'Raster')) ]
		if (length(x) < 2) {
			stop('merge needs at least 2 Raster* objects')
		}
		filename <- trim(filename)
		if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
		if (missing(overwrite)) { overwrite <- .overwrite()	}
		if (missing(progress)) { progress <- .progress() }
		if (missing(datatype)) { datatype <- .commonDataType(sapply(x, dataType)) } 
	}

	nl <- max(unique(sapply(x, nlayers)))

	compare(x, extent=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)
	

	bb <- unionExtent(x)
	if (nl > 1) {
		out <- brick(x[[1]], values=FALSE, nl=nl)
	} else {
		out <- raster(x[[1]])
	}
	out <- setExtent(out, bb, keepres=TRUE, snap=FALSE)
	
	if ( canProcessInMemory(out, 3) ) {
		if (nl > 1) {
			v <- matrix(NA, nrow=ncell(out), ncol=nl)
			for (i in 1:length(x)) {
				cells <- cellsFromExtent( out, extent(x[[i]]) )
				vv <- v[cells, ]
				na <- as.logical( apply(vv, 1, FUN=function(x) sum(is.na(x))==nl) )
				dat <- getValues(x[[i]])
				if (!is.matrix(dat)) {
					dat <- matrix(dat, ncol=1)
				}
				vv[na, ] <- dat[na, ]
				v[cells, ] <- vv
			}
		} else {
			v <- rep(NA, ncell(out))
			for (i in 1:length(x)) {
				cells <- cellsFromExtent( out, extent(x[[i]]) )
				vv <- v[cells]
				vv[is.na(vv)] <- getValues(x[[i]])[is.na(vv)]
				v[cells] <- vv
			}
		}
		rm(vv)
		out <- setValues(out, v)
		if (filename != '') {
			out <- writeRaster(out, filename=filename, format=format, datatype=datatype, overwrite=overwrite)
		}
		return(out)
	}
	
	rowcol <- matrix(NA, ncol=5, nrow=length(x))
	for (i in 1:length(x)) {
		xy1 <- xyFromCell(x[[i]], 1) 				# first row/col on old raster[[i]]
		xy2 <- xyFromCell(x[[i]], ncell(x[[i]]) )   # last row/col on old raster[[i]]
		rowcol[i,1] <- rowFromY(out, xy1[2])       	# start row on new raster
		rowcol[i,2] <- rowFromY(out, xy2[2])    	# end row
		rowcol[i,3] <- colFromX(out, xy1[1])	    # start col
		rowcol[i,4] <- colFromX(out, xy2[1])		# end col
		rowcol[i,5] <- i							# layer
	}

	tr <- blockSize(out, minblocks=2)
	tr$row <- sort(unique(c(tr$row, rowcol[,1], rowcol[,2]+1)))
	tr$row <- subset(tr$row, tr$row <= nrow(out)) 
	tr$nrows <- c(tr$row[-1], nrow(out)+1) - c(tr$row)
	tr$n <- length(tr$row)

	pb <- pbCreate(tr$n, type=progress)
	out <- writeStart(out, filename=filename, format=format, datatype=datatype, overwrite=overwrite)
	
	if (nl == 1) {
		for (i in 1:tr$n) {
			v <- matrix(NA, nrow=tr$nrow[i], ncol=ncol(out))
			rc <- subset(rowcol, tr$row[i] >= rowcol[,1] &  tr$row[i] <= rowcol[,2])			
			if (nrow(rc) > 0) {
				vv <- v
				for (j in nrow(rc):1) {  #reverse order so that the first raster covers the second etc.
					vv[] <- NA
					vv[, rc[j,3]:rc[j,4]] <- matrix(getValues(x[[ rc[j,5] ]], tr$row[i]-rc[j,1]+1, tr$nrow[i]), nrow=tr$nrow[i], byrow=TRUE)	
					v[!is.na(vv)] <- vv[!is.na(vv)]	
				}
			}
			out <- writeValues(out, as.vector(t(v)), tr$row[i])
			pbStep(pb, i)
		}
	} else {
		for (i in 1:tr$n) {
			v <- matrix(NA, nrow=tr$nrow[i]*ncol(out), ncol=nl)
			rc <- subset(rowcol, tr$row[i] >= rowcol[,1] &  tr$row[i] <= rowcol[,2])			
			if (nrow(rc) > 0) {
				vv <- v
				for (j in nrow(rc):1) { 
					vv[] <- NA
					cells <- cellFromRowColCombine(out, 1:tr$nrow[i], rc[j,3]:rc[j,4])
					vv[cells, ] <- getValues(x[[ rc[j,5] ]], tr$row[i]-rc[j,1]+1, tr$nrow[i])
					v[!is.na(vv)] <- vv[!is.na(vv)]	
				}
			}
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
	}

	pbClose(pb)
	writeStop(out)
}
)

