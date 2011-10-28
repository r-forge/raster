# Authors: Robert J. Hijmans 
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("merge")) {
	setGeneric("merge", function(x, y, ...)
		standardGeneric("merge"))
}	


setMethod('merge', signature(x='Raster', y='Raster'), 
function(x, y, ..., tolerance=0.05, filename="", format, overwrite, progress) { 
	
	x <- c(x, y, list(...))
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) { overwrite <- .overwrite()	}
	if (missing(progress)) { progress <- .progress() }
	merge(x, tolerance=tolerance, filename=filename, format=format, overwrite=overwrite, progress=progress, test=FALSE)

} )



setMethod('merge', signature(x='list', y='missing'), 
function(x, y,..., tolerance=0.05, filename="", format, overwrite, progress, test=TRUE){ 

	s <- sapply(x, function(x) inherits(x, 'Raster'))
	x <- x[s]

	if (length(x) < 2) {
		stop('merge needs at least 2 Raster* objects')
	}
	
	nl <- unique(sapply(x, nlayers))
	if (length(nl) != 1) {
		if (length(nl) == 2 & min(nl) == 1) {
			nl <- max(nl)
		} else {
			stop( 'different number of layers' )
		}
	}
	compare(x, extent=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)
	
	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) { overwrite <- .overwrite()	}
	if (missing(progress)) { progress <- .progress() }


	bb <- unionExtent(x)
	
	if (nl > 1) {
		out <- brick(x[[1]], values=FALSE)
	} else {
		out <- raster(x[[1]])
	}
	out <- setExtent(out, bb, keepres=TRUE, snap=FALSE)

	datatype <- 'INT4S'
	for (i in 1:length(x)) {
		dtype <- .shortDataType(dataType(x[[i]]))
		if (any(dtype == 'FLT')) {
			datatype <- 'FLT4S'
		}	
	}

	if ( canProcessInMemory(out, 3) ) {
		if (nl > 1) {
			v = matrix(NA, nrow=ncell(out), ncol=nl)
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
			v = rep(NA, ncell(out))
			for (i in 1:length(x)) {
				cells = cellsFromExtent( out, extent(x[[i]]) )
				vv = v[cells]
				vv[is.na(vv)] = getValues(x[[i]])[is.na(vv)]
				v[cells] = vv
			}
		}
		rm(vv)
		out <- setValues(out, v)
		if (filename != '') {
			out <- writeRaster(out, filename=filename, format=format, datatype=datatype, overwrite=overwrite)
		}
		return(out)
	}
	
	
	rowcol <- matrix(0, ncol=4, nrow=length(x))
	for (i in 1:length(x)) {
		xy1 <- xyFromCell(x[[i]], 1) # first row/col on old raster[[i]]
		xy2 <- xyFromCell(x[[i]], ncell(x[[i]]) )     #last row/col on old raster[[i]]
		rowcol[i,1] <- rowFromY(out, xy1[2])       #start row on new raster
		rowcol[i,2] <- rowFromY(out, xy2[2])       #end row
		rowcol[i,3] <- colFromX(out, xy1[1])       #start col
		rowcol[i,4] <- rowcol[i,3] + ncol(x[[i]]) - 1  #end col
	}

	if (filename == "") {
		filename <- rasterTmpFile()
	} 

	out <- writeStart(out, filename=filename, format=format, datatype=datatype, overwrite=overwrite)

	tr <- blockSize(out, minblocks=2)
	tr$row = sort(unique(c(tr$row, rowcol[,1:2])))
	tr$nrows = c(tr$row[-1], nrow(out)+1) - c(tr$row)
	tr$n = length(tr$row)
	pb <- pbCreate(tr$n, type=progress)

	if (nl == 1) {
		for (i in 1:tr$n) {
			vv <- v <- matrix(NA, nrow=tr$nrow[i], ncol=ncol(out))
			for (j in length(x):1) {  #reverse order so that the first raster covers the second etc.
				if (tr$row[i] >= rowcol[j,1] &  tr$row[i] <= rowcol[j,2]) {
					row1 <- tr$row[i] - rowcol[j,1] + 1
					vv[] <- NA
					vv[, rowcol[j,3]:rowcol[j,4]] <- matrix(getValues(x[[j]], row1, tr$nrow[i]), nrow=tr$nrow[i], byrow=TRUE)	
					v[!is.na(vv)] <- vv[!is.na(vv)]	
				}
			}
			out <- writeValues(out, as.vector(t(v)), tr$row[i])
			pbStep(pb, i)
		}
	} else {
		for (i in 1:tr$n) {
			vv <- v <- matrix(NA, nrow=tr$nrow[i]*ncol(out), ncol=nl)
			for (j in length(x):1) {  #reverse order so that the first raster covers the second etc.
				if (tr$row[i] >= rowcol[j,1] &  tr$row[i] <= rowcol[j,2]) {
					row1 <- tr$row[i] - rowcol[j,1] + 1
					vv[] <- NA
					cells <- cellFromRowColCombine(out, 1:tr$nrow[i], rowcol[j,3]:rowcol[j,4])
					vv[cells, ] <- getValues(x[[j]], row1, tr$nrow[i])
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


