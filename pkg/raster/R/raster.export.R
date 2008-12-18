
write.import <- function(raster, outfile, overwrite=FALSE) {
# check extension
	rsout <- set.raster(raster, filename=outfile)
	for (r in 1:nrow(raster)) {
		d <- readRow(raster, r)
		set.values.row(rsout, d, r)
		write.row(rsout, overwrite=overwrite)
	}
	clear.values(rsout)
	return(rsout)
}

write.export <- function(raster, outfile="", filetype="ascii", overwrite=FALSE) {
	if (string.trim(outfile) == "") { outfile <- filename(raster) }
	if (filetype == "ascii") {
		raster <- set.filename(raster,filename=outfile)
		write.ascii(raster, overwrite=overwrite) 
	} else {
		stop("filetype not yet supported (sorry..., more coming ...)")
	}
}



write.ascii <- function(raster, overwrite=FALSE) {
  	resdif <- abs((yres(raster) - xres(raster)) / yres(raster) )
	if (resdif > 0.01) {
		print(paste("raster has unequal horizontal and vertical resolutions","\n", "these data cannot be stored in arc-ascii format"))
	} else {
		if (raster@data@indices[1] == 1) {
			raster <- set.filename(raster, fileChangeExtension(filename(raster), '.asc'))
			if (!overwrite & file.exists(filename(raster))) {
				stop(paste(filename(raster), "exists. Use 'overwrite=TRUE'")) }

			thefile <- file(raster@file@name, "w")  # open an txt file connection
			cat("NCOLS", ncol(raster), "\n", file = thefile)
			cat("NROWS", nrow(raster), "\n", file = thefile)
			cat("XLLCORNER", xmin(raster), "\n", file = thefile)
			cat("YLLCORNER", ymin(raster), "\n", file = thefile)
			cat("CELLSIZE",  xres(raster), "\n", file = thefile)
			cat("NODATA_value", raster@file@nodatavalue, "\n", file = thefile)
			close(thefile) #close connection
		}
    }
	if (dataContent(raster) == "All") {	
		raster@data@values[is.na(values(raster))] <- raster@file@nodatavalue 
		write.table(values(raster), filename(raster), append = TRUE, quote = FALSE, 
							sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
	} else {
		for (r in 1:nrow(raster)) {
			readRow(raster, r)
			raster@data@values[is.na(values(raster))] <- raster@file@nodatavalue 
			write.table(values(raster), filename(raster), append = TRUE, quote = FALSE, 
							sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
		}
	}
		
	if ( dataIndices(raster)[2] >= ncells(raster)) {
		if ( dataIndices(raster)[2] > ncells(raster)) {
			stop(paste('writing beyond end of file. last cell:', raster@data@indices[2], '>', ncells(raster)))
		} else {
		# create a new object with gdal handle tfrom the new file
			raster <- raster.from.file(filename(raster)) 
		}
	}
	return(raster)
}
 