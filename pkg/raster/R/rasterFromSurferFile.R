
.isSurferFile <- function(filename) {
	con <- file(filename, "rb")
	id <- readBin(con, "character", n=1, size=4)
	if (id == 'DSBB') { 
		return (TRUE) 
	} else {
		return (id) 
	}
}

.rasterFromSurferFile <- function(filename) {
	con <- file(filename, "rb")
	r <- raster()
	id <- readBin(con, "character", n=1, size=4)
	r@ncols <- readBin(con, "int", n=1, size=2)
	r@rows <- readBin(con, "int", n=1, size=2)
	r@extent@xmin <- readBin(con, "double", n=1, size=8)
	r@extent@xmax <- readBin(con, "double", n=1, size=8)
	r@extent@ymin <- readBin(con, "double", n=1, size=8)
	r@extent@ymax <- readBin(con, "double", n=1, size=8)
	r@data@min <-  readBin(con, "double", n=1, size=8)
	r@data@max <-  readBin(con, "double", n=1, size=8)
	close(con)
	r@file@offset <- 56
	r@file@toptobottom <- FALSE
	dataType(r) <- 'FLT4S'
	r@data@source <- 'disk'
	r@file@driver <- "surfer"
	return(r)
}


