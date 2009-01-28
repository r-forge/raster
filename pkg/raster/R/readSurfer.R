
.readSurfer6 <- function(filename) {
	con <- file(filename, "rb")
	id <- readBin(con, "characater", n=1, size=4)
	r <- newRaster()
	r@ncols <- readBin(con, "int", n=4, size=2)
	r@rows <- readBin(con, "int", n=4, size=2)
	r@bbox@xmin <- readBin(con, "double", n=1, size=8)
	r@bbox@xmax <- readBin(con, "double", n=1, size=8)
	r@bbox@ymin <- readBin(con, "double", n=1, size=8)
	r@bbox@ymax <- readBin(con, "double", n=1, size=8)
	r@data@min <-  readBin(con, "double", n=1, size=8)
	r@data@max <-  readBin(con, "double", n=1, size=8)
	ncells <- r@ncols * r@nrows
	v <- readBin(con, "numeric", n=ncells, size=4)
	close(con)
	m <- matrix(v, nrow=r@rows, ncol=r@ncol, byrow=T)
	m <- m[nrow(m):1, ] 
	r <- setValues(r, as.vector(t(m)))
	r@file@driver <- "surfer"
	return(r)
}


