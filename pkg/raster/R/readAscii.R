# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2009
# Version 0.9
# Licence GPL v3


.readAllAscii <- function(x) {
	filename <- trim(filename(x))
    if (!file.exists(filename)) { stop(paste(filename, " does not exist")) }
	con <- file(filename, "rt")
	lines <- readLines(con, n=6)
	lines <- readLines(con, n=-1)
	close(con)
	x@data@values <- as.numeric(unlist(strsplit(lines, ' ')))
	x@data@content <- 'all'
	x@data@indices <- c(1, ncell(x))
	x <- setMinMax(x)
	return(x)
}


.readRowAscii <- function(x, rownr) {
	filename <- trim(filename(x))
    if (!file.exists(filename)) { stop(paste(filename, " does not exist")) }
	con <- file(filename, "rt")
	lines <- readLines(con, n=6)
	for (r in 1:rownr) {
	# ugly; and assuming that rows are all on new lines
		lines <- readLines(con, n=1)
	}
	close(con)
	firstcell <- cellFromRowCol(x, rownr, 1)
	lastcell <- cellFromRowCol(x, rownr, x@ncols)
	x@data@indices <- c(firstcell, lastcell)
	x@data@values <- as.numeric(unlist(strsplit(lines, ' ')))
	x@data@content <- 'row'
	return(x)
}
