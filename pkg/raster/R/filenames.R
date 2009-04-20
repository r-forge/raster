# R miscellaneouse file name related functions
# Authors: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3

# no longer used 

.shortFileName <- function(filename) {
# is this the same as basename ?
    filename <- gsub("\\\\", "/", filename)
	if (filename == "") {return(filename)
	} else {
		split <- strsplit(filename, "/")
		l <- length(split[[1]])
		shortfilename <- split[[1]][[l]]
		return(shortfilename)
	}	
}   
   
.path <- function(filename) {
#  use dirname instead
    filename <- gsub("\\\\", "/", filename)
	file <- .shortFileName(filename)
	path <- gsub(file, '', filename)
	return(path)
}   
