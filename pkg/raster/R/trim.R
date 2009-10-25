# R miscellaneouse file name related functions
# Authors: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


trim <- function(x) {
	f <- function(s) {return( gsub('^[[:space:]]+', '',  gsub('[[:space:]]+$', '', s) ) )}
	return(unlist(lapply(x, f)))
}
