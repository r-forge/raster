# R miscellaneouse file name related functions
# Authors: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3

   
ext <- function(filename) {
	lfn <- nchar(filename)
	extstart <- -1
    for (i in lfn : 2) {
		if (substr(filename, i, i) == ".") {
			extstart <- i
			break
		}
	}
    if (extstart > 0) {
		ext <- substr(filename, extstart, lfn)
		}
	else { ext <- "" }   
	return(ext)  
}   


'ext<-' <- function(filename, value) {
	lfn <- nchar(filename)
	value <- trim(value)
	if (value != "" & substr(value, 1, 1) != ".") {
		value <- paste(".", value, sep="") 
	}
	extstart <- -1
	for (i in lfn : 2) {
		if (substr(filename, i, i) == ".") {
			extstart <- i
			break 
		}
	}
    if (extstart > 0) {
	   fname <- paste(substr(filename, 1, extstart-1), value, sep="")
	   }
	else { fname <- paste(filename, value, sep="")   
	}
  return(fname)  
}   

