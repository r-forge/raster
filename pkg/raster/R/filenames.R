# R miscellaneouse file name related functions
# Authors: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3


trim <- function(x) {
	f <- function(s) {return( gsub('^[[:space:]]+', '',  gsub('[[:space:]]+$', '', s) ) )}
	return(unlist(lapply(x, f)))
}


shortFileName <- function(filename) {
    filename <- gsub("\\\\", "/", filename)
	if (filename == "") {return(filename)
	} else {
		split <- strsplit(filename, "/")
		l <- length(split[[1]])
		shortfilename <- split[[1]][[l]]
		return(shortfilename)
	}	
}   
   
filePath <- function(filename) {
    filename <- gsub("\\\\", "/", filename)
	file <- shortFileName(filename)
	path <- gsub(file, '', filename)
	return(path)
}   

   
fileExtension <- function(filename) {
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


setFileExtension <- function(filename, newextension="") {
	lfn <- nchar(filename)
	newextension <- trim(newextension)
	if (newextension != "" & substr(newextension, 1, 1) != ".") {
		newextension <- paste(".", newextension, sep="") 
	}
	extstart <- -1
	for (i in lfn : 2) {
		if (substr(filename, i, i) == ".") {
			extstart <- i
			break 
		}
	}
    if (extstart > 0) {
	   fname <- paste(substr(filename, 1, extstart-1), newextension, sep="")
	   }
	else { fname <- paste(filename, newextension, sep="")   
	}
  return(fname)  
}   

