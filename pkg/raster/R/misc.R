# R miscellanea
# Authors: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0,2
# Licence GPL v3

calc.mode <- function(x, na.rm = TRUE) {
#partly based on http://wiki.r-project.org/rwiki/doku.php?id=tips:stats-basic:modalvalue
# ties are broken at random
# earlier approach contained this
#		freq <- tapply(rep(0, length(x)), x, length)
#		w <- as.vector(which(freq == max(freq)) )
#		result <- as.numeric(dimnames(freq)[[1]][w])
#		return(result)
	z <- x[!is.na(x)]
	if (length(z) == 0) { return(NA) 
	} else if (na.rm == FALSE & length(z) < length(x)) { return(NA)	 
	} else if (length(z) == 1) { return(z)
	} else {
		freq <- table(z)
		w <- as.numeric(names(freq[max(freq)==freq]))
		if (length(w) > 1) {
			r <- runif(length(w))
			w <- w[which.max(r)]
		} 
		return(w)
	}	
}

calc.cv <- function(x, na.rm = TRUE, singlevalueaszero=TRUE) {
#  R function to compute the coefficient of variation (expressed as a percentage)
# if there is only a single value, sd = NA. However, one could argue that cv =0. In this case a NA is returned if(singlevaluecvzero=FALSE) 
# else a value of 0 is returned.
	z <- x[!is.na(x)]
	if (length(z) == 0) { return(NA) 
	} else if (na.rm == FALSE & length(z) < length(x)) { return(NA)	 
	} else if (length(z) == 1 & singlevalueaszero == TRUE) { return(0)
	} else {
		return(100 * sd(z) / mean(z))
	}	
}

string.trim <- function(astring) {
	return( gsub('^[[:space:]]+', '',  gsub('[[:space:]]+$', '', astring) ) )
}  

fileName <- function(filename) {
# Author: Robert Hijmans
# Version 1; Date: 1-Sep-2008; License: GPL3
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
	file <- fileName(filename)
	path <- gsub(file, '', filename)
	return(path)
}   
   
 fileExtension <- function(filename) {
# Author: Robert Hijmans
# Version 1; Date: 1-Sep-2008; License: GPL3
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


fileChangeExtension <- function(filename, newextension="") {
# Author: Robert Hijmans
# Version 1; Date: 1-Sep-2008; License: GPL3
	lfn <- nchar(filename)
	newextension <- string.trim(newextension)
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

