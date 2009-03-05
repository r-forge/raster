# Authors: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.8
# Licence GPL v3

readIniFile <- function(filename) {

	strSplitOnFirstToken <- function(s, token="=") {
		pos <- which(strsplit(s, '')[[1]]==token)[1]
		if (is.na(pos)) {
			return(c(trim(s), NA)) 
		} else {
			first <- substr(s, 1, (pos-1))
			second <- substr(s, (pos+1), nchar(s))
			return(trim(c(first, second)))
		}
	}

	strsplitcomment <- function(s) { 
		# ";" is the start of a comment .
		strSplitOnFirstToken(s, token=";") 
	}

	
    if (!file.exists(filename)) { stop(paste(filename, " does not exist")) }
	
	Lines <- readLines(filename,  warn = FALSE)

	ini <- lapply(Lines, strsplitcomment) 
	
	Lines <- matrix(unlist(ini), ncol=2, byrow=T)[,1]
	ini <- lapply(Lines, strSplitOnFirstToken) 
	
 	ini <- matrix(unlist(ini), ncol=2, byrow=T)
	ini <- subset(ini, ini[,1] != "")

	sections <- c(which(is.na(ini[,2])), length(ini[,2]))
# here I should check whether the section text is enclused in [ ]. If not, it is junk text that should be removed, rather than used as a section
	ini <- cbind("", ini)
	for (i in 1:(length(sections)-1)) {
		ini[sections[i]:(sections[i+1]), 1] <- ini[sections[i],2]
	}	
	ini[,1] <- gsub("\\[", "", ini[,1])
	ini[,1] <- gsub("\\]", "", ini[,1])
		
	colnames(ini) <- c("section", "name", "value")
	sections <- sections[1:(length(sections)-1)]
	return(ini[-sections,])
}

