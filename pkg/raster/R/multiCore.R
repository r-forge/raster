# Author: Matteo Mattiuzzi and Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2010
# Version 1.0
# Licence GPL v3


# taken from pkg multicore:
# detect the number of [virtual] CPUs (cores)
.multicoreDetectCores <- function(all.tests = FALSE) {
  # feel free to add tests - those are the only ones I could test [SU]
	systems <- list(darwin  = "/usr/sbin/sysctl -n hw.ncpu 2>/dev/null",
					linux   = "grep processor /proc/cpuinfo 2>/dev/null|wc -l",
					irix    = c("hinv |grep Processors|sed 's: .*::'", "hinv|grep '^Processor '|wc -l"),
					solaris = "/usr/sbin/psrinfo -v|grep 'Status of.*processor'|wc -l")
					
	for (i in seq(systems)) {
		if(all.tests || length(grep(paste("^", names(systems)[i], sep=''), R.version$os))) {
			for (cmd in systems[i]) {
				a <- gsub("^ +", "", system(cmd, TRUE)[1])
				if (length(grep("^[1-9]", a))) {
					return(as.integer(a))
				}
			}
		}	
	}		
	return(1)
}


.detectCores <- function(all.tests = FALSE) {
	if (.Platform$OS.type == 'windows') {
		nn <- length(readRegistry("HARDWARE\\DESCRIPTION\\System\\CentralProcessor", maxdepth=1)) # tested on XP
	} else {
		# detect the number of [virtual] CPUs (cores)
		nn <- .multicoreDetectCores(all.tests)
	}
	return(nn)
}


setCluster <- function(n, type) {
	if (! require(snow) ) {
		stop('you need to install the "snow" package')
	}
	if (missing(n)) {
		n <- .detectCores()
		cat(n, 'cores detected\n')
	}
	if (n > 1) {
		if (missing(type)) {
			type <- getClusterOption("type")
			cat('cluster type:', type, '\n')
		}
		options(rasterCluster = TRUE)
		options(rasterClusterCores = n)
		options(rasterClusterType = type)
	} else {
		options(rasterCluster = FALSE)
		options(rasterClusterCores = 1)
		options(rasterClusterType = '')
	}
}

.doCluster <- function() {
	rc <- options("rasterCluster")[[1]]
	if ( isTRUE(rc) ) {
		return(TRUE)
	} else {
		return(FALSE) 	
	}
}

.makeCluster <- function() {
	if (! require(snow) ) {
		stop('you need to install the "snow" package')
	}
	nodes <- options("rasterClusterCores")[[1]]
	cltype <- options("rasterClusterType")[[1]]
	cl <- makeCluster(nodes, type=cltype) 

	pkgs <- .packages()
	i <- which(pkgs %in% c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base"))
	pkgs <- rev(pkgs[-i])

	for (pk in pkgs) {
		clusterCall(cl, library, pk, character.only=TRUE )
	}
	return(cl)
}

