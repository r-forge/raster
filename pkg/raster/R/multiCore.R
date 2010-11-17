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



beginCluster <- function(n, type) {
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
		cl <- makeCluster(n, type) 
		clusterCall(cl, library, 'raster', character.only=TRUE )
		raster_Cluster_raster_Cluster <<- cl
		options(rasterCluster = TRUE)
	} else {
		stop('only 1 core detected. No cluster made')	
		options(rasterCluster = FALSE)
	}
}

endCluster <- function() {
	options(rasterCluster = FALSE)
	stopCluster(.getCluster())
	rm(raster_Cluster_raster_Cluster, envir=.GlobalEnv)
}



.getCluster <- function() {
	return(get('raster_Cluster_raster_Cluster', envir=.GlobalEnv))
}

.doCluster <- function() {
	rc <- options("rasterCluster")[[1]]
	if ( isTRUE(rc) ) {
		cl <- .getCluster()
		pkgs <- .packages()
		i <- which( pkgs %in% c("raster", "sp", "stats", "graphics", "grDevices", "utils", "datasets", "methods", "base") )
		pkgs <- rev( pkgs[-i] )
		for ( pk in pkgs ) {
			clusterCall(cl, library, pk, character.only=TRUE )
		}
		raster_Cluster_raster_Cluster <<- cl
		return(TRUE)
	} else {
		return(FALSE) 	
	}
}



