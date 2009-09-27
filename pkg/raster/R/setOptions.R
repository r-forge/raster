# Author: Robert J. Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.9
# Licence GPL v3


setOptions <- function(filetype, overwrite, datatype, tmpdir, progress) {
	if (!missing(filename)) { .setFilename(filename) }
	if (!missing(filetype)) { .setFiletype(filetype) }
	if (!missing(overwrite)) { .setOverwrite(overwrite) }
	if (!missing(datatype)) { .setDatatype(datatype) }
	if (!missing(progress)) { .setProgress(progress) }
	if (!missing(tmpdir)) { .setTmpdir(tmpdir) }
}



.setFilename <- function(filename) {
	if (is.character(filename)) {
		if (length(filename) == 1) {
			options(rasterFilename = filename)
		} else {
			warning(paste('Could not set filename. It should not be a vector with length > 1'))	
		}
	} else {
		warning(paste('Could not set filename. It should a character type variable'))	
	}
}

.setOverwrite <- function(overwrite) {
	if (is.logical(overwrite)) {
		options(rasterOverwrite = overwrite)
	} else {
		warning(paste('Could not set overwrite. It must be a logical value'))	
	}
}


.setInMemory <- function(inmemory) {
	if (is.logical(inmemory)) {
		options(rasterInMemory = inmemory )
	} else {
		warning(paste('inmemory must be a logical value'))	
	}
}


.setProgress <- function(progress) {
	if (is.character(progress)) {
		progress <- trim(progress)
		if (progress %in% c('text', 'tcltk', 'windows', 'none', '')) {
			if (substr( R.Version()$platform, 1, 7) != "i386-pc" ) {
				if (progress == 'windows') {
					warning('The windows progress bar is only availble on the Windows Operating System')
				} else {
					options(rasterProgress = progress )
				}
			} else {
				options(rasterProgress = progress )
			}
		}
	} else {
		warning(paste('progress must be a character value'))	
	}
}



.setFiletype <- function(filetype) {
	if (.isSupportedFormat(filetype)) {
		options(rasterFiletype = filetype)
	} else {
		warning(paste('Cannot set filetype to unknown or unsupported type:', filetype, '. See writeFormats()'))
	}
}


.setDatatype <- function(datatype) {
	if (datatype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'INT4U', 'INT8U', 'FLT4S', 'FLT8S')) {
		options(rasterDatatype = datatype)
	} else {
		warning(paste('Cannot set datatype to unknown type:',datatype))
	}
}


.setTmpdir <- function(tmpdir) {
	if (!missing(tmpdir)) {
		res <- file.exists(tmpdir)
		if (!res) {
			res <- dir.create(tmpdir,  showWarnings = FALSE)
		}
		if (res) { 
			options(rasterTmpDir = tmpdir) 
		} else {
			warning(paste('could not create tmpdir:',tmpdir))
		}
	}
}

