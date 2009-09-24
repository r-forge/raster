\name{rasterOptions}

\alias{rasterOptions}


\title{Global options for the raster package}

\description{
rasterOptions allows you to set, and inspect, a number of global options used by the raster package.

Most of these options, are used when writing files to disk. They can be ignored by specific functions if the corresponding argument is provided as an argument to these functions.

The desfault location is returned by \code{rasterTmpDir}. It is the same as that of the R tempdir but you can change it (for the current session) with \code{rasterOptions(tmpdir= }. 

To permanentely set any of these options, you can add them to \code{<your R installation>/etc/Rprofile.site}. For example, to change the default directory used to save temporary files, add a line like this: \code{options(rasterTmpDir='c:/temp/')} to that file. Beware: all raster (*.grd and *.gri) files in that folder will be deleted every time the raster package is loaded.
}

\usage{
rasterOptions(filename, filetype, overwrite, datatype, track, tmpdir, progress, show=TRUE)
}

\arguments{
\item{filename}{Character. The default filename to write to; only used by raster algebra functions (to which you cannot supply a filename or other argument)}
\item{filetype}{Character. The default file format to use. See \code{\link[raster]{writeFormats}}}
\item{overwrite}{Logical. The default value for overwriting existing files. If \code{TRUE}, existing files will be overwritten}
\item{datatype}{Character. The default data type to use. See \code{\link[raster]{dataType}}}
\item{track}{Integer. Vector with the default row numbers for which functions provide feedback when tracking progress in functions. Default=-1 (no tracking) }
\item{tmpdir}{Character. The default location for writing temporary files}
\item{progress}{Character. Valid values are "text", "tcltk" (requirs the tcltk package), "windows" (on that platform only) and "" (the default in most functions, no progress bar)}
\item{show}{Logical. If \code{TRUE}, the current values for the options are shown}
}

\details{
argument \code{filename} sets global option \code{rasterFilename}. This value is only used by raster algebra functions (e.g., \code{\link[raster]{Math-methods}} and \code{\link[raster]{Arith-methods}}) that cannot take a 'filename' argument (or any other additional argument).

argument \code{filetype} sets global option \code{rasterFiletype}. The option is ignored by a function if you pass \code{filetype} as an argument to that function.

argument \code{overwrite} sets global option \code{rasterOverwrite}. The option is ignored by a function if you pass \code{overwrite} as an argument to that function.

argument \code{datatype} sets global option \code{rasterDatatype}. The option is ignored by a function if you pass \code{datatype} as an argument to that function.

argument \code{track} sets global option \code{rasterTrack}. The option is ignored by a function if you pass \code{track} as an argument to that function.

argument \code{tmpdir} sets global option \code{rasterTmpDir}.  See \code{\link[raster]{rasterTmpFile}}

argument \code{progress} sets global option \code{rasterProgress}. The option is ignored by a function if you pass \code{progress} as an argument to that function. 
}

\value{
this function is used for the side-effect of setting global options
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[base]{options}}, \code{\link[raster]{tmpFile}} }  


\keyword{ spatial }
