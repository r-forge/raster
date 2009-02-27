\name{miscellanous}
\alias{setFileExtension}
\alias{fileExtension}
\alias{shortFileName}
\alias{filePath}
\alias{trim}

\title{Miscellaneous functions}
\description{
  Low level functions for file and string handling. Get or change file extension or extract the filename or path from a full name (path + filename). Trim a string of characters (remove the whitespace before or after the string).
}
\usage{
setFileExtension(filename, newextension)
fileExtension(filename)
shortFileName(filename)
filePath(filename)
trim(astring)
}
\arguments{
  \item{filename}{A file name, with or without the path}
  \item{newextension}{A file extension, e.g., ".txt" }
  \item{astring}{A string that needs to be trimmed}
}

\value{
  A file extension, filename or path.  
}

\author{Robert J. Hijmans }

\examples{
	fn <- "  c:/temp folder/filename.extension  "
	fn <- trim(fn)
	fileExtension(fn)
	setFileExtension(fn, ".txt")
	shortFileName(fn)
	filePath(fn)
}
\keyword{file}
