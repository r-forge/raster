\name{fileFunctions}

\alias{fileExtension}
\alias{fileExtension<-}
\alias{shortFileName}
\alias{filePath}
\alias{trim}

\title{filename functions}

\description{
  Low level functions for file name handling. 
  Get or change file extension or extract the filename or path from a full name (path + filename). 
  use \code{trim} to remove the whitespace before or after a string of characters.
}

\usage{
fileExtension(filename)
fileExtension(filename) <- value
shortFileName(filename)
filePath(filename)
trim(x)
}

\arguments{
  \item{filename}{A filename, with or without the path}
  \item{value}{A file extension including the dot, e.g., ".txt" }
  \item{x}{A character string (filename)}
}

\value{
A file extension, filename or path.  
}

\author{Robert J. Hijmans }

\examples{
	fn <- "  c:/temp folder/filename.extension  "
	fn <- trim(fn)
	fileExtension(fn)
	fileExtension(fn) <- ".txt"
	fileExtension(fn)
	shortFileName(fn)
	filePath(fn)
}

\keyword{file}

