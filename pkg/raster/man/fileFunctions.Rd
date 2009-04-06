\name{fileFunctions}

\alias{ext}
\alias{ext<-}
\alias{path}
\alias{trim}

\title{Filename manipulation}

\description{
  Low level functions for file name handling. 
  Get or change file extension or extract the filename or path from a full name (path + filename). 
  use \code{trim} to remove the whitespace before or after a string of characters.
}

\usage{
ext(filename)
ext(filename) <- value
path(filename)
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
fn <- "  c:/temp folder/filename.exten sion  "
fn <- trim(fn)
ext(fn)
ext(fn) <- ".txt"
ext(fn)
path(fn)
}

\keyword{file}

