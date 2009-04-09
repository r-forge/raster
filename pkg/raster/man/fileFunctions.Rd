\name{fileFunctions}

\alias{ext}
\alias{ext<-}
\alias{trim}

\title{Filename manipulation}

\description{
Get or change a file extension
use \code{trim} to remove the whitespace before or after a string of characters.
}

\usage{
ext(filename)
ext(filename) <- value
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
}

\keyword{file}

