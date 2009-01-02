\name{miscellanous}
\alias{setFileExtension}
\alias{fileExtension}
\alias{fileName}
\alias{filePath}
\alias{trim}
\alias{calcMode}
\alias{calcCv}
\alias{readIniFile}

\title{ Miscellaneous functions }
\description{
  Low level functions for file and string handling. Get or change file extension or extract the filename or path from a full name (path + filename). Trim a string of characters (remove the whitespace before or after the string).
  And two miscellanous statistical functions (mode and coefficient of variation)
}
\usage{
setFileExtension(filename, newextension)
fileExtension(filename)
fileName(filename)
filePath(filename)
trim(astring)
calcMode(x, na.rm = TRUE)
calcCv(x, na.rm = TRUE, singlevalueaszero=TRUE)
readIniFile(filename)
}
\arguments{
  \item{filename}{ a filename with or without the path }
  \item{newextension}{ a file extension, e.g., ".txt"  }
  \item{astring} { a string that needs to be trimmed }
  \item{x} { a vector of numbers (typically integers for mode) }
  \item{na.rm} { remove (ignore) NA values }
  \item{singlevalueaszero} { if TRUE a zero is returned (rather than an NA) if the mode of single value is computed }
}

\value{
  file.get.* returns a file extension, filename or path.  
  calcMode returns the modal (most frequent) value of a vector of values, ties are broken at random
  calcCv returns the coefficient of variation (expressed as a percentage). If there is only a single value, sd = NA
  and calcCv returns NA if singlevaluecvzero=FALSE. However, the default is that cv=0 in this case.
  readIniFile reads "ini-files". I.e. text file databases that are orginized in [sections] and "name = value" pairs.
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
	fn <- "c:/temp folder/filename.extension"
	fileExtension(fn)
	setFileExtension(fn, ".txt")
	fileName(fn)
	filePath(fn)
	data <- c(0,1,2,3,3,3,3,4,4,4,5,5,6,7,7,8,9,NA)
	calcMode(data)
	calcCv(data)
}
\keyword{ spatial }

