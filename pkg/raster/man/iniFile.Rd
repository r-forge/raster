\name{inifile}

\alias{readIniFile}

\title{read ini file}

\description{
This function reads  '.ini' files. These are text file databases that are orginized in [sections] pairs of "name = value".
}

\usage{
readIniFile(filename)
}

\arguments{
  \item{filename}{filename of the .ini file}
}

\details{
This function allows for using inistrings that have "=" as part of a value 
}


\value{
	a n*3 matrix of characters with columns: section, name, value
}

\author{Robert J. Hijmans}

\keyword{file}
