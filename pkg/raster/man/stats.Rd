\name{stats}
\alias{Mode}
\alias{CV}

\title{Miscellaneous statistical functions}
\description{
  Miscellanous statistical functions: mode and coefficient of variation
}
\usage{
Mode(x, ties='random', na.rm = TRUE)
CV(x, na.rm = TRUE, singlevalueaszero=TRUE)
}
\arguments{
  \item{x}{A vector of numbers (typically integers for mode)}
  \item{ties}{character. Indicates how to treat ties. Either 'random', 'lowest', 'highest', or 'NA'}
  \item{na.rm}{Remove (ignore) NA values}
  \item{singlevalueaszero}{If \code{TRUE} a zero is returned (rather than an NA) if the mode of single value is computed}
}

\value{
  Mode returns the modal (most frequent) value of a vector of values, ties are broken at random
  CV returns the coefficient of variation (expressed as a percentage). If there is only a single value, sd = NA
  and CV returns NA if singlevaluecvzero=FALSE. However, the default is that cv=0 in this case.
}

\author{Robert J. Hijmans \email{r.hijmans@gmail.com}}

\examples{
	data <- c(0,1,2,3,3,3,3,4,4,4,5,5,6,7,7,8,9,NA)
	Mode(data)
	CV(data)
}
\keyword{univar}

