\name{factors}

\docType{methods}

\alias{is.factor}
\alias{is.factor,Raster-method}
\alias{is.factor,RasterStack-method}
\alias{as.factor}
\alias{as.factor,RasterLayer-method}
\alias{levels}
\alias{levels,Raster-method}
\alias{levels,RasterStack-method}
\alias{levels<-}
\alias{levels<-,Raster-method}
   
  
\title{Factors}

\description{
Factors are categorical variables. These functions allow for defining a layer as a categorical variable. 
This feature is not used much in the package. Whether a layer is defined as a factor or not is currently ignored by all, except the 'extract' functions (when used with option df=TRUE).
}

\usage{
is.factor(x)
as.factor(x)
levels(x)
}

\arguments{
  \item{x}{Raster* object}
}


\section{Methods}{
\describe{
factor type layers can be created with \code{as.factor} (RasterLayer objects only) or \code{levels(x) <- value}. \code{value} can be a factor or vector for RasterLayer objects. For multi-layer objects, it should be a list of the same lenght as nlayers(x). Each element of the list should be either NULL or a vector or factor. 
}}


\value{
Raster* object
}


\author{Robert J. Hijmans}

\examples{

set.seed(0)
r <- raster(nrow=10, ncol=10)
r[] <- runif(ncell(r)) * 10
is.factor(r)

r <- round(r)
f <- as.factor(r)
is.factor(f)

levels(f)
levels(f) <- letters[10:19]
levels(f)
f
}

\keyword{methods}
\keyword{spatial}

