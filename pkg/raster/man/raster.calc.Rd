\name{calc}
\alias{calc}
\alias{reclass}
\alias{neighborhood}
\alias{isNA}
\alias{setNA}
\alias{init}

\title{ RasterLayer calculate }
\description{
  Calculate values for a new raster, from scratch or based on a single existing raster
}
\usage{
calc(raster, fun, filename="", overwrite=FALSE, INT = FALSE)
isNA(raster, value = 0, filename="", overwrite=FALSE, INT = FALSE)
setNA(raster, operator = "<=", value = 0, filename="", overwrite=FALSE, INT = FALSE)
init(raster, fun=runif, filename="", overwrite=FALSE, INT = FALSE) 
neighborhood(raster, fun=mean, filename="", ngb=3, keepdata=TRUE, overwrite=FALSE) 
reclass(raster, rclmat, filename="", overwrite=FALSE, INT = FALSE)
}

\arguments{
  \item{raster}{ a RasterLayer object }
  \item{fun}{ the function to be applied, for \code{calc} this should be a function that takes a single number as an argument }
  \item{value}{ the (threshold) value to replace with NA or that should be set to NA }
  \item{operator}{ the operator in setNA relative to the \code{value}, to set e.g. all values <= 2 to NA}
  \item{rclmat}{ matrix for reclassifcation. (see below) }
  \item{filename}{ output filename for a new raster; if NA the result is not written to a file but returned with the raster object, in the data slot }
  \item{overwrite}{ logical to indicate whether an existing output file should be overwritten }
  \item{INT}{ logical. If \code{TRUE}, output will be integer values }
  \item{ngb} { neighborhood size in number of cells }
  \item{keepdata} {logical. If \code{TRUE}, NA will be removed for neighborhood computations. The result will only be NA if all cells are NA }
}

\details{
	Use 'calc' to calculate values for a new raster based on an existing single raster. E.g. to add, multiply or divide a number. Create and pass the appropriate function as an argument. 
	'isNA' replaces NA with a value and r.setNA sets values to NA; both are helper functions based on calc.
	'init' sets a value without reference to existing values. E.g. initialize with a single, sequential, or random number.
	'neighborhood' computes a neighborhood value. I.e. a value computed with function 'fun', for all cells in the square neigborhood (of size ngb * ngb) around each cell 
	'reclass' reclassifies groups of values to other values. E.g. All values between 1 and 10 become 1, and between 11 and 15 become 2. 
	Reclassification is done with matrix "rclmat". This matrix must have 3 columns. The first two columns are "from" "to" of the input values, and the third column has the new value for that range.
	
	If the input raster has all values in memory (after read.all(raster)), the functions will also return the new data in memory. If there is a file name, the data will also be saved to that file. 
	If the values are not in memory the new values will be written to file. 
	}
\value{
	a new raster file (on disk or in memory) and associated object (in the R environment)
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{

 r <- raster.new(ncols=36, nrows=18)
 
# two ways to set all value to 1 
#1, with a function
 set1function <- function(x){rep(1, x)}
 r <- init(r, fun=set1function)
# 2, "manually" 
 r <- set.values(r, rep(1, ncells(r)))
 
# random values (see ?runif )
 r <- init(r, fun=runif)

# all values below 0.5 become NA
 r2 <- setNA(r, operator ="<", value=0.5)
# change the NAs to -9999
 r3 <- isNA(r2, -9999)  
 
# reclassify the values into three groups 
# all values between 0 and 0.25 become 1, etc.
 m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
 rclmat <- matrix(m, ncol=3, byrow=TRUE)
 r4 <- reclass(r, rclmat)
 
#hist(r)
#hist(r2)
#hist(r3)
#hist(r4)
 
}
\keyword{ spatial }
