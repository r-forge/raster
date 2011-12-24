# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3

if (!isGeneric("union")) {
	setGeneric("union", function(x, y)
		standardGeneric("union"))
}	

setMethod('union', signature(x='Extent', y='Extent'), 
function(x, y) { 
	unionExtent(x, y)
} )

