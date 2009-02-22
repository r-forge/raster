
if (!isGeneric("ncell")) {
	setGeneric("ncell", function(x)
		standardGeneric("ncell"))
}	

setMethod('ncell', signature(x='ANY'), 
	function(x) {
		d <- dim(x)
# return numeric to avoid integer overflow
		return(as.numeric(d[1]) * d[2])
	}
)

