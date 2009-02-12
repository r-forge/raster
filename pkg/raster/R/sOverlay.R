

setMethod('overlay', signature(x='RasterStack', y='missing'), 
function(x, y, fun, indices=1:nlayers(x), filename="", overwrite=FALSE, filetype='raster', datatype='FLT4S', track=-1){ 

	if (missing(fun)) { 
		stop("you must supply a function 'fun'. E.g., 'fun=function(x,y){return(x+y)}'") 
	}

	warning('not implemented yet')
}
)

