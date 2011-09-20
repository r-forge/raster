

setClass ('BigRasterLayer',
	contains = 'RasterLayer',
	representation (
		bigtrix = "big.matrix"
		),
	prototype (
		)
	)
	
	

	
setClass ('BigRasterBrick',
	contains = 'RasterBrick',
	representation (
		bigtrix = "big.matrix"
		),
	prototype (
		)
	)
	
	
	