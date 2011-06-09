# Oscar Perpiñan Lamigueiro

#setOldClass('Date')
#setOldClass('POSIXct')
#setOldClass('yearmon')
#setOldClass('yearqtr')
#setClassUnion('timeExtended', c('Date','POSIXct', 'yearmon', 'yearqtr'))


setClass('RasterLayerTime',
	contains='RasterLayer',
    representation(
		time='list'
	) ,
	validity=function(object){
        if (length(object@list[[1]]) == 1) {
			TRUE
		} else {
			cat('Length of values in the time slot is different from number of layers.\n')
			FALSE
        }	
	}
)


setClass('RasterStackTime',
	contains='RasterStack',
    representation(
		time='list'
	) ,
	validity=function(object){
        if (length(object@list[[1]]) == 1) {
			TRUE
		} else {
			cat('Length of values in the time slot is different from number of layers.\n')
			FALSE
        }	
	}
)


setClass('RasterBrickTime',
	contains='RasterBrick',
    representation(
		time='list'
	) ,
	validity=function(object){
        if (length(object@list[[1]]) == 1) {
			TRUE
		} else {
			cat('Length of values in time slot is different from number of layers.\n')
			FALSE
        }	
	}
)


setClassUnion("RasterStackBrickTime", c("RasterStackTime", "RasterBrickTime"))


setMethod('show', 'RasterStackTime',
    function(object){
    callNextMethod()
    cat('time        :\n')
    print(summary(object@time[[1]]))
})

setMethod('show', 'RasterBrickTime',
    function(object){
    callNextMethod()
    cat('time        :\n')
    print(summary(object@time[[1]]))
})

setMethod('show', 'RasterLayerTime',
    function(object){
    callNextMethod()
    cat('time        :\n')
    print(summary(object@time[[1]]))
})




#if (!isGeneric("timeApply")) {
#	setGeneric("timeApply", function(x, ...)
#		standardGeneric("timeApply"))}	
		   
#setMethod('timeApply', signature(x='RasterStackBrickTime'), 
 .timeApply <- function(x, by, fun, ...){
        ##from aggregate.zoo
        my.unique <- function(x) x[match(x, x) == seq_len(length(x))] 
		my.sort <- function(x) x[order(x)]
        if (is.function(by)) { by <- by(x@time[[1]]) }
        ##stopifnot(length(time(x)) == length(by))
        b <- stackApply(x, as.numeric(factor(by)), match.fun(fun))
        b <- as(b, 'RasterBrickTime')
        time <- my.sort(my.unique(by))
        b@time <- time
        layerNames(b) <- as.character(time)
        b
    }
#)

