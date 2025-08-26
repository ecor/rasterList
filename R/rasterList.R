NULL


#' @title Creates a \code{\link{RasterList-class}}  object
#' 
#' @description The method \code{rasterList} is the constructor of a  \code{\link{RasterList-class}} from a generic \code{object}. 
#'
#' 
#' 
#' @param object the object to coerce 
#' @param list a \code{list} object to assign to the raster map. 
#' @param object.name character string containing the name to assign to \code{object}.
#' @param FUN function that can be used to apply to each element of the list in a \code{\link{RasterList-class}}
#' @param ... further arguments for \code{\link[raster]{raster}}(generic) or \code{FUN} (\code{\link{RasterList-class}})
#' 
#' @rdname rasterList
#' @export
#' 
#' @details The argument \code{FUN} is useful to create or transform \code{\link{RasterList-class}} from other Raster* classes. 
#' 
#' @return a \code{\link{RasterList-class}} object. 
#' 
#' @importFrom raster ncell raster nlayers 
#' @importFrom methods as
#' 
#' 
#' 
#' @examples 
#'
#' f <- system.file("external/test.grd", package="raster")
#' rr <- rasterList(f) 
#' rs <- as.RasterList(f)
#' # The package-provided datasets shall be only used as example datasets. 
#' precf <- system.file("map/precipitation.grd", package="rasterList")##
#' ## A resampled preciptation raster map based on CHIRS dataset:
#' ## Funk, Chris, Pete Peterson, Martin Landsfeld, Diego Pedreros, James Verdin, 
#' ## Shraddhanand Shukla, Gregory Husak, James Rowland, Laura Harrison, 
#' ## Andrew Hoell and Joel Michaelsen.
#' ## "The climate hazards infrared precipitation with stations - a new environmental 
#' ## record for monitoring extremes". Scientific Data 2, 150066. doi:10.1038/sdata.2015.66 2015. 
#' ## http://chg.geog.ucsb.edu/data/chirps/
#' ##
#' \dontrun{
#' if (requireNamespace("lmom",quietly = TRUE) & requireNamespace("lubridate",quietly = TRUE)) {
#' ## Sample L-moments
#'  library(lmom)
#'  library(lubridate)
#'  
#' prec <- stack(precf)
#' samlmom <- stack(rasterList(prec,FUN=samlmu))
#' ## Fitting a Random Probability Distribution: it is a 'rasterList' Object
#' fitdist <- rasterList(samlmom,FUN=pelgam)
#' }
#' }
#' 
#' if (requireNamespace("lmom",quietly = TRUE) & requireNamespace("lubridate",quietly = TRUE)) {
#' library(lmom)
#' library(lubridate)
#' precf <- system.file("map/Mekrou_precipitation.grd", package="rasterList")
#' prec <- stack(precf)
#'  # Set time
#' time <- as.Date(names(prec),format="X%Y.%m.%d")
#' year <- sprintf("X%04d",lubridate::year(time)) ##as.character(time,format="X%Y")
#' 
#' ## Compute Annual Precipitation (sum aggregration)
#' yearlyprec <- stackApply(x=prec,fun=sum,indices=year)
#' ## L-moments
#' samlmom <- stack(rasterList(yearlyprec,FUN=samlmu))
#' fitdist <- rasterList(samlmom,FUN=pelgam)
#' 
#' }
#' 


#precf <- system.file("map/precipitation.grd", package="rasterList")
#prec <- stack(precf)
# 
#sdd <- stackApply(x=prec,fun=sd,indices=1)
#avg <- stackApply(x=prec,fun=mean,indices=1)
# 
#prec_std <- (prec-avg)/sdd
#prec_ks <- rasterList(prec_std,FUN=ks.test,y="pgamma",
#shape=1,rate=1) ## Complete Kolgomorov-Smirnov fitting test 
# 
#pval_ks <- raster(prec_ks,FUN=function(x){x$p.value})
# 
# 

rasterList <- function (object=NULL,...)  {
	
	
	return(standardGeneric("rasterList"))
	
}

NULL
#' @title rasterList
#' @rdname rasterList
#' @aliases rasterList 
#' @export
#' 

as.RasterList <- function(object,...) {
	
	
	return(rasterList(object,...))
	
	
}

NULL
#' @title rasterList
#' @rdname rasterList
#' @method rasterList default
#' @aliases rasterList 
#' @export


setGeneric("rasterList",function (object=NULL,list=NULL,object.name=NA,...)  {
			
		
			if (is.null(object)) {
				
				out <- raster(...)
			} else { 
			
				out <- raster(object,...)
			
			}
			
		####	out <- as(object=out,Class="RasterList")
			
			
			

			out <- rasterList(out,list=list,object.name=object.name,...)
			
			return(out)
			
			
			
		})



NULL
#' @title rasterList
#' @rdname rasterList
#' @method rasterList RasterLayer
#' @aliases rasterList 
#' @export


setMethod("rasterList","RasterLayer",function (object,list=NULL,object.name=NA,...)  {
			
			
			
			out <- as(object,Class="RasterList")
		    if (is.null(list)) {
				
				list <- as.list(object[,])
				
			}
			
			out <- rasterList(out,list=list,object.name=object.name,...)
			
			
			
			return(out)
			
			
			
		})




NULL
#' @title rasterList
#' @rdname rasterList
#' @method rasterList RasterStack
#' @aliases rasterList 
#' @export


setMethod("rasterList","RasterStack",function (object,object.name=NA,...)  {
			
			
			
			out <- as(object[[1]],Class="RasterList")
			##	out@list <- list
			##list, ...
			list <- apply(X=object[,],MARGIN=1,FUN=list)
			list <- lapply(X=list,FUN=unlist)
			
			
			
			out <- rasterList(out,list=list,object.name=object.name,...)
			
			
			
			return(out)
			
			
			
		})





NULL
#' @title rasterList
#' @rdname rasterList
#' @method rasterList RasterStack
#' @aliases rasterList 
#' @export


setMethod("rasterList","RasterBrick",function (object,object.name=NA,...)  {
			
			
			
			out <- as(object[[1]],Class="RasterList")
			##	out@list <- list
			##list, ...
			list <- apply(X=object[,],MARGIN=1,FUN=list)
			list <- lapply(X=list,FUN=unlist)
			
			
			
			out <- rasterList(out,list=list,object.name=object.name,...)
			
			
			
			return(out)
			
			
			
		})





NULL





#' @title rasterList
#' @rdname rasterList
#' @method rasterList RasterList
#' @aliases rasterList 
#' @export


setMethod("rasterList","RasterList",function (object,list=NULL,object.name=NA,FUN=NULL,...)  {
			
			
			
		###	out <- as(object,Class="Rasterlist")
			if (!is.null(list)) object@list <- list
			
			if (is.null(object.name)) object.name <- NA 
			if (length(object.name)==0) object.name <- NA 
			
			if (!is.na(object.name)) object@name <- as.character(object.name)
			if (length(object@name)==0) object@name <- as.character(object.name)
		
		
			if (length(object@list)!=0 & (length(object@list)!=ncell(object))) {
			
				
				msg <- sprintf("Mismach between length of the list and number of raster calles : %d %d in %s",length(list),ncell(object),object@name)
				print(msg)
				stop(msg)
				
			}
			
			if (length(object@list)!=0 & (!is.null(FUN))) {
				
				
				
				object@list <- lapply(X=object@list,FUN=FUN,...)	
				
			}	
			
			return(object)
			
			
			
		})

































































