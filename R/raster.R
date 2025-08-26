# TODO: Add comment
# 
# Author: cordaem
###############################################################################
NULL
#' \code{Raster} methods for a \code{\link{RasterList-class}} object.
#'
#' @param x a valid \code{\link{RasterList-class}} object
#' @param FUN if it not \code{NULL} a function is applied to all elements of the \code{list} slot in \code{x}. 
#' @param ... further arguments 
#' 
#' @export 
#'
#' @return a \code{\link[raster]{RasterLayer-class}} object
#'
#' @rdname raster
#' @method raster RasterList
#' @aliases raster 
#' @importFrom methods is
#' @seealso \code{\link{stack}},\code{\link{RasterListApply}}
#' @examples 
#' 
#' f <- system.file("external/test.grd", package="raster")
#' ur <- rasterList(raster(f),FUN=function(x,d){x+0:d},d=10)
#' 
#' r1 <- raster(ur)
#' r2 <- raster(ur,FUN=function(x){x[2]})
#' 

# RasterListApply
# ## TEST RASTER 
# 
# 
# 


setMethod('raster', signature(x='RasterList'), 
		function(x,FUN=NULL,...) {
			
		
			x <- rasterList(object=x,FUN=FUN,...)
			if (length(x@list)==0) {
				
				x@list <- as.list(x[,])
				
			}
			sout <- sapply(X=x@list,FUN=function(x){
						
						o <- try(as.numeric(as.vector(x))[1],silent=TRUE)
						if (is(o,"try-error")) o <- 1*NA
						return(o)
					
					}
						
			)
			x <- as(x,"RasterLayer")
			
			x[] <- sout
			return(x)
#			if (is.null(FUN)) {
#			
#				out <- as(x,"RasterLayer")
#			
#			} else { 
#				
#				out <- x
#				if (length(x@list)==0) {
#					
#					out@list <- as.list(x[,])
#					
#				}
#				
#				####out <- rasterList(object=out,FUN=FUN,...)
#				
#				sout <- sapply(X=out@list,FUN=function(x){(as.numeric(as.vector(x))[1])})
#				
#				out <- as(out,"RasterLayer")
#				
#				out[] <- sout
#				
#				
#			}
#			return(out)
		}
)
