# TODO: Add comment
# 
# Author: cordaem
###############################################################################


#' @title Creates a \code{\link[raster]{RasterStack-class}}  object from a \code{\link{RasterList-class}}
#' 
#' @description The method transforms a \code{\link{RasterList-class}} into a \code{\link[raster]{RasterStack-class}} in case of the list elements are numeric vectors. 
#'
#' 
#' 
#' @param x a \code{rasterList-class} object 
#' @param ... further arguments for \code{\link{rasterList}}
#' 
#' 
#' @rdname stack
#' @method stack RasterList
#' @aliases stack 
#' @export
#' 
#' @return a \code{\link[raster]{RasterStack-class}} object
#' 
#' @importFrom raster ncell stack
#' @importFrom methods as
#' 
#' @seealso \code{\link{rasterList}}
#' 
#' @examples 
#'
#' f <- system.file("external/test.grd", package="raster")
#' 
#' ## Creates a simple generic RasterList
#' 
#' rl <- rasterList(f) 
#' 
#' list <- as.list(as.vector(rl))
#' list <- lapply(X=list,FUN=function (x) {c(x,x+10,x+15)}) 
#' 
#' rl <- rasterList(rl,list=list,object.name="test")
#' 
#' ss <- stack(rl)
#' 
#' il <- 8331
#' list[[il]] <- numeric(0)
#' rla <- rasterList(rl,list=list,object.name="test2")
#' sa <- stack(rla) 


setMethod("stack", signature(x='RasterList'), 
		function(x, ...) {
			
			out <- rasterList(object=x,...)
			
			if (length(out@list)==0) {
				
				out@list <- as.list(out[,])
				
			}
	
			nn <- lapply(X=out@list,FUN=names)
			inn <- which(sapply(nn,FUN=length)>0) ## EC 20230207
			if (length(inn)>0) {
			  inn <- inn[1]
			} else {
			  inn <- 1
			}
			snn <- nn[[inn]] ## EC 20230207
			l <- out@list ##lapply(X=out@list,FUN=as.vector) ## EC 20230207
			lnl <- length(l)
			nl <- sapply(X=l,FUN=length)
	
	#		### NULL into a vector of NAs 
			inz <- which(nl!=0)
			iz <-  which(nl==0)
			
			
			if (max(nl[inz],na.rm=FALSE)!=min(nl[inz],na.rm=FALSE)){
			
				if (length(out@name)<1) out@name <- "<NO_NAME>"
				msg <- sprintf("RasterList-class Object %s cannot be coerced into a RasterStack-class object",out@name)
				
				stop(msg)
			}
		
			if (length(iz)>0) {
				
				val_null <- array(NA,nl[inz][1])
				names(val_null) <- snn ## EC 20230207
				for (i in iz){
					
					
					l[[i]] <- val_null
					
				}
				
				
			}
			
			nn <- lapply(X=l,FUN=names) ## EC 20230207
		#####nn00 <<- nn
			####snn00 <<- snn
			#####l00 <<- l
			if (!is.null(nn[[1]])) {
				
				cond_names <- all(sapply(X=nn,FUN=identical,y=snn))
			
			} else {
			
				cond_names <- FALSE
			}	
					
		
			
			l <- array(unlist(l),c(nl[inz][1],lnl))
			
			
			
			l <- apply(X=l,FUN=as.list,MARGIN=1)
			l <- lapply(X=l,FUN=unlist)
			out <- lapply(X=l,FUN=function(x,ra) { 
					o <- raster(ra) 
					o[] <- x
					
					return(o)
					},ra=out)
			
			out <- stack(out)
			
			if (cond_names==TRUE) names(out) <- snn
			
			return(out)
		}
)
