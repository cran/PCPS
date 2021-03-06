#' @rdname pcps.curve
#' @encoding UTF-8
#' @export
print.pcpscurve<-function(x, ...){
	res <- summary.pcpscurve(x)
	cat("Call:\n")
	cat(deparse(x$call), "\n\n")
	if(length(x$list.warning)>0){
	  cat("List of warning:\n")
	  namestemp <- SYNCSA::CollectNames(x$list.warning, prefix = "$list.warning$")
	  cat(unlist(namestemp, use.names = FALSE), sep = "\n")
	}
	cat("PCPS curve observed:\n")
	print(res$curve.obs)
	if(!is.null(x$curve.null.ts)){
		cat("\n")
		cat("Mean PCPS curve model TS:\n")
		print(as.matrix(res$null.model.ts[,1:2]))
	}
	if(!is.null(x$curve.null.bm)){
		cat("\n")
		cat("Mean PCPS curve model BM:\n")
		print(as.matrix(res$null.model.bm[, 1:2]))
	}
	invisible(x)
}