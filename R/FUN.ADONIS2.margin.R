#' @rdname pcps.sig
#' @include pcps.sig.R
#' @include matrix.p.sig.R
#' @encoding UTF-8
#' @export
FUN.ADONIS2.margin <- function(x, envir, method.p, formula, sqrt.p = TRUE, return.model = FALSE){
  p.dist <- vegan::vegdist(x, method = method.p)
  if(sqrt.p){
    p.dist <- sqrt(p.dist)
  }
  invisible(utils::capture.output(mod.obs <- vegan::adonis2(formula, data = data.frame(envir), permutations = 2, by = "margin", parallel = NULL), type = c("message")))
  nf <- length(mod.obs$F)-2
  statistic.obs <- mod.obs$F[seq_len(nf)]
  if(return.model){
    res <- list()
    res$mod.obs <- mod.obs
    res$statistic.obs <- statistic.obs
  } else{
    res <- statistic.obs
  }
  return(res)
}