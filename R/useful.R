
# https://www.r-bloggers.com/developing-an-r-package-from-scratch-with-travis-continuous-integration/
# https://stateofther.github.io/finistR2018/atelier2_rcpp_modules.html
# https://r-pkgs.org/
# https://cran.r-project.org/doc/manuals/R-exts.html
# https://blog.methodsconsultants.com/posts/developing-r-packages-with-usethis-and-gitlab-ci-part-ii/
# http://dirk.eddelbuettel.com/blog/2017/06/13/  
# https://towardsdatascience.com/travis-ci-for-r-advanced-guide-719cb2d9e0e5#5da6

#' @import ggplot2
#' @useDynLib NeutronNoise
#' @importFrom Rcpp sourceCpp
NULL


# =============================================================================
#' Logarithmic sequence
#' 
#' Code taken from the "emdbook" package. Create a logarithmic sequence between \code{from} and \code{to} of size \code{length}.
#' 
#' @param from Lower limit.
#' @param to Upper limit.
#' @param length Sequence length.
#' 
#' @examples 
#' lseq(1, 10, 7)
#' 
#' @return Numeric vector
#' @export
lseq <- function(from, to, length) {
  exp(seq(log(from), log(to), length.out = length))
}


stop_if <- function(x, ...){
  if (x) stop(sprintf(...), call. = FALSE)
}
