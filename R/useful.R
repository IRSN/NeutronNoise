
# https://www.r-bloggers.com/developing-an-r-package-from-scratch-with-travis-continuous-integration/
# https://stateofther.github.io/finistR2018/atelier2_rcpp_modules.html
# https://r-pkgs.org/
# https://cran.r-project.org/doc/manuals/R-exts.html
# https://blog.methodsconsultants.com/posts/developing-r-packages-with-usethis-and-gitlab-ci-part-ii/
# http://dirk.eddelbuettel.com/blog/2017/06/13/  
# https://towardsdatascience.com/travis-ci-for-r-advanced-guide-719cb2d9e0e5#5da6
# https://sahirbhatnagar.com/blog/2020/03/03/creating-a-website-for-your-r-package/

# Sticker generation
# hexSticker::sticker("man/figures/sticker_base.png", package="NeutronNoise", p_size=6.5, s_x=0.95, s_y=0.9, s_width=0.9, p_color="black", h_fill ="grey", h_color="red", filename = "man/figures/sticker.png", dpi=100, spotlight = T, l_y=0.2)

# Avant chaque publication :
# devtools::test()
# devtools::test_coverage()
# pkgdown::build_site()
# Création du README.md (clique knit)
# Vérification de DESCRIPTION (version du package)
# devtools::check()
# Incrémentation de la versin dév
# usethis::use_dev_version()

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

warning_if <- function(x, ...){
  if (x) warning(sprintf(...), call. = FALSE)
}


'%ni%' <- function(x,y) {!('%in%'(x,y)) }

