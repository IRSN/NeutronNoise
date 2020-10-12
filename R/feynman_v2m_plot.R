


# =================================================================================================
#' Plot Feynman Variance-to-Mean curve
#' 
#' Plot Feynman Variance-to-Mean curve from \code{\link[NeutronNoise]{feynman_v2m}} function. 
#' The assumed time unit is second.
#' 
#' @param x data.frame from \link[NeutronNoise]{feynman_v2m} functions.
#' @param ... Unused.
#' 
#' @return ggplot object.
#' 
#' @importFrom magrittr "%>%"
#' 
#' @examples
#' mls <- data.frame(nu = 0:4, pdf = c(0.1,0.3,0.35,0.4,0.2))
#' artificial_signal(1000, hists_rate = 5000, 
#'                   fission_multiplicity = mls, k = 0.9, lambda = 10) %>%
#' feynman_v2m(samples_widths = lseq(from = 0.001, to = 10, 50)) %>% plot()
#' 
#' @import ggplot2
#' @export
plot.feynman_v2m <- function(x, ...) {
  
  ggplot(mapping = aes(x = x$samples_widths, y = x$Y)) +
    labs(x = "Samples widths [s]", y = 'Y')+
    theme_bw()+
    scale_x_log10()+
    geom_line()
  
}