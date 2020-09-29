

#' Plot feynman histograms
#' 
#' Plot feynman histograms from \code{\link[NeutronNoise]{feynman_hist}} function. The assumed time unit is second.
#' 
#' @param x data.frame from \link[NeutronNoise]{feynman_hist} functions.
#' @param ... Unused.
#' 
#' @return ggplot object.
#' 
#' @importFrom magrittr "%>%"
#' 
#' @examples
#' hs <- feynman_hist(sort(runif(0,10, n=10000)), c(0.11, 0.33, 0.58))
#' plot(hs)
#' 
#' @import ggplot2
#' @export
plot.feynman_hist <- function(x, ...) {
  
  ggplot(mapping = aes(x = x$multiplet, y = x$frequency, color = as.factor(x$samples_width))) +
    labs(x = "Multiplet", y = 'Freqency', color = "Samples widths [s]")+
    theme_bw()+
    theme(legend.position = c(0.8, 0.8))+
    scale_y_log10()+
    geom_step()
  
}