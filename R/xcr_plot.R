

#' Plot count rates
#' 
#' Plot count rates from \code{\link[NeutronNoise]{xcr}} function. The assumed time unit is second.
#' Plot 4 graphs for Y1, Y2, Y3 and Y.
#' 
#' @param x data.frame from \link[NeutronNoise]{xcr} functions.
#' @param ... Unused.
#' 
#' @return ggplot object.
#' 
#' @importFrom magrittr "%>%"
#' 
#' @examples
#' artificial_signal(1000, 5) %>% feynman_hist(samples_widths = c(0.8, 0.9)) %>% xcr() %>% plot()
#' 
#' @import ggplot2
#' @export
plot.xcr <- function(x, ...) {
  
  samples_width <- Y1 <- Y1_std <- Y2 <- Y2_std <- Y3 <- Y3_std <- Y <- NULL # devtools::check
  
  g1 <- ggplot(x, aes(x = samples_width, y = Y1)) + 
    labs(x = "Samples widths [s]", y = "Y1")+
    geom_line() + scale_x_log10() + theme_bw() +
    geom_errorbar(aes(ymin = Y1 - Y1_std, ymax = Y1 + Y1_std))
  
  g2 <- ggplot(x, aes(x = samples_width, y = Y2)) +
    labs(x = "Samples widths [s]", y = "Y2")+
    geom_line() + scale_x_log10() + theme_bw() +
    geom_errorbar(aes(ymin = Y2 - Y2_std, ymax = Y2 + Y2_std))
  
  g3 <- ggplot(x, aes(x = samples_width, y = Y3)) +
    labs(x = "Samples widths [s]", y = "Y3")+
    geom_line() + scale_x_log10() + theme_bw() +
    geom_errorbar(aes(ymin = Y3 - Y3_std, ymax = Y3 + Y3_std))
  
  g4 <- ggplot(x, aes(x = samples_width, y = Y)) +
    labs(x = "Samples widths [s]", y = "Y")+
    geom_line() + scale_x_log10() + theme_bw() +
    geom_errorbar(aes(ymin = Y - Y_std, ymax = Y + Y_std))
  
  print(g1)
  print(g2)
  print(g3)
  print(g4)
}