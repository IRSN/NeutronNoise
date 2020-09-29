
# =================================================================================================
#' Plot a signal
#' 
#' Plot a signal. The assumed time unit is second.
#' 
#' @param x A data.frame from signal function like \link[NeutronNoise]{artificial_signal}.
#' @param binwidth Same argument as \link[ggplot2]{geom_histogram}.
#' @param ... Unused.
#' 
#' @return ggplot object.
#' 
#' @examples
#' artificial_signal(duration = 1000, uncorr_rate = 5) %>% plot()
#' 
#' @import ggplot2
#' @export
plot.signal <- function(x, binwidth = 1, ...) {
      
  ggplot(x, mapping = aes_string(x = "TIME")) +
    labs(x = "Time [s]", y = 'Freqency')+
    theme_bw()+
    geom_histogram(binwidth = binwidth)
  
}
