

# =============================================================================
#' Raw signal
#' 
#' Wrapper to create a signal from a numeric vector.
#' 
#' @param x Numeric vector containting time detection.
#' @param x_duration Signal duration
#' 
#' @importFrom data.table data.table setkey
#' @importFrom magrittr "%>%"
#' 
#' @examples 
#' as_signal(runif(200, 0, 20), x_duration = 20) %>% plot()
#' 
#' @return A sorted data.table containing a TIME column.
#' 
#' @export
as_signal <- function(x, x_duration)
{
  stop_if(x_duration <= 0, "Duration <= 0");
  
  warning_if(x_duration < max(x), "x_duration does not cover all x values.")
  
  d <- data.table(TIME = x)
  
  setkey(d, "TIME")
  
  class(d) <- c("signal", "data.table", "data.frame")
  
  attr(d, "duration") <- x_duration
  
  d
}

