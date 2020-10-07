


# =============================================================================
#' Generic to signal
#' 
#' Wrapper to create a signal from various data type.
#' See \code{\link[NeutronNoise]{as_signal.numeric}} and 
#' \code{\link[NeutronNoise]{as_signal.data.frame}} for specialised version.
#' 
#' 
#' @param x Data containting time detections.
#' @param x_duration Signal duration (unitless).
#' @param ... Unused.
#' 
#' @return A sorted data.table containing a TIME column.
#' 
#' @export
as_signal <- function(x, x_duration, ...)
{
  stop_if(x_duration < 0, "Negative x_duration value.")
  
  UseMethod("as_signal", x)
}

# =============================================================================
#' Numeric to signal
#' 
#' Wrapper to create a signal from a numeric vector.
#' 
#' @param x Numeric vector containting time detections.
#' @param x_duration Signal duration (unitless).
#' @param ... Unused.
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
as_signal.numeric <- function(x, x_duration, ...)
{
  stop_if(min(x) < 0, "Negative x values.")
  
  warning_if(x_duration < max(x), "x_duration does not cover all x values.")
  
  d <- data.table(TIME = x)
  
  setkey(d, "TIME")
  
  class(d) <- c("signal", "data.table", "data.frame")
  
  attr(d, "duration") <- x_duration
  
  d
}


# =============================================================================
#' data.frame to signal
#' 
#' Wrapper to create a signal from a numeric vector.
#' 
#' @param x data.frame containing at least a TIME column with numeric values.
#' @param x_duration Signal duration (unitless).
#' @param ... Unused.
#' 
#' @importFrom data.table data.table setkey setDT
#' @importFrom magrittr "%>%"
#' 
#' @examples 
#' as_signal(data.frame(TIME = runif(200, 0, 20)), x_duration = 20) %>% plot()
#' 
#' @return A sorted data.table containing a TIME column.
#' 
#' @export
as_signal.data.frame <- function(x, x_duration, ...)
{
  stop_if("TIME" %ni% colnames(x), "No TIME column.")
  
  stop_if(class(x$TIME) %ni% c("integer", "numeric"), "Wrong TIME values class.")

  stop_if(min(x$TIME) < 0, "Negative TIME values.")
  
  warning_if(x_duration < max(x$TIME), "x_duration does not cover all TIME values.")
  
  setDT(x)
  
  setkey(x, "TIME")
  
  class(x) <- c("signal", "data.table", "data.frame")
  
  attr(x, "duration") <- x_duration
  
  x
}
