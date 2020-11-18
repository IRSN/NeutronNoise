


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
  
  warning_if(x_duration < max(x), "x_duration does not cover all the signal.")
  
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
#' @param is_t0 If TRUE this means that all source events were generated at time 0. 
#' The function then distributes the histories over the specified time interval \code{x_duration}. 
#' \code{x} must contains in addition to the TIME column the HIST column (story identifier).
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
as_signal.data.frame <- function(x, x_duration, is_t0 = FALSE, ...)
{
  TIME <- NULL # devtools::check
  
  stop_if("TIME" %ni% colnames(x), "No TIME column.")
  
  stop_if(class(x$TIME) %ni% c("integer", "numeric"), "Wrong TIME values class.")
  
  stop_if(min(x$TIME) < 0, "Negative TIME values.")
  
  stop_if(x_duration <=0, "Negative or nulle value for x_duration.")
  
  setDT(x)

  if(is_t0) {

    stop_if(is_t0 && "HIST" %ni% colnames(x), "No HIST column.")
    stop_if(is_t0 && class(x$HIST) %ni% c("integer"), "Wrong HIST values class.")
    
    setkey(x, "HIST")
    signal_shift_histories(x$TIME, x$HIST, x_duration)
    setkey(x, "TIME")
  } else {
    
    warning_if(x_duration < max(x$TIME), "x_duration does not cover all TIME values. Remove beyond values.")
    
    x <- x[TIME <= x_duration]
    
    setkey(x, "TIME")
  }
    
  class(x) <- c("signal", "data.table", "data.frame")
  
  attr(x, "duration") <- x_duration
  
  x
}
