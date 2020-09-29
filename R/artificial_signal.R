

# =============================================================================
#' Artificial signal
#' 
#' Creates an artificial signal (i.e. not from sensors or simulations) 
#' with uncorrelated and/or correlated components.
#' Delayed neutrons are not take into account.
#' Signal start from time 0.
#' 
#' @param duration Signal duration (unitless).
#' @param uncorr_rate Number of uncorrelated detections per time unite.
#' @param hists_rate Number of histories per time unite. Every creted neutron create a detection.
#' @param fission_multiplicity Same argument as \code{\link[NeutronNoise]{get_fission_chains_length}}.
#' @param k Same argument as \code{\link[NeutronNoise]{get_fission_chains_length}}.
#' @param lambda Exponential decay parameter of each history. Parameter rate of \code{\link[stats]{rexp}}.
#' @param hists_id If TRUE also generate virtual history identifiers.
#' 
#' @importFrom data.table data.table setkey
#' @importFrom stats runif rexp
#' @importFrom magrittr "%>%"
#' 
#' @examples 
#' artificial_signal(duration = 10, uncorr_rate = 5, hists_rate = 20, 
#' fission_multiplicity = data.frame(nu=1:2,pdf=c(0.5,0.5)), k = 0.9, lambda = 5, hists_id  = TRUE)
#' 
#' @return A sorted data.table containing at lease a TIME column.
#' 
#' @export
artificial_signal <- function(duration, uncorr_rate = 0, hists_rate = 0, fission_multiplicity = NULL, k = 0, lambda = 1, hists_id = FALSE)
{
  stop_if(duration <= 0, "Duration <= 0");
  
  times <- c()
  
  hists <- c()
  
  if(uncorr_rate > 0) {
    
    nb_uncorr <- round(duration * uncorr_rate, 0)
    times <- runif(nb_uncorr, 0, duration)
    
    if(isTRUE(hists_id)){
      hists <-  seq_len(nb_uncorr)
    }
  }
  
  if(hists_rate > 0) {

    nb_corr <- get_fission_chains_length(n = hists_rate, fission_multiplicity = fission_multiplicity, k = k)$lengths
    times <- c(times, unlist(lapply(nb_corr, function(n) {(rexp(n, lambda) + runif(1, min = 0, max = duration)) %% duration })))
    
    if(isTRUE(hists_id)){
      hists <- c(hists, rep(seq_along(nb_corr), times = nb_corr) + max(hists, 0))
    }
  }
  
  d <- data.table(TIME = times)
  rm(times)
  
  if(isTRUE(hists_id)) {
    d$HIST <- hists
    rm(hists)
  }
  
  setkey(d, "TIME")
  
  class(d) <- c("signal", "data.table", "data.frame")
  
  d
}


