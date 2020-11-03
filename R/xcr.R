

# =================================================================================================
#' Calculate single counte rate, double counte rate and triple counte rate
#' 
#' @description Thoses count rates are calculated base on the feynman histograms obtain from 
#'  \code{\link[NeutronNoise]{feynman_hist}} function. 
#' The assumed time unit is second.
#' 
#' The calculated values are:
#' 
#' -  Y1: single count rate
#' -  Y2: double count rate
#' -  Y3: triple count rate
#' -  Y1_std: standard deviation of Y1
#' -  Y2_std: standard deviation of Y2
#' -  Y3_std: standard deviation of Y3
#' -  Y: Feynman value
#' -  Y_std: standard deviation of Y
#' 
#' @param x feynman_hist object from \link[NeutronNoise]{feynman_hist} functions.
#' 
#' @return xcr object which is a tibble object.
#' 
#' @importFrom magrittr "%>%"
#' 
#' @examples
#' artificial_signal(1000, 5) %>% feynman_hist(samples_widths = c(0.8, 0.9)) %>% xcr()
#' 
#' @import data.table
#' @export
xcr <- function(x)
{
  
  # sub_xcr <- function(samples_width, multiplet, frequency)
  # {
  #   n <- multiplet
  #   cn <- frequency
  #   sw <- unique(samples_width)
  #   
  #   N <- sum(cn)
  #   m1 <- sum( n * cn ) / N
  #   m2 <- sum( n * (n-1) * cn ) / (2 * N)  # m2<-var(b)
  #   m3 <- sum( n * (n-1) * (n-2) * cn ) / (6 * N)
  #   m4 <- sum( n * (n-1) * (n-2) * (n-3) * cn ) / (24 * N)
  #   m5 <- sum( n * (n-1) * (n-2) * (n-3) * (n-4) * cn ) / (120 * N)
  #   m6 <- sum( n * (n-1) * (n-2) * (n-3) * (n-4) * (n-5) * cn ) / (720 * N)
  #   
  #   # Single, Double and Triple counting rate
  #   Y1 <- 1/sw * (m1)
  #   Y2 <- 1/sw * (m2 - m1^2/2)
  #   Y3 <- 1/sw * (m3 - m2*m1 + m1^3/3)
  #   
  #   denom <- 1 / (sqrt(N - 1) * sw)
  #   Y1_std <- denom * sqrt(2*m2 + m1 - m1^2)
  #   Y2_std <- suppressWarnings(denom * sqrt(6*m4 + 6*m3 + m2 - m2^2 + 4*m2*m1^2 + m1^3 - m1^4 - 6*m3*m1 - 4*m2*m1 ))
  #   Y3_std <- suppressWarnings(denom * sqrt(20*m6 + 30*m5 + 12*m4 + m3 - m3^2 + 2*m2^3 + m1^5 - m1^6 - 20*m5*m1
  #                                           - 8*m4*m2 - 24*m4*m1 + 14*m4*m1^2 - 6*m3*m2 - 6*m3*m1 + 12*m3*m1^2 - 8*m3*m1^3
  #                                           + 5*m2^2*m1 + m2*m1^2 - 6*m2*m1^3 + 6*m2*m1^4 -8*m2^2*m1^2 + 10*m3*m2*m1))
  #   
  #   # Feynman-Y value for sample_width
  #   Y <- 2 * Y2 / Y1
  #   Y_std <- 2 * sqrt (m2/2 + m1/4 - m1^2/4 + 2*m2^2/m1^2 - m2/m1 + 2*m2^3/m1^4
  #                      -3*m2^2/m1^3 + 6*m4/m1^2 + 6*m3/m1^2 + m2/m1^2 - 3*m3/m1 - 6*m2*m3/m1^3) / sqrt(N - 1)
  #   
  #   tibble(samples_width = sw,
  #        Y1 = Y1, Y2 = Y2, Y3 = Y3,
  #        Y1_std = Y1_std, Y2_std = Y2_std, Y3_std = Y3_std,
  #        Y = Y, Y_std = Y_std)
  # }
  # 
  # ret <- x %>% group_by(samples_width) %>% summarise(sub_xcr(samples_width, multiplet, frequency), .groups = "drop_last")
  
  # class(ret) <- c("xcr", "tbl_df", "tbl", "data.frame")
  
  
  
  ret <- rbindlist(lapply(split(x, x$samples_width), function(h){
    
    # Reduced factorial moment values and std
    n <- h$multiplet
    cn <- h$frequency
    samples_width <- unique(h$samples_width)
    
    N <- sum(cn)
    m1 <- sum( n * cn ) / N
    m2 <- sum( n * (n-1) * cn ) / (2 * N)  # m2<-var(b)
    m3 <- sum( n * (n-1) * (n-2) * cn ) / (6 * N)
    m4 <- sum( n * (n-1) * (n-2) * (n-3) * cn ) / (24 * N)
    m5 <- sum( n * (n-1) * (n-2) * (n-3) * (n-4) * cn ) / (120 * N)
    m6 <- sum( n * (n-1) * (n-2) * (n-3) * (n-4) * (n-5) * cn ) / (720 * N)
    
    # Single, Double and Triple counting rate
    Y1 <- 1/samples_width * (m1)
    Y2 <- 1/samples_width * (m2 - m1^2/2)
    Y3 <- 1/samples_width * (m3 - m2*m1 + m1^3/3)
    
    denom <- 1 / (sqrt(N - 1) * samples_width)
    Y1_std <- denom * sqrt(2*m2 + m1 - m1^2)
    Y2_std <- suppressWarnings(denom * sqrt(6*m4 + 6*m3 + m2 - m2^2 + 4*m2*m1^2 + m1^3 - m1^4 - 6*m3*m1 - 4*m2*m1 ))
    Y3_std <- suppressWarnings(denom * sqrt(20*m6 + 30*m5 + 12*m4 + m3 - m3^2 + 2*m2^3 + m1^5 - m1^6 - 20*m5*m1
                                            - 8*m4*m2 - 24*m4*m1 + 14*m4*m1^2 - 6*m3*m2 - 6*m3*m1 + 12*m3*m1^2 - 8*m3*m1^3
                                            + 5*m2^2*m1 + m2*m1^2 - 6*m2*m1^3 + 6*m2*m1^4 -8*m2^2*m1^2 + 10*m3*m2*m1))
    
    # Feynman-Y value for sample_width
    Y <- 2 * Y2 / Y1
    Y_std <- 2 * sqrt (m2/2 + m1/4 - m1^2/4 + 2*m2^2/m1^2 - m2/m1 + 2*m2^3/m1^4
                       -3*m2^2/m1^3 + 6*m4/m1^2 + 6*m3/m1^2 + m2/m1^2 - 3*m3/m1 - 6*m2*m3/m1^3) / sqrt(N - 1)
    
    list(samples_width = samples_width,
         Y1 = Y1, Y2 = Y2, Y3 = Y3,
         Y1_std = Y1_std, Y2_std = Y2_std, Y3_std = Y3_std,
         Y = Y, Y_std = Y_std)
    
  }))
  
  class(ret) <- c("xcr", class(ret))
  
  ret
}