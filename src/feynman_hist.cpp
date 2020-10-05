#include <Rcpp.h>
#include <RcppThread.h>
#include <useful.hpp>
#include <IntegerHist.hpp>
#include <log.hpp>
#include <tuple>

using namespace Rcpp;
using namespace std;

struct FeynmanHistReturn
{
  double samples_width;
  
  int nb_samples;
  
  vector<int> multiplet;
  
  vector<int> frequency;
};


// ============================================================================
//
//
auto feynman_hist_sub_task(const double *x_begin, 
                           const double *x_end, 
                           const double x_duration,
                           const double samples_width, 
                           const unsigned int max_nb_samples, 
                           const int verbose)
{
  double gate_end = samples_width;
  bool finish = false;
  auto x_it = x_begin;
  IntegerHist h;
  
  
  
  while(x_it != x_end && gate_end <= x_duration && h.get_nb_commit() != max_nb_samples && RcppThread::isInterrupted() == false)
  {
    if(*x_it > gate_end)
    {
      h.commit();
      
      gate_end = samples_width * (h.get_nb_commit() + 1);
      
    } else {
      
      h();
      
      x_it ++;
    }
  }
  
  
  if(x_it == x_end)
  {
    while(gate_end <= x_duration)
    {
      h.commit();
      
      gate_end = samples_width * (h.get_nb_commit() + 1);
    }
  }
  
  
  FeynmanHistReturn ret;
  ret.samples_width = samples_width;
  ret.nb_samples = h.get_nb_commit();
  
  auto & d = h.get_data();
  
  for(unsigned int i = 0; i < d.size(); i++)
  {
    if(d[i] != 0)
    {
      ret.multiplet.push_back(i);
      ret.frequency.push_back(d[i]);
    }
  }
  
  return ret;
}

// ============================================================================
//' Feynman histogram
//' 
//' @description
//' This function divide the signal into equal samples of width \code{T} up to the signal duration (\code{attr(s, "duration")}). 
//' For each of them the number of events are counted and binned into a histogram called the Feynman histogram.
//' This histogram therefore represents the occurrence probabilities of various multiplets (i.e. 1 detection, 2 detections, etc.) occuring within
//' a specified time gate width \code{T}.
//' 
//' If the upper limit of the last sample exceeds the value \code{x_duration} it is not taken into account.
//' 
//' This function uses all available cores of the computer.
//' 
//' @param x Either a sorted numeric vector representing the detection times of a data.frame with a sorted TIME column.
//' The function assumes that the signal starts at the time 0 seconds. 
//' @param samples_widths Numeric vector of samples width (multiple values of \code{T}).
//' @param max_nb_samples If different from 0 then the calculation is limited to the specified number of samples. 
//' @param verbose For debbuging purpose only.
//' @return A data.frame.
//' 
//' @importFrom magrittr "%>%"
//' 
//' @examples
//' artificial_signal(1000, 5) %>% feynman_hist(samples_widths = 0.8) %>% plot()
//' 
//' @seealso \link[NeutronNoise]{plot.feynman_hist} for ploting the result.
//' @export
// [[Rcpp::export]]
DataFrame feynman_hist(const DataFrame x,
                       const NumericVector samples_widths, 
                       int max_nb_samples = 0,
                       const int verbose = 0)
{
  Log::set_threshold(verbose);
  
  /*auto get_x_arg=[&x](){
    if(TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP)
    {
      return as<NumericVector>(x);
    } else if(TYPEOF(x) == VECSXP) {
      return as<NumericVector>((as<DataFrame>(x))["TIME"]);
    }
    stop("Bad x type");
  };
  
  const NumericVector xx = get_x_arg();
  */
  
  const NumericVector xx = x["TIME"];
  
  stop_if(samples_widths.size() == 0 || min(samples_widths) <= 0, "samples_widths is empty or not strictly positive.");
  
  stop_if(xx.size() == 0 || is_sorted(xx) == false || xx[0] < 0, "x is empty or not sorted or not positive.");
  
  stop_if(max_nb_samples < 0, "max_nb_samples is negative.");
  
  double x_duration = x.attr("duration");
  
  warning_if(x_duration < xx[xx.size() - 1], "x_duration does not cover all the signal.");
  
  max_nb_samples = max_nb_samples == 0 ? numeric_limits<int>::max() : max_nb_samples;
  
  RcppThread::ThreadPool pool;
  
  vector<future<FeynmanHistReturn>> futures(samples_widths.size());
  
  for(int i = 0; i < samples_widths.size(); i++)
  {
    futures[i] = pool.pushReturn(feynman_hist_sub_task, cbegin(xx), cend(xx), x_duration, samples_widths[i], max_nb_samples, verbose);
  }
  
  vector<double> ret_samples_widths;
  vector<int> ret_nb_samples;
  vector<int> ret_multiplet;
  vector<int> ret_frequency;
  
  for(auto & future : futures)
  {
    auto a = future.get();
    
    ret_samples_widths.insert(ret_samples_widths.end(), a.multiplet.size(), a.samples_width);
    
    ret_nb_samples.insert(ret_nb_samples.end(), a.multiplet.size(), a.nb_samples);
    
    ret_multiplet.insert(ret_multiplet.end(), a.multiplet.begin(), a.multiplet.end());
    
    ret_frequency.insert(ret_frequency.end(), a.frequency.begin(), a.frequency.end());
  }
  
  pool.join();
  
  
  
  DataFrame ret = DataFrame::create(_["samples_width"] = ret_samples_widths,
                                    _["nb_samples"] = ret_nb_samples,
                                    _["multiplet"] = ret_multiplet,
                                    _["frequency"] = ret_frequency);
  
  ret.attr("class") = CharacterVector::create ("feynman_hist", "data.frame");
  
  return ret;
}




/*** R
#
*/
