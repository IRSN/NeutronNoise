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
                           const double* x_end, 
                           double samples_width, 
                           int max_nb_samples, 
                           int verbose)
{
  int nb_samples = 0;
  double gate_end = samples_width;
  bool finish = false;
  auto x_it = x_begin;
  IntegerHist h;
  
  while(x_it != x_end && finish == false && RcppThread::isInterrupted() == false) 
  {
    
    while (*x_it > gate_end && finish == false)
    {
      h.commit();
      
      nb_samples ++;
      
      if(max_nb_samples != 0 && nb_samples == max_nb_samples) 
      {
        finish = true;
        continue;
      }
      
      gate_end = samples_width * (nb_samples + 1);
    }
    
    h();
    
    x_it ++;
  }
  
  // On ne compte que si on n'est pas arrivé au bout
  // pour ne pas prendre en compte un échantillon non entier.
  /*if(x_it != x_end)
  {
    h.commit();
    nb_samples ++;
  }*/
  
  
  FeynmanHistReturn ret;
  ret.samples_width = samples_width;
  ret.nb_samples = nb_samples;
  
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
//' This function divide the signal into equal samples of width \code{T}. 
//' For each of them the number of events are counted and binned into a histogram called the Feynman histogram.
//' This histogram therefore represents the occurrence probabilities of various multiplets (i.e. 1 detection, 2 detections, etc.) occuring within
//' a specified time gate width \code{T}.
//' 
//' To avoid numerical problem with functions using \code{feynman_hist} the last sample is not taken into account.
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
//' @examples
//' hs <- feynman_hist(sort(runif(0,10, n=10000)), c(0.11, 0.33, 0.58))
//' plot(hs)
//' feynman_hist(data.frame(TIME=sort(runif(0,10, n=10000))), c(0.11, 0.33, 0.58)) %>% plot()
//' artificial_signal(1000, 5) %>% feynman_hist(samples_widths = 0.8) %>% plot()
//' 
//' @seealso \link[NeutronNoise]{plot.feynman_hist} for ploting the result.
//' @export
// [[Rcpp::export]]
DataFrame feynman_hist(const SEXP x, 
                       const NumericVector samples_widths, 
                       const int max_nb_samples = 0,
                       const int verbose = 0)
{
  Log::set_threshold(verbose);
  
  auto get_x_arg=[&x](){
    if(TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP)
    {
      return as<NumericVector>(x);
    } else if(TYPEOF(x) == VECSXP) {
      return as<NumericVector>((as<DataFrame>(x))["TIME"]);
    }
    stop("Bad x type");
  };
  
  const NumericVector xx = get_x_arg();
  
  stop_if(samples_widths.size() == 0, "samples_widths is empty.");
  
  stop_if(min(samples_widths) <= 0, "samples_widths not strictly postive.");
  
  stop_if(xx.size() == 0, "x is empty.");
  
  stop_if(is_sorted(xx) == false || xx[0] < 0, "x must be positive and sorted.");
  
  stop_if(max_nb_samples < 0, "max_nb_samples is negative.");
  
  RcppThread::ThreadPool pool;
  
  vector<future<FeynmanHistReturn>> futures(samples_widths.size());
  
  for(int i = 0; i < samples_widths.size(); i++)
  {
    futures[i] = pool.pushReturn(feynman_hist_sub_task, cbegin(xx), cend(xx), samples_widths[i], max_nb_samples, verbose);
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
