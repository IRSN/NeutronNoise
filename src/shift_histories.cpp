#include "useful.hpp"

using namespace Rcpp;

// [[Rcpp::export]]
void signal_shift_histories(NumericVector times,
                            IntegerVector hists,
                            double duration) {
  
  stop_if(hists.size() != times.size(), "Number of histories != number of times");
  
  if(times.size() == 0)
    return;
  
  stop_if(is_sorted(hists) == false, "Histories not sorted");
  
  auto h = hists[0];
  
  double offset = runif(1, 0, duration)[0];
  
  for(int i = 0; i < times.size(); i++)
  {
    if(hists[i] != h)
    {
      h = hists[i];
      offset = runif(1, 0, duration)[0];
    }
    times[i] = fmod(times[i] + offset, duration);
  }
}

