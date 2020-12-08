#include <Rcpp.h>
#include <log.hpp>
#include <useful.hpp>

using namespace Rcpp;
using namespace std;





auto feynman_v2m_sub(const NumericVector & x, const double x_duration, const double samples_width, int max_nb_samples = 0)
{
  Log::for_level(1, "Doing for samples width of {}", samples_width) ;
  
  int nb_samples = 1;                // total number of samples taken into account
  
  double gate_end = samples_width;   // end time of the current sample
  
  int nb_counts_in_sample = 0;
  
  double E1 = 0, E2 = 0, E3 = 0, E4 = 0, V = 0;
  
  auto it = x.cbegin();
  
  auto it_end = x.cend();
  
  if(max_nb_samples == 0)
    max_nb_samples = numeric_limits<int>::max();
  
  while(gate_end <= x_duration && nb_samples <= max_nb_samples)
  {
    check_user_interrupt(1000);
    
    if(it != it_end && *it <= gate_end)
    {
      nb_counts_in_sample ++;
      
      it ++;
      
      continue;
    } 
    
    // Reached and of sample
    E1 += nb_counts_in_sample;
    E2 += pow(nb_counts_in_sample, 2.);
    E3 += pow(nb_counts_in_sample, 3.);
    E4 += pow(nb_counts_in_sample, 4.);
    
    nb_counts_in_sample = 0;
    
    nb_samples ++;
    
    gate_end = samples_width * nb_samples;
  }

  if(nb_samples != max_nb_samples)
    nb_samples --;
  
  E1 /= nb_samples;
  E2 /= nb_samples;
  E3 /= nb_samples;
  E4 /= nb_samples;
  V = (E2 - pow(E1, 2.)) * (nb_samples / (nb_samples - 1.));
  
  //double fc = pow( (1 + (V/pow(E1,2) - (E3 - 3*E2*E1 + 2*pow(E1,3))/(V*E1)) / nb_samples), -1);
  double fc = 1;
  double Y = fc * V / E1 - 1;
  
  double Y_std = sqrt(
    pow(fc,2) / nb_samples * pow(V,2) / pow(E1, 2) *
      (
          (E4 - 4*E3*E1 + 8*E2*pow(E1,2) - pow(E2,2) - 4*pow(E1,4)) / pow(V,2) +
            (V / pow(E1,2)) -
            2 * (E3 - 3*E2*E1 + 2*pow(E1,3))/(V*E1) +
            2 / (nb_samples - 1) -
            pow(V/pow(E1,2) - (E3 - 3*E2*E1+2*pow(E1,3))/(V*E1),2) / nb_samples
      )
  );
  
  double Y1 = E1 / samples_width;
  
  return make_tuple(nb_samples, Y1, Y, Y_std, fc);
}

// ================================================================================================
//' Feynman Variance-to-Mean
//' 
//' Calculation of the Feynman curve from the ratio between the variance and 
//' the mean minus 1 of the number of detections in consecutive samples.
//' Variance is calculated using Bessel's correction.
//' 
//' @param x A signal. The function assumes that the signal starts at the time 0 seconds.
//' @param samples_widths Numeric vector of samples widths.
//' @param max_nb_samples Maximum number of samples to take into account for the calculation. 
//' Unlimited if 0.
//' @param verbose For debugging purpose.
//' 
//' @return A data.frame containing for each samples width :
//' - The number of samples used
//' - The count rate "Y1"
//' - The Feynamn Variance-to-Mean value "Y"
//' - The Feynman Variance-to-Mean standard deviation "Y_std"
//' 
//' @importFrom magrittr "%>%"
//' 
//' @examples
//' mls <- data.frame(nu = 0:4, pdf = c(0.1,0.3,0.35,0.4,0.2))
//' artificial_signal(1000, hists_rate = 5000, fission_multiplicity = mls, 
//' k = 0.9, lambda = 10) %>%
//' feynman_v2m(samples_widths = lseq(from = 0.001, to = 10, 50)) %>% plot()
//' 
//' @export
// [[Rcpp::export]]
DataFrame feynman_v2m(DataFrame x, 
                      NumericVector samples_widths, 
                      int max_nb_samples = 0, 
                      int verbose = 0)
{
  Log::set_threshold(verbose);
  
  const NumericVector xx = x["TIME"];
  
  stop_if(xx.size() == 0 || is_sorted(xx) == false || xx[0] < 0, "x is empty or not sorted or not positive.");
  
  stop_if(samples_widths.size() == 0 || min(samples_widths) <= 0, "samples_widths is empty or not strictly positive.");
  
  stop_if(max_nb_samples < 0, "Negtive value for max_nb_samples.");
  
  double x_duration = x.attr("duration");
  
  stop_if(x_duration <= 0, "Negative or zero value for signal duration.");
  
  warning_if(x_duration < xx[xx.size() - 1], "x_duration does not cover all the signal.");
  
  vector<double> vY1, vY, vY_std, vfc;
  vector<int> vnb_samples;
  
  for(auto samples_width : samples_widths)
  {
    auto [nb_samples, Y1, Y, Y_std, fc] = feynman_v2m_sub(xx, x_duration, samples_width, max_nb_samples);
    
    vnb_samples.push_back(nb_samples);
    vY1.push_back(Y1);
    vY.push_back(Y);
    vY_std.push_back(Y_std);
    vfc.push_back(fc);
  }
  
  DataFrame ret = DataFrame::create(_["samples_widths"] = samples_widths,
                                    _["nb_samples"] = vnb_samples,
                                    _["Y1"] = vY1,
                                    _["Y"] = vY,
                                    _["Y_std"] = vY_std,
                                    _["fc"] = vfc);
  
  ret.attr("class") = CharacterVector::create ("feynman_v2m", "data.frame");
  
  return ret;
}



/*** R

*/
