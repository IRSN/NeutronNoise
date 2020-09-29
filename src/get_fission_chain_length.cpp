#include <Rcpp.h>

#include <useful.hpp>

#include <random>

using namespace Rcpp;
using namespace std;

// ================================================================================================
//' Generate virtual fission chains length
//' 
//' This function makes it possible to obtain the lengths of virtual fission chains. 
//' The length of a fission chain is defined as the total number of neutrons contained 
//' in it (this also includes the neutrons of the source event).
//' The algorithm works as follows:
//' - a first source fission is generated and eventually produces child neutrons.
//' - Each child neutron performs or not a fission with a probability and eventually produces child neutrons.
//' - The previous step is repeated until there are no more child neutrons.
//' The number of generated child neutrons depends on the multiplicity parameter.
//' Delayed neutrons are not taken into account.
//' 
//' @param n Number of virtual fission chains simulated.
//' @param fission_multiplicity A DataFrame containing the number (nu column) and the probability (pdf column) of 
//' of generated neutron per fission.
//' @param k System multiplication (between 0 and 0.99).
//' @param seed Seed of the random generator.
//' 
//' @return A list containing:
//' - the mean nu value, 
//' - the fission probability,
//' - the chains lengths.
//' 
//' @examples
//' get_fission_chains_length(fission_multiplicity = data.frame(nu = 0:2, pdf = c(0.1,0.2,0.3)), 
//' n = 5, k = 0.7)
//' 
//' @export
// [[Rcpp::export]]
List get_fission_chains_length(int n, DataFrame fission_multiplicity, double k, int seed = 1) 
{
  stop_if(n < 0, "Negative value for n");
  
  NumericVector pdf = fission_multiplicity["pdf"];
  
  stop_if(pdf.size() == 0, "No multiplcity");
  
  stop_if(min(pdf) < 0 || sum(pdf) <= 0, "Wrong values for pdf");
  
  IntegerVector nu = fission_multiplicity["nu"];
  
  stop_if(min(nu) < 0 || sum(nu) == 0, "Wrong values for nu");
  
  stop_if(unique(nu).size() != nu.size(), "Duplicate values for nu");
  
  stop_if(k > 0.99 || k < 0, "k out of range 0 < {} < 0.99", k);
  
  pdf = pdf / sum(pdf);
  
  double nu_bar = sum(pdf * as<NumericVector>(nu));
  
  stop_if(nu_bar == 0, "nu_bar == 0");
  
  double fission_probability = k / nu_bar;
  
  stop_if(fission_probability < 0 || fission_probability >= 1, "Fission probability out of range (0 < {} < 1.0).", fission_probability);
//Rcout<<fission_probability<<endl;
//Rcout<<k<<endl;
//Rcout<<nu_bar<<endl;
  IntegerVector results(n);
  
  mt19937 rnd(seed);
  
  discrete_distribution<int> f_distrib { 1 - fission_probability, fission_probability};
  
  discrete_distribution<int> nu_distrib(pdf.begin(), pdf.end());
  
  for(int i_n = 0; i_n < n; i_n ++)
  {
    int nb_curr = nu[nu_distrib(rnd)];
    int chains_length = nb_curr;
    
    while (nb_curr != 0) 
    {
      int nb_next = 0;
      
      while(nb_curr-- != 0)
      {
        check_user_interrupt(100);
        
        nb_next += f_distrib(rnd) == 1 ? nu[nu_distrib(rnd)] : 0;
      }
      
      chains_length += nb_next;
      nb_curr = nb_next;
    }
    
    results[i_n] = chains_length;
  }
  
  return List::create(_["mean_nu"] = nu_bar,
                      _["fission_probability"] = fission_probability,
                      _["lengths"] = results);
}

/*** R
# get_fission_chains_length(fission_multiplicity = data.frame(nu = 0:5, pdf = c(0.1,0.2,0.3,0.5,0.3,0.2)), n = 5, k = 0.7)
*/


