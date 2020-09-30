// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// feynman_hist
DataFrame feynman_hist(const SEXP x, const NumericVector samples_widths, const int max_nb_samples, const int verbose);
RcppExport SEXP _NeutronNoise_feynman_hist(SEXP xSEXP, SEXP samples_widthsSEXP, SEXP max_nb_samplesSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type samples_widths(samples_widthsSEXP);
    Rcpp::traits::input_parameter< const int >::type max_nb_samples(max_nb_samplesSEXP);
    Rcpp::traits::input_parameter< const int >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(feynman_hist(x, samples_widths, max_nb_samples, verbose));
    return rcpp_result_gen;
END_RCPP
}
// get_fission_chains_length
List get_fission_chains_length(int n, DataFrame fission_multiplicity, double k, int seed);
RcppExport SEXP _NeutronNoise_get_fission_chains_length(SEXP nSEXP, SEXP fission_multiplicitySEXP, SEXP kSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type fission_multiplicity(fission_multiplicitySEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(get_fission_chains_length(n, fission_multiplicity, k, seed));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_NeutronNoise_feynman_hist", (DL_FUNC) &_NeutronNoise_feynman_hist, 4},
    {"_NeutronNoise_get_fission_chains_length", (DL_FUNC) &_NeutronNoise_get_fission_chains_length, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_NeutronNoise(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}