// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_AdaptR
int rcpp_AdaptR(std::string PfileName);
RcppExport SEXP _AdaptR_rcpp_AdaptR(SEXP PfileNameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type PfileName(PfileNameSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_AdaptR(PfileName));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_MuruCompactor
int rcpp_MuruCompactor(std::string PfileName);
RcppExport SEXP _AdaptR_rcpp_MuruCompactor(SEXP PfileNameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type PfileName(PfileNameSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_MuruCompactor(PfileName));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_AdaptR_rcpp_AdaptR", (DL_FUNC) &_AdaptR_rcpp_AdaptR, 1},
    {"_AdaptR_rcpp_MuruCompactor", (DL_FUNC) &_AdaptR_rcpp_MuruCompactor, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_AdaptR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
