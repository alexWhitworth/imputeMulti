// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// count_compare
IntegerVector count_compare(IntegerMatrix& x, IntegerMatrix& dat, const std::string& hasNA);
RcppExport SEXP _imputeMulti_count_compare(SEXP xSEXP, SEXP datSEXP, SEXP hasNASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix& >::type dat(datSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type hasNA(hasNASEXP);
    rcpp_result_gen = Rcpp::wrap(count_compare(x, dat, hasNA));
    return rcpp_result_gen;
END_RCPP
}
// supDistC
double supDistC(const NumericVector& x, const NumericVector& y);
RcppExport SEXP _imputeMulti_supDistC(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(supDistC(x, y));
    return rcpp_result_gen;
END_RCPP
}
// xy_compare
List xy_compare(IntegerMatrix& mat_x, IntegerMatrix& mat_y);
RcppExport SEXP _imputeMulti_xy_compare(SEXP mat_xSEXP, SEXP mat_ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix& >::type mat_x(mat_xSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix& >::type mat_y(mat_ySEXP);
    rcpp_result_gen = Rcpp::wrap(xy_compare(mat_x, mat_y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_imputeMulti_count_compare", (DL_FUNC) &_imputeMulti_count_compare, 3},
    {"_imputeMulti_supDistC", (DL_FUNC) &_imputeMulti_supDistC, 2},
    {"_imputeMulti_xy_compare", (DL_FUNC) &_imputeMulti_xy_compare, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_imputeMulti(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
