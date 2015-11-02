#include <Rcpp.h>
using namespace Rcpp;

// find which elements are NA in a vector - integer
IntegerVector which_na_i(IntegerVector vec) {
  int n = vec.size();
  IntegerVector out;
  for (int k = 0; k < n; k++) {
    if (vec[k] == NA_INTEGER) out.push_back(k);
  }
  return out;
}

// find which elements are NA in a vector - character
IntegerVector which_na_s(CharacterVector vec) {
  int n = vec.size();
  IntegerVector out;
  for (int k = 0; k < n; k++) {
    if (vec[k] == NA_STRING) out.push_back(k);
  }
  return out;
}

// find which elements are NA in a vector - numeric
IntegerVector which_na_n(NumericVector vec) {
  int n = vec.size();
  IntegerVector out;
  for (int k = 0; k < n; k++) {
    if (vec[k] == NA_REAL) out.push_back(k);
  }
  return out;
}

// find which elements are NA in a vector - logical
IntegerVector which_na_l(LogicalVector vec) {
  int n = vec.size();
  IntegerVector out;
  for (int k = 0; k < n; k++) {
    if (vec[k] == NA_LOGICAL) out.push_back(k);
  }
  return out;
}

// [[Rcpp::export]]
SEXP which_na(SEXP x) {
  switch(TYPEOF(x)) {
    case INTSXP: return which_na_i(x);
    case REALSXP: return which_na_n(x);
    case STRSXP: return which_na_s(x);
    case LGLSXP: return which_na_l(x);
  }
  return R_NilValue;
}
