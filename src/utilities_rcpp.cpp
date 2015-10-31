#include <Rcpp.h>
#include <iostream>
#include <cmath>
#include <string>

using namespace Rcpp;
using namespace std;

//sup of L1 distance between x and y
// [[Rcpp::export]]
double supDistCpp (NumericVector& x, NumericVector& y) {
  int nx = x.size();
  int ny = y.size();
  if (nx != ny) {
    cout << "ERROR: Length of x and y differ." << endl;
    return -1;
  }

  double sup = -1.0;
  for (int i = 0; i < nx; i++) {
    if (abs(x[i] - y[i]) > sup) sup = abs(x[i] - y[i]);
  }
  return sup;
}

// @description Compare an array with missing values \code{marg} and an array
// with complete values \code{complete}. Return matching indices. Can compare
// either marginal-to-complete or complete-to-marginal.
// @param marg A two dimensional array with missing values
// @param complete A two dimensional array without missing values
// @param marg_to_comp Logical. Do you wish to compare marginal values to
// complete values/matches? Defaults to \code{FALSE} ie- complete values compared
// to marginal matches.
// @return A \code{list} of matches.
// [[Rcpp::export]]
List marg_comp_compare (IntegerMatrix& marg, IntegerMatrix& complete, bool marg_to_complete= false) {
  int ncol_m = marg.ncol(), ncol_c = complete.ncol();
  if (ncol_m != ncol_c) {
    cout << "ERROR: ncol of marg and complete do not match." << endl;
    return 0;
  }

  int nrow_m = marg.nrow(), nrow_c = complete.nrow();
  if (marg_to_complete == false) { // compare rows of complete to rows of marg
    vector<vector<int> > out(nrow_c); // output vector of vectors, will be coerced to list

    for (int ci = 0; ci < nrow_c; ci++) {
      IntegerVector comp = complete.row(ci);
      for (int mj = 0; mj < nrow_m; mj++) {
        IntegerVector comp2 = marg.row(mj);
        LogicalVector eq;
        for (int k = 0; k < ncol_c; k++) {
          // check for missing values
          if (comp[k] != NA_LOGICAL && comp2[k] != NA_LOGICAL) {
            eq.push_back(comp[k] == comp2[k]);
          }
        }
        if(is_true(all(eq))) out[ci].push_back(mj + 1); // use R's iterator syntax (ie [1,n] vs [0, n-1]
      }
    }
    return wrap(out);
  }
  else { // compare rows of marg to rows of complete
    vector<vector<int> > out(nrow_m); // output vector of vectors, will be coerced to list

    for (int mi = 0; mi < nrow_m; mi++) {
      IntegerVector comp = marg.row(mi);
      for (int cj = 0; cj < nrow_c; cj++) {
        IntegerVector comp2 = complete.row(cj);
        LogicalVector eq;
        for (int k = 0; k < ncol_c; k++) {
          // check for missing values
          if (comp[k] != NA_LOGICAL && comp2[k] != NA_LOGICAL) {
            eq.push_back(comp[k] == comp2[k]);
          }
        }
        if(is_true(all(eq))) out[mi].push_back(cj + 1); // use R's iterator syntax (ie [1,n] vs [0, n-1]
      }
    }
    return wrap(out);
  }
}

// @title count compare (internal only)
// @description Given a dataset and a data.frame of comparison patterns,
// count the number of occurances of each pattern. Internally called by \code{count_levels}.
// @param dat A \code{data.frame}. All variables must be factors
// @param x A \code{data.frame} consisting of all possible patterns for matching
// with \code{dat}.
// @param hasNA A string. Denotes if \code{dat} has complete data or not.
  // \code{"no"} - there are no missing values, count observed patterns
  // \code{"count.obs"} - there are missing values, count the marginally observed patterns
  // \code{"count.miss"} - there are missing values, count the full observed-and-missing patterns

// [[Rcpp::export]]
IntegerVector count_compare (IntegerMatrix& x, IntegerMatrix& dat, string& hasNA) {
  // remember will work with x.ncol() - 1 vs dat.ncol()

  IntegerVector out(x.nrow());
  int nr_x = x.nrow(), nr_dat = dat.nrow(), nc_x = (x.ncol() - 1);

  if (hasNA == "no") {
    for (int i = 0; i < nr_x; i++) {
        int cnt = 0;
      for (int j = 0; j < nr_dat; j++) {
        if (is_true(all(x.row(i) == dat.row(j)))) ++cnt;
      }
      out[i] = cnt;
    }
  } else if (hasNA == "count.obs") {
    for (int i = 0; i < nr_x; i++) {
        int cnt = 0;
      for (int j = 0; j < nr_dat; j++) {
        // find observed values. Observed values in dat(j,) must match same values in x(i,)
        IntegerVector x_exist, dat_exist;
        for (int k = 0; k < nc_x; k++) {
          if (dat.row(j)[k] != NA_INTEGER) {
            dat_exist.push_back(dat.row(j)[k]);
            x_exist.push_back(x.row(i)[k]);
          }
        }
        if (is_true(all(x_exist == dat_exist))) ++cnt;
      }
      out[i] = cnt;
    }
  } else if (hasNA == "count.miss") {
    for (int i = 0; i < nr_x; i++) {
        int cnt = 0;
      for (int j = 0; j < nr_dat; j++) {
        // find NA values
        // split both rows into two vectors: one of indices of missing values and one of observed values
        // both must match for equivalence
        IntegerVector x_na, x_exist, dat_na, dat_exist;
        for (int k = 0; k < nc_x; k++) {
          if (x.row(i)[k] == NA_INTEGER) {
            x_na.push_back(k);
            x_exist.push_back(x.row(i)[k]);
          }
          if (dat.row(j)[k] == NA_INTEGER) {
            dat_na.push_back(k);
            dat_exist.push_back(dat.row(j)[k]);
          }
        }
        // if (same number NA, same positions NA, same values existing) ==> match
        if (x_na.size() == dat_na.size() && is_true(all(x_na == dat_na)) &&
            is_true(all(x_exist == dat_exist)))
          ++cnt;
      }
      out[i] = cnt;
    }
  }
  else {
    cout << "ERROR: hasNA improperly specified" << endl;
    return -1;
  }
  // return counts
  return out;
}
