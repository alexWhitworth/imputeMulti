#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using namespace std;

//' @title Calculate the sup of L1 distance between x and y
//' @description sup of L1 distance between x and y
//' @param x A numeric \code{vector}
//' @param y A numeric \code{vector}
//' @return a numeric scalar.
// [[Rcpp::export]]
double supDistC (const NumericVector& x, const NumericVector& y) {
  int nx = x.size();
  
  double sup = -1.0;
  for (int i = 0; i < nx; i++) {
    if (abs(x[i] - y[i]) > sup) sup = abs(x[i] - y[i]);
  }
  return sup;
}

//' @title Compare two integer matrices, allowing missing values
//' @description Compare two two-dimensional arrays (\code{mat_x}, \code{mat_y}), where \code{mat_x}
//' permits missing values. Return a \code{list} of length \code{nrow(mat_x)} such that each list
//' element contains a vector of row indices from \code{mat_y} with row-equivalence of the non
//' missing values.
//' @param mat_x A two dimensional array which may contain missing values
//' @param mat_y A two dimensional array without missing values
//' @return A \code{list} of matches.
// [[Rcpp::export]]
List xy_compare (IntegerMatrix& mat_x, IntegerMatrix& mat_y) {

  // initialize variables
  int ncol_y = mat_y.ncol(), nrow_y = mat_y.nrow(), nrow_x = mat_x.nrow(); 
  vector<vector<int> > out(nrow_y); // output vector of vectors, will be coerced to list
  
  // iterate through -- very inefficient implementation
  for (int ci = 0; ci < nrow_y; ci++) {
    IntegerVector comp = mat_y.row(ci);
    for (int mj = 0; mj < nrow_x; mj++) {
      IntegerVector comp2 = mat_x.row(mj);
      LogicalVector eq;
      for (int k = 0; k < ncol_y; k++) {
        // check for missing values
        if (comp[k] != NA_INTEGER && comp2[k] != NA_INTEGER) {
          eq.push_back(comp[k] == comp2[k]);
        }
      }
      if(is_true(all(eq))) out[ci].push_back(mj + 1); // use R's iterator syntax (ie [1,n] vs [0, n-1]
    }
  }
  
  return wrap(out);
}

