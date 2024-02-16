#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix enumerate_simplex_cpp(int p, int n, int count) {
  // initialise output
  IntegerMatrix simplex_mat(count, p);
  
  // initialise vector that updates the output rows iteratively
  IntegerVector x(p);
  // populate the first row with n in the first position and 0s elsewhere 
  x[0] = n;
  for (int j = 1; j < p; j++) {
    x[j] = 0;
  }
  
  // positional indexes inside x
  int p1 =  p - 1;
  int p2 = p1 - 1;
  int target = 0;
  
  // start populating the output matrix
  for (int i = 0; i < count; i++) {
    simplex_mat(i, _) = x;
    x[target] = x[target] - 1;
    if (target < p2) {
      target = target + 1;
      x[target] = x[p1] + 1;
      x[p1] = 0;
    } else {
      x[p1] = x[p1] + 1;
      while (x[target] == 0) {
        target = target - 1;
        if (target < 0) {
          i = i + 1;
          simplex_mat(i, _) = x;
          return simplex_mat;
        }
      }
    }
  }
  
  return simplex_mat; // will have returned prior to this
  
}

