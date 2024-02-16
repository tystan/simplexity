



cppFunction('IntegerMatrix enumerate_simplex_cpp(int p, int n, int count) {
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
  
}')

# p <- 5
# n <- 8
# (count_row <-  n_points(p, n))
# table(abs(enumerate_simplex_cpp(p, n, count_row) - xsimplex_old(p, n)) > 0)

n_points <- function(D, m) {
  as.integer(round(prod((D + m - 1):(m + 1)) / factorial(D - 1)))
}

enumerate_simplex <- function(p, n, verbose = TRUE) {
  
  p <- as.integer(p)
  n <- as.integer(n)
  
  if (p < 1L) {
    warning("p must be an integer of least 1")
    return(NULL)
  } else if (n < 0L) {
    warning("n must be a non-negative integer")
    return(NULL)
  } else if (n == 0L) {
    warning("As n == 0, returning the trivial result")
    return(matrix(0L, nrow = 1, ncol = p))
  }
  
  count <- n_points(p, n)
  
  if (verbose) {
    message("There will be ", count, " rows and ", p, " cols in the returned matrix")
    out_tmp <- matrix(0L, nrow = count, ncol = p)   
    out_tmp_sz <- format(object.size(out_tmp), "MB") 
    rm(list = "out_tmp")
    message("This matrix will be approximately ", out_tmp_sz, " in memory")
  }
  
  return(enumerate_simplex_cpp(p, n, count))
  
}
