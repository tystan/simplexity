
row_wise_closure <- function(y, clo_val = 1) {
  clo_val * t(apply(y, 1, function(x) x / sum(x)))
}