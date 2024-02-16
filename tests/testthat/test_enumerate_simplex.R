context("enumerate_simplex() checks")

p <- 5
n <- 8
# (count_row <-  n_points(p, n))
es_result <- enumerate_simplex(p, n) / n
ms_result <- mk_simplex_grid(p, 1 / n, nc = 1)

ii1 <- do.call(order, args = as.data.frame(es_result))
es_result <- es_result[ii1, ]

ii2 <- do.call(order, args = as.data.frame(ms_result))
ms_result <- ms_result[ii2, ]


any(abs(es_result - ms_result) > 1e-6)


test_that("mk_simplex_grid() and enumerate_simplex() agree to epsilon", {
  
  expect_equal(nrow(es_result), nrow(ms_result))
  expect_equal(ncol(es_result), ncol(ms_result))
  expect_equal(any(abs(es_result - ms_result) > 1e-06), FALSE)
  expect_equal(any(abs(es_result - ms_result) > 1e-12), FALSE)
  expect_equal(any(abs(es_result - ms_result) > 1e-16), FALSE)
  
})
