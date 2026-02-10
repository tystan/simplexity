


enumerate_simplex(4, 5)



enumerate_simplex_clip <- function(p, n, clip = 0) {
  print(paste("max value in grid =", n - (p - 1) * clip))
  enumerate_simplex(p, n - p * clip) + clip
}
enumerate_simplex(4, 5 - 4) + 1
enumerate_simplex_clip(4, 5, 1)



enumerate_simplex(4, 9 - 2 * 4) + 2
enumerate_simplex_clip(4, 9, 2)

# min value = clip
# max value = n - (p - 1) * clip
enumerate_simplex_clip(3, 15, 4)
enumerate_simplex(3, 15 - 4 * 3) + 4
