
#' Add transparency to supplied vector of colours
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Add transparency to supplied vector of colours
#' @param col vector of colours
#' @param alpha a value in (0, 1]. Default is 1 which does not change the transparency. 0 would be an invisible colour.
#' @export
#' @details
#' The returned colours are hex colour values even if colours were
#' entered as names. e.g., \code{add_alpha("black", 0.5)} returns \code{"#00000080"}
#'
#' @examples
#' add_alpha(c("black", red", yellow", blue"), 0.5)
#'
#'
add_alpha <- function(col, alpha = 1) {
  apply(
    sapply(col, col2rgb) / 255,
    2, # by column
    function(x) rgb(x[1], x[2], x[3], alpha = alpha)
  )
}
