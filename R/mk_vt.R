#' Create the transpose of a V matrix (i.e., V^T)
#' 
#' @author Ty Stanford <tystan@gmail.com>
#' @description Create the transpose of a V matrix (i.e., V^T)
#' @param n_comp Integer value \code{D} that is the number of compositional components (columns)
#' @param normalise do you want to normalise the columns of V^T (similarly, the rows of V) to unit vectors? 
#' @export
#' 
#' @details 
#' Note that:
#' 
#' \code{ilr(x) = ln(x) \%*\% t(V)}
#' 
#' \code{-> x = clo(exp(ilr(x) \%*\% V))}
#' 
#' as
#' 
#' \code{t(V) \%*\% V =} \eqn{I_{D-1}}, and
#' 
#' \code{V \%*\% t(V) =} \eqn{I_{D} - \frac{1}{D} \times 1_{DxD}}
#' 
#' @examples 
#' mk_vt(4)
#' (VT <- mk_vt(3))
#' VT %*% t(VT) 
#' t(VT) %*% VT  


mk_vt <- function(n_comp, normalise = TRUE) {
  
  base_zeros <- rep(0, n_comp)
  
  ilr_base <- matrix(0, nrow = n_comp, ncol = n_comp - 1)
  for (j in 1:(n_comp - 1)) {
    this_col <- base_zeros
    this_col[(j + 1):n_comp] <- -1
    this_col[j] <- n_comp - j # -sum(this_col[(j + 1):n_comp])
    ilr_base[, j] <- this_col
  }
  # alternative ilr bases can be made via compositions::ilrBase(D = n_comp) 
  
  if (normalise) {
    vec_len <- sqrt(colSums(ilr_base ^ 2))
    for (j in 1:(n_comp - 1)) {
      ilr_base[, j] <- ilr_base[, j] / vec_len[j]
    }
  }
  
  return(ilr_base)
  
}

