

#' mean centred, unit variance scale a block of variables simultaneously
#'
#' @param dat a \code{n} times \code{p} data.frame or matrix
#' @param grp_cns optional column names for \code{dat} to be returned in \code{osg_dat} (see below)
#'
#' @returns 
#' a list with elements \code{list(osg_dat, m_grp, v_grp, cis)} where:
#' 
#' \code{osg_dat} is the mean centred, unit variance transformation of \code{dat} as a matrix.
#' \code{m_grp} the vector mean of length \code{p} of \code{dat}. 
#' \code{v_grp} the \code{p} times \code{p} variance-covariance \code{dat} as a matrix.
#' \code{osg_dat} the matrix inverse of \code{v_grp} with the matrix square root subsequently applied (which is used in the group scaling).
#' @export
#'
#' @examples
#' library(mvtnorm)
#' # mean
#' m_ <- c(v1 = -5, v2 = 10)
#' 
#' # var-covar ("v_")
#' r_ <- -0.5
#' (s_ <- matrix(sqrt(c(v1 = 10, v2 = 5)), ncol = 1))
#' (c_ <- r_ * prod(s_))
#' (v_ <- s_ %*% t(s_))
#' v_[upper.tri(v_)] <- v_[lower.tri(v_)] <- c_
#' v_ # peek
#' 
#' # sample data
#' set.seed(1234)
#' samp_dat <- mvtnorm::rmvnorm(30, mean = m_, sigma = v_)
#' # returns list(osg_dat, m_grp, v_grp, cis)
#' (osg_lst <- orthog_scale_grp(samp_dat))
#' #' # inverse procedure
#' orthog_scale_grp_inv(osg_lst$osg_dat, osg_lst$m_grp, osg_lst$v_grp)
#' table( # should be all true, i.e., equal to tolerance
#'   abs(
#'     samp_dat - orthog_scale_grp_inv(osg_lst$osg_dat, osg_lst$m_grp, osg_lst$v_grp)
#'   ) < 1e-12,
#'   useNA = "ifany"
#' )

orthog_scale_grp <- function(dat, grp_cns = NULL) {
  
  n_c <- ncol(dat)
  cns <- colnames(dat)
  if (!is.null(grp_cns)) {
    cns <- grp_cns
  } else if (!is.null(cns)) {
    cns <- paste0("v", 1:n_c)
  }
  
  (m_grp <- colMeans(dat))
  (v_grp <- var(dat))
  cis <- covar_inverse_sqrt(v_grp)
  
  names(m_grp) <- colnames(v_grp) <- rownames(v_grp) <- 
    colnames(cis) <- rownames(cis) <- cns
  
  osg_dat <- t(t(as.matrix(dat)) - m_grp) %*% cis
  
  return(list(
    osg_dat = as.data.frame(osg_dat),
    m_grp = m_grp,
    v_grp = v_grp,
    cis = cis
  ))
  
}
