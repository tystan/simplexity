




#' The inverse procedure to return orthogonal mean centred, unit variance blockwise scaled data back to the original scale
#'
#' @param dat_sc is the mean centred, unit variance \code{n} times \code{p} \code{sc}aled matrix.
#' @param m_grp the vector mean of length \code{p} of the original data. 
#' @param v_grp the \code{p} times \code{p} variance-covariance of the original data.
#' @param grp_cns (optional) column names the original data.
#'
#' @return the\code{n} times \code{p} matrix on the original scale
#' @export
#'
#' @examples
#' #' library(mvtnorm)
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
orthog_scale_grp_inv <- function(dat_sc, m_grp, v_grp, grp_cns = NULL) {
  
  n_c <- ncol(dat_sc)
  
  cns <- colnames(dat_sc)
  if (!is.null(grp_cns)) {
    cns <- grp_cns
  } else if (is.null(cns)) {
    cns <- paste0("v", 1:n_c)
  }
  
  dat_sc <- as.matrix(dat_sc)
  stopifnot(is.matrix(v_grp))
  m_grp <- as.vector(m_grp, mode = "double")
  
  # v_grp_sqrt <- expm::sqrtm(v_grp)
  cis <- covar_inverse_sqrt(v_grp)
  
  dat <- t(t(dat_sc %*% cis %*% v_grp) + m_grp)
  colnames(dat) <- cns
  
  return(dat)
  
}


 









