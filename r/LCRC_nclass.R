#' Finding the optimal number of class to obtain LCRC
#'
#' Finding the optimal number of class to obtain LCRC. To estimate the LCRC, we
#' need to determine the number of latent classes. Existing criteria for
#' determining are AIC and BIC, but van der Ark et al. (2011) used AIC3.
#' The current version is only available with 6 or 10 items. Requires the poLCA
#' package.
#' @param data a dataframe or matrix
#' @return Optimal number of classes when each criterion (AIC, BIC, AIC3) is applied
#' @references Linzer, D. A., & Lewis, J. B. (2011). PoLCA: An R Package for
#' polytomous variable latent class analysis. Journal of Statistical Software,
#' 42(10). https://doi.org/10.18637/jss.v042.i10
#' @references van der Ark, L. A., van der Palm, D. W., & Sijtsma, K. (2011). A
#' latent class approach to estimating test-score reliability. Applied
#' Psychological Measurement, 35(5), 380-392.
#' https://doi.org/10.1177/0146621610392911
LCRC_nclass <- function(data){
  stopifnot(requireNamespace("poLCA"))
  data <- data.frame(data)
  k <- ncol(data)
  colnames(data) <- LETTERS[1:k]
  if (min(data) < 1) {
    data <- data + 1
  }
  if (k == 3) {
    f <- cbind(A, B, C) ~ 1
  } else if (k == 6) {
    f <- cbind(A, B, C, D, E, F) ~ 1 # Oosterwijk et al. (2017)
  } else if (k == 9) {
    f <- cbind(A, B, C, D, E, F, G, H, I) ~ 1 # Oosterwijk et al. (2017)
  } else {
    stop("sorry, the column length should be either three, six, or nine")
  }
  AIC3 <- AIC <- BIC <-  vector("double", k)
  for (i in 1:k) {
    poLCA_out <- poLCA::poLCA(f, data, nclass = i)
    AIC[i] <-  poLCA_out$aic
    BIC[i] <-  poLCA_out$bic
    AIC3[i] <-  -2 * poLCA_out$llik + 3 * poLCA_out$npar # used by van der Ark et al. (2011)
  }
  out <- list(AIC = which.min(AIC), BIC = which.min(BIC), AIC3 = which.min(AIC3))
  return(out)
}
