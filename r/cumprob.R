cumprob <- function(dscrm, loc, numcat, order, cat, latent) {
  if (cat == 0) {
    out <- 1 #All items have a value greater than or equal to 0
  } else if (cat == numcat) {
    out <- 0
  } else {
    if (numcat == 2) {
      exp_sum <- exp(dscrm[order] * (latent - loc[order]))
    } else {
      exp_sum <- exp(dscrm[order] * (latent - loc[order, cat]))
    }
    out <- exp_sum / (1 + exp_sum)
  }
  return(out)
}
