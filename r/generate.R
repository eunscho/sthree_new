generate <- function(conditions, condition_number, rep_set, rep, true_rel = F) {
  #========================================================================
  # seed and variable names
  #========================================================================
  set.seed(10000 * condition_number + 1000 * rep_set + rep)
  dd <- as.integer(conditions[condition_number, 1])
  n <- as.integer(conditions[condition_number, 2])
  k <- as.integer(conditions[condition_number, 3])
  numcat <- as.integer(conditions[condition_number, 4])
  #========================================================================
  # determine location and discrimination parameters
  #========================================================================
  # loc_parm <- matrix(vector("double", (total_cat - 1) * n_items),
  #                    nrow = n_items)
  pattern <- rep(c(-1, 0, 1), k/3)
  if (numcat == 2) {
    loc <- pattern
  } else {
    loc <- matrix(vector("double", (numcat - 1) * k), nrow = k)
    for (i in 1:k) {
      for (j in 1:(numcat - 1)) {
        loc[i, j] <- pattern[i] -1.25 + .5 * j
      }
    }
  }
  if (dd == 0) {
    dscrm <- rep(2, k)
  } else {
    dscrm <- rep(c(1.5, 2, 2.5), k/3)
  }
  latent <- rnorm(n)
  ###########################################################################
  # Item score
  ##########################################################################
  item_score <- matrix(vector("double", n * k), nrow = n)
  prob <- vector("double", numcat)  # Probability of item value being zero
  for (i in 1:n) {
    for (j in 1:k) {
      for (l in 1:numcat) {
        prob[l] <- cumprob(dscrm, loc, numcat, order = j, cat = l - 1, latent[i]) -
                   cumprob(dscrm, loc, numcat, order = j, cat = l, latent[i])
      }
      if (numcat == 2) {
        among <- c(0, 1)
      } else {
        among <- c(0, 1, 2, 3, 4)
      }
      item_score[i, j] <- sample(among, size = 1, replace = TRUE, prob = prob)
    }
  }
  ###########################################################################
  # If the goal is to find true reliability
  ##########################################################################
  if (true_rel) {
    true_score <- vector("double", n)
    for (i in 1:n) {
      sum <- 0
      for (j in 1:k) {
        for (l in 1:numcat) {
          sum <- sum + cumprob(dscrm, loc, numcat, order = j, cat = l - 1, latent = latent[i])
        }
      }
      true_score[i] <- sum
    }
    out <- sum(var(true_score)) / sum(var(item_score))
  } else {
    out <- item_score
  }
  return(out)
}