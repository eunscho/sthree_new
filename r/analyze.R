analyze <- function(conditions, condition_number, rep_set, rep, data) {
  library(mokken)
  library(poLCA)
  n <- as.integer(conditions[condition_number, 1])
  k <- as.integer(conditions[condition_number, 2])
  numcat <- as.integer(conditions[condition_number, 3])
  m <- var(data)
  r <- cov2cor(m)
  
  
  #===========================================================================
  # reliability
  #===========================================================================
  alpha <- reliacoef::alpha(m, print = F)
  stn_alpha <- reliacoef::alpha(r, print = F)
  # ordinal alpha
  safepolychoric <- purrr::safely(psych::polychoric)
  polycor <- safepolychoric(data)$result
  if (is.null(polycor)) {
    ord_alpha <- NA
  } else {
    ord_alpha <- reliacoef::alpha(polycor$rho, print = F)
  }
  lambda2 <-  Lambda4::lambda2(m)
  stn_lambda2 <- Lambda4::lambda2(r)
  mu3 <- reliacoef::mu3(m)
  stn_mu3 <- reliacoef::mu3(r)
  safejoreskog <- purrr::safely(reliacoef::joreskog)
  joreskog <- safejoreskog(m)$result
  safecfa <- purrr::safely(reliacoef::uni_cfa)
  cfar <- safecfa(r)$result
  if (is.null(polycor)) {
    polycfa <- NA
  } else {
    polycfa <- safecfa(polycor$rho)$result
  }
  
  # joreskog <- reliacoef::joreskog(m)
  # cfar <- uni_cfa(r)
  if (is.na(cfar)) {
    joreskog_r <- NA
  } else {
    joreskog_r <- sum(cfar$lambda)^2/(sum(cfar$lambda)^2 + sum(cfar$theta))
  }
  # polycfa <- uni_cfa(polycor)
  if (is.na(polycfa)) {
    ord_omega <- NA
  } else {
    ord_omega <- sum(polycfa$lambda)^2/(sum(polycfa$lambda)^2 + sum(polycfa$theta))
  }
  cat_omega <- misty::item.omega(data, type = "categ")$result$omega$omega
  safeomega <- purrr::safely(psych::omega)
  minres <- safeomega(r, nfactors = 1, fm = "minres")$result$omega.tot
  kaiser <- reliacoef::kaisercaffrey(m)
  # LCRC & MS
  safenclass <- purrr::safely(LCRC_nclass)
  nclass <- safenclass(data)$result
  if(is.null(safenclass)) {
    MS <- AIC <- AIC3 <- BIC <- NA
  } else {
    mokken_AIC <- mokken::check.reliability(data, LCRC = TRUE, nclass = nclass$AIC)
    mokken_BIC <- mokken::check.reliability(data, LCRC = TRUE, nclass = nclass$BIC)
    mokken_AIC3 <- mokken::check.reliability(data, LCRC = TRUE, nclass = nclass$AIC3)
    MS <- mokken_AIC$MS
    AIC <- mokken_AIC$LCRC
    AIC3 <- mokken_AIC3$LCRC
    BIC <- mokken_BIC$LCRC
  }
  
  
  out <- tibble::tibble(condition_number = condition_number,
                        n = n,
                        k = k,
                        numcat = numcat,
                        rep_set = rep_set,
                        rep = rep,
                        ord_alpha =ord_alpha,
                        ord_omega = ord_omega,
                        cat_omega = cat_omega,
                        MS = MS,
                        AIC = AIC,
                        AIC3 = AIC3,
                        BIC = BIC,
                        alpha = alpha,
                        stn_alpha = stn_alpha,
                        lambda2 = lambda2,
                        stn_lambda2 = stn_lambda2,
                        mu3 = mu3,
                        stn_mu3 = stn_mu3,
                        joreskog = joreskog,
                        joreskog_r = joreskog_r,
                        minres = minres,
                        kaiser = kaiser
                        )
  return(out)
}
