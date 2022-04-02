main <- function(start = 1, end = 60, true_rel = F) {
  library(tidyverse)
  library(semTools)
  library(reliacoef)
  library(psych)
  library(Lambda4)
  library(tictoc)
  library(misty)
  # specify simulation conditions
  dd <- c(0, 1) # difference in discrimination, 0: no difference, 1: difference
  n <- c(50, 100, 250, 500, 1000)
  k <- c(3, 6, 9)
  numcat <- c(2, 5)
  if (true_rel) {
    N_SET <- 100
    TOTREP <- 1000000
    name <- "truerel"
  } else {
    N_SET <- 10
    TOTREP <- 1000
    name <- "irtm"
  }
  rep_sets <- 1:N_SET
  rep_per_set <- 1:(TOTREP/N_SET)
  conditions <- tidyr::crossing(dd, n, k, numcat)
  colnames(conditions) <- c("dd", "n", "k", "numcat")
  condition_numbers <- start:end
  #========================================================================
  # Loop
  #========================================================================
  for (condition_number in condition_numbers) {
    condition <- conditions[condition_number, ]
    print(condition)
    for (rep_set in rep_sets) {
      tictoc::tic()
      print(paste("Starting condition number", condition_number, "rep", rep_set))
      print(condition)
      filename <- paste0(name, condition_number, "-", rep_set, ".csv")
      if (!file.exists(filename)) {
        for (rep in rep_per_set) {
          cat(name, ": ", condition_number, "rep set: ", rep_set, "rep: ", rep)
          data <- generate(conditions, condition_number, rep_set, rep, true_rel)
          if (rep == 1) {
            if (true_rel) {
              temp <- data
            } else {
              temp <- analyze(conditions, condition_number, rep_set, rep, data)
            }
          } else {
            if (true_rel) {
              temp <- rbind(temp, data)
            } else {
              temp <- rbind(temp,
                            analyze(conditions, condition_number, rep_set, rep, data))
            }
          }
        } # end of for (rep in rep_per_set)
        # 3 duplicates, leaving only one
        if (!true_rel) {
          out <- temp[1:100,]
        } else {
          out <- temp
        }
        # for (i in 1:300) {
        #   if (i %% 3 == 0) {
        #     out[round(i / 3), ] <- temp[i, ]
        #   }
        # }
        readr::write_csv(data.frame(out), file = filename)
        print(out)
        tictoc::toc()
      } # end of if (!file.exists(filename))
    } # end of  for (rep_set in rep_sets)
  } # end of for (condition_number in condition_numbers)
} # end of function