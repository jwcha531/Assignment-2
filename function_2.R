###
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result <- median(d_1[[var]]) - median(d_2[[var]])
  return(result)
}

###

randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]])
  return(d)
}

###
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic, n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    permutations <- randomize(d, var)
    permutation_statistics[i] <- statistic(permutations, var, grouping_var, group1, group2) 
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}


###
permutation_pvalue_right <- function(simulated, observed) {
  return(mean(simulated >= observed))
}

permutation_pvalue_left <- function(simulated, observed) {
  return(mean(simulated <= observed))
}


## Exercice 3
permutation_t_test <- function(x1, x2)  {
  diff_in_means <- mean(x1) - mean(x2)
  n1 <- length(x1)
  n2 <- length(x2)
  variance_term <- sqrt( var(x1)/n1 + var(x2)/n2 )
  return(diff_in_means/variance_term)
}

