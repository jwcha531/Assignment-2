# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
difference_in_medians <- function(data, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(data, get(grouping_var) == group1)
  d_2 <- dplyr::filter(data, get(grouping_var) == group2)
  return(median(d_1[[var]])-median(d_2[[var]]))
  # [[var]] renvoie le contenu de la colonne tandis que [var] renvoie une dataframe
}

# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
#      provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  d[[var]] <- randomize_rts(d[[var]])
  return(d) 
}
randomize_rts <- function(d){
  
  n <- length(d)
  result <- sample(d, n)
  return(result)
}

# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
#      provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
#            a data frame, the name of a variable on which to calculate the
#            test statistic, the name of a grouping variable, the value of
#            the grouping variable corresponding to the first group, and
#            the value of the grouping variable corresponding to the second
#            group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permutation_twogroups <- function(d, var, grouping_var, 
                                  group1, group2, 
                                  statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    data_randomized <- randomize(d, var)
    permutation_statistics[i] <- statistic(data_randomized,var,grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}




permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}
permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}
permutation_pvalue_twosided <- function(p) {
  return(2*min(permutation_pvalue_left(p), permutation_pvalue_right(p)))
}



## Exercice 3
permutation_t_test <- function(d, value, grouping_var, group1, group2, N_SAMPLES=9999){
  statistics <- rep(NA, N_SAMPLES)
  for (i in 1:N_SAMPLES){
    d[[grouping_var]] <- sample(d[[grouping_var]])
    # run t-test for permutation groups
    statistics[i] <- t.test(dplyr::filter(d, get(grouping_var) == group1)$value, dplyr::filter(d, get(grouping_var) == group2)$value)$statistic
  } 
  return (statistics)
}

