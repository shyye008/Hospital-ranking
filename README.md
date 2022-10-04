# Hospital-ranking
The file "Simulated Data.csv" contains a simulated data set. 
The file "Function.R" contains the R function "sim_ranking" for the simulation-based power, FPR, PPV, and NPV calculation.
The file "Data analysis.R" contains example code for calculating the power, FPR, PPV and NPV for each hospital based on the simulated data set "Simulated Data.csv". 

# The function "sim_ranking" takes the following arguements:
  # K: number of simulations. Default is 1000.
  # fmla: a two-sided linear formula of the marginal model with the response (y) on the left of a ~ operator and the covariates, separated by + operators, on the right.
  # ID: a vector of hospital indexes.
  # X: a data frame containing the covariates named in formula. 
  # beta_hat: a vector of estimated fixed effects parameters in the logistic mixed effects model.
  # alpha: a scalar of random effects mean.
  # sigma_alpha: a positive scalar of random effects standard deviation.
# The output of "sim_ranking" will be a data frame with calculated power, FPR, PPV, and NPV for each unique hospital ID.

