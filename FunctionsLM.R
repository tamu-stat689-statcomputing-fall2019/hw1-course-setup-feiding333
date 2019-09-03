# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, where epsilon is normal zero with variance sigma^2 independent across samples. Seed should be set at the beginning of the function
# X - design matrix
# beta - given parameter vector
# sigma - standard deviation of the noise
# seed  - starting seed value
generateY <- function(X, beta, sigma, seed = 5832652){
  #[ToDo] Set seed and generate Y following linear model
  nobs <- dim(X)[1] # get the number of observations
  noise <- rnorm(n = nobs, mean = 0, sd = sigma) # generate the noise term
  Y <- X %*% beta + noise # generate the observations
  # Return Y
  return(Y)
}

# Calculate beta_LS - least-squares solution, do not use lm function
# X - design matrix
# Y -response
calculateBeta <- function(X, Y){
  # Calculate beta_LS
  
  # check wether the matrix is singular or not
  XtX <- crossprod(X, X)
  XtY <- crossprod(X, Y) 
  flag_non_singular <- class( try(solve( XtX ), silent=T) )=="matrix" # check wether it is singular matrix
  
  if (flag_non_singular){
    # if it is not a singular matrix
    beta_LS <- solve( XtX, XtY) # calculate the beta
    
  }else{
    # if it is a singular matrix, use svd decomposition to calculate the generalized inverse matrix
    svd_decom <- svd( XtX ) # svd decomposition
    flag_eig_non_zero <- ( svd_decom$d > 1e-2 ) # only use the columns that the eigenvalues are none zero
    # calculate the inverse matrix using svd
    inv_matrix <- svd_decom$v[ ,flag_eig_non_zero] %*% diag(1/svd_decom$d[flag_eig_non_zero]) %*%t(svd_decom$u[ ,flag_eig_non_zero])
    # calculate the beta
    beta_LS <- inv_matrix %*% XtY # calculate the beta
    
  }
  
  # Return beta
  return(beta_LS)
}

# Calculate MSE
calculateMSE <- function(beta, beta_LS){
  
  # Return MSE - error ||beta - beta_LS||_2^2
  return(MSE)
}