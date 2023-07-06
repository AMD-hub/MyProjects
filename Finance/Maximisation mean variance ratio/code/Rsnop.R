library(Rsolnp)

S = as.matrix(Sigma)
r = as.matrix(performance$mean_return)

assets = colnames(S)

optim_function <- function(x) {
  return( -t(r)%*%x/ (t(x)%*%S)%*%x  )
}



constraint_function = function(weights){
  sum(weights)
}


# Optimization
allocation <- 1
starting_weights <- rep(1/length(r), length(r)) # start with uniforme
output <- solnp( starting_weights, # Initialize weights
                 optim_function, # The function to minimize
                 eqfun = constraint_function, # The constraint function ( sum(weights) )
                 eqB = 1, # Constraint function must equal 1
                 LB = rep(0, length(r)), # Lower bound of constraint, weight >= 0
                 UB = rep(1, length(r)) ) # Upper bound of constraint, weight <= 1




