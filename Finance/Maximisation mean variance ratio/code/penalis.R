norm <- function(x){
  return(sum(x^2))
}

source("C:/Users/DELL/Documents/R STUDIO/shiny app/March_Fin/code mean + cov.R")


S = as.matrix(Sigma)
r = as.matrix(performance$mean_return)



alpha = function(x){
  alpha1 = abs(sum(x)-1)
  
  for(i in 1:length(x)){
    x[i] = min(x[i],0)  }
  alpha2 = norm(x)^2 
  
  return(alpha1+alpha2)
}



obj_func = function(x){  return( -t(r)%*%x/ (t(x)%*%S)%*%x  ) }

taux = 0.95

penalis <- function(objective_func,alpha, initial_point,  epsilon, max_iterations ) { 
  k <- 1
  x <- initial_point
  while ( k < max_iterations) {
    f <- function(x){ objective_func(x)+alpha(x)/epsilon }
    x = optim(par = x, fn = f, method = "BFGS")$par
    epsilon = epsilon*taux
    k <- k + 1
  }
  
  return(x)
}


# Paramètres de l'algorithme
rho <- 0.001  # Pas d'apprentissage
epsilon <- 1e-7  # Critère d'arrêt
max_iterations <- 5000  # Nombre maximal d'itérations







