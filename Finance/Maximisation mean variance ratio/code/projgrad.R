norm <- function(x){
  return(sum(x^2))
}

source("C:/Users/DELL/Documents/R STUDIO/shiny app/March_Fin/code mean + cov.R")


S = as.matrix(Sigma)
r = as.matrix(performance$mean_return)



projected_gradient_algorithm <- function(objective_func, gradient_func, projection_func, initial_point, rho, epsilon, max_iterations) {
  k <- 0
  x <- initial_point
  g <- gradient_func(x)
  
  while (norm(g) > epsilon && k < max_iterations) {
    x_projected <- projection_func(x - rho * g)
    x = x_projected
    g <- gradient_func(x)
    k <- k + 1
  }
  
  return(x)
}

# Fonction objectif à minimiser
objective_func <- function(x) {
  return( -t(r)%*%x/ (t(x)%*%S)%*%x  )
}

# Gradient de la fonction objectif
gradient_func <- function(x) {
  return(  -( as.numeric( (t(x)%*%S)%*%x )*r - as.numeric(2*(t(r)%*%x)) *(S%*%x) )/as.numeric(((t(x)%*%S)%*%x )^2 )     )
}

# Fonction de projection sur les variables positives et qui satisfait la contrainte d'égalité
projection_func <- function(x) {
  for(i in 1:length(x)){
    x[i] = abs(x[i])  }
  x <- x / sum(x)
  return(x)
}


# Paramètres de l'algorithme
rho <- 0.001  # Pas d'apprentissage
epsilon <- 1e-7  # Critère d'arrêt
max_iterations <- 100000  # Nombre maximal d'itérations




