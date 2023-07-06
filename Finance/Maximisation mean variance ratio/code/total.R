source("C:/Users/DELL/Documents/R STUDIO/shiny app/March_Fin/projgrad.R")

source("C:/Users/DELL/Documents/R STUDIO/shiny app/March_Fin/penalis.R")

source("C:/Users/DELL/Documents/R STUDIO/shiny app/March_Fin/Rsnop.R")



#########################################
### AFFICHAGE DES STATS #################
#########################################

performance
Sigma

#########################################
### METHODE GRADIENT PROJETÉ ############
#########################################
initial_point =matrix(c(0,0.38,0,0,0.23,0.23,0,0,0.16)) 
# Exécution de l'algorithme
result <- projected_gradient_algorithm(objective_func, gradient_func, projection_func, initial_point, rho, epsilon, max_iterations)

# Affichage du résultat : 
print("Méthode de Gradient projeté : ")
cat("Point optimal (%) : ", round(result*100,1), "\n")
cat("Rendement optimal : ", t(r)%*%result, "\n")

cat("Variance pondérée : \n" )
Varpond = Sigma * (result %*% t(result))
print( Varpond)

a =tibble(
  `rendement optimale ` =  t(r)%*%result,
  `Total variance` = sum(Varpond),
  `Total std` = sqrt(sum(Varpond)),
  `Annuelle std ` = sqrt(sum(Varpond))*sqrt(12),
  `Ratio ` = as.numeric(t(r)%*%result) /  (sqrt(sum(Varpond))*sqrt(12) ) 
)


#########################################
### METHODE PÉNALISATION ############
#########################################

initial_point =matrix(c(0,0.38,0,0,0.23,0.23,0,0,0.16)) 
# Exécution de l'algorithme
result <- penalis(obj_func,alpha, initial_point,  epsilon, max_iterations )


# Affichage du résultat : 
print("Méthode de pénalisation : ")
cat("Point optimal : ", round(result*100,1), "\n")
cat("Rendement optimal : ", t(r)%*%result, "\n")

cat("Variance pondérée : \n" )
Varpond = Sigma * (result %*% t(result))
print( round(10^(6)*Varpond,3))


a =tibble(
  `rendement optimale ` =  t(r)%*%result,
  `Total variance` = sum(Varpond),
  `Total std` = sqrt(sum(Varpond)),
  `Annuelle std ` = sqrt(sum(Varpond))*sqrt(12),
  `Ratio ` = as.numeric(t(r)%*%result) /  (sqrt(sum(Varpond))*sqrt(12) ) 
)



#########################################
### METHODE bibliotheque de R ###########
#########################################

# Exécution de l'algorithme
result = round(output$pars, 2)

# Affichage du résultat : 
print("Méthode de pénalisation : ")
cat("Point optimal : ", round(result*100,1), "\n")
cat("Rendement optimal : ", t(r)%*%result, "\n")

cat("Variance pondérée : \n" )
Varpond = Sigma * (result %*% t(result))
print( round(10^(6)*Varpond,3))


a =tibble(
  `rendement optimale ` =  t(r)%*%result,
  `Total variance` = sum(Varpond),
  `Total std` = sqrt(sum(Varpond)),
  `Annuelle std ` = sqrt(sum(Varpond))*sqrt(12),
  `Ratio ` = as.numeric(t(r)%*%result) /  (sqrt(sum(Varpond))*sqrt(12) ) 
)


