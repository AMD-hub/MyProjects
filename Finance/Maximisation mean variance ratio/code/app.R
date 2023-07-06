library(shiny)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(Rsolnp)

# Norme 
norm <- function(x){
  return(sum(x^2))
}


ui <- fluidPage(
  titlePanel("Asset Returns"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Import CSV file"),
      selectInput("assets", "Choose assets", choices = NULL, multiple = TRUE),
      actionButton("calc", "Calculate"), 
      actionButton("sol", "Solve 1"),
      actionButton("sol1", "Solve 2"),
      actionButton("sol2", "Solve 3")
    ),
    mainPanel(
      tableOutput("table"),
      tableOutput("matrix"),
      tableOutput("matrixx"),
      tableOutput("matrixxx"),
      tableOutput("matrixxxx")
  )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read_delim(input$file$datapath, 
               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
               trim_ws = TRUE)
  })
  
  observe({
    updateSelectInput(session, "assets", choices = colnames(data()))
  })
  
  observeEvent(input$calc, {
    returns <- reactive({
      data()  %>% select(input$assets) %>% 
        mutate(across(everything(),function(col){(col-lag(col))/lag(col)} ) ) %>% 
        na.omit()
    })
  
    
    output$table <- renderTable({
      returns() %>% pivot_longer(everything(),values_to = "Return",names_to = "Asset" ) %>% 
        group_by(Asset) %>% 
        summarise(mean_return = 12*mean(Return),perf_return = prod(Return+1)-1)
      
    }, digits = 5,caption = "Performance moyenne puis total de chaque action")
    
    output$matrix <- renderTable({
      returns() %>%
        cov() %>% `rownames<-`(colnames(returns())) %>% 
        round(6)
    }, digits = 5,caption = "Matrice de variance covariance pure pour chaque couple d'action")
  })


  
  observeEvent(input$sol, {
    returns  <- reactive({
      data()  %>% select(input$assets) %>% 
        mutate(across(everything(),function(col){(col-lag(col))/lag(col)} ) ) %>% 
        na.omit()
    })

    x=returns() %>% pivot_longer(everything(),values_to = "Return",names_to = "Asset" ) %>% 
      group_by(Asset) %>% 
      summarise(mean_return = 12*mean(Return))
    
    r=as.matrix(x$mean_return)
    
    x = returns() %>%
      cov() %>% `rownames<-`(colnames(returns()))
    
    S=as.matrix(x)
    
    #########################################
    ### METHODE GRADIENT PROJETÉ ############
    #########################################
    
    
    # Paramètres de l'algorithme
    rho <- 0.001  # Pas d'apprentissage
    epsilon <- 1e-7  # Critère d'arrêt
    max_iterations <- 100000  # Nombre maximal d'itérations
    
    
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
    
    norm <- function(x){
      return(sum(x^2))
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
    n = length(input$assets)
    initial_point =matrix(rep(1/n,n)) 
    
    # Exécution de l'algorithme
    
    result <- projected_gradient_algorithm(objective_func, gradient_func, projection_func, initial_point, rho, epsilon, max_iterations)
    rownames(result)<-colnames(S)

    Varpond = S * (result %*%t(result)) 
    
    a =tibble(
      `rendement optimale ` =  t(r)%*%result,
      `Total variance` = sum(Varpond),
      `Total std` = sqrt(sum(Varpond)),
      `Annuelle std ` = sqrt(sum(Varpond))*sqrt(12),
      `Ratio ` = as.numeric(t(r)%*%result) /  (sqrt(sum(Varpond))*sqrt(12) ) 
    )
    
    out2 = reactive({a})
    
    out1 = reactive({Varpond})  
    
    out = reactive({t(result)})
    
    output$matrixx <- renderTable({
      out()
    }, digits = 5,caption = "Les poids optimaux par : méthode de gradient projeté")

    
    output$matrixxx <- renderTable({
      out1()
    }, digits = 5,caption = "Matrice de variance covariance ponderée pour chaque couple d'action")

    output$matrixxx <- renderTable({
      out2()
    }, digits = 5,caption = "Statsitique génerales")
    
    
    
  })
  
  observeEvent(input$sol1, {
    returns  <- reactive({
      data()  %>% select(input$assets) %>% 
        mutate(across(everything(),function(col){(col-lag(col))/lag(col)} ) ) %>% 
        na.omit()
    })
    
    x=returns() %>% pivot_longer(everything(),values_to = "Return",names_to = "Asset" ) %>% 
      group_by(Asset) %>% 
      summarise(mean_return = 12*mean(Return))
    
    r=as.matrix(x$mean_return)
    
    x = returns() %>%
      cov() %>% `rownames<-`(colnames(returns()))
    
    S=as.matrix(x)
    
    #########################################
    ### METHODE  DE PÉNALISATION ############
    #########################################
    norm <- function(x){
      return(sum(x^2))
    }
    
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

 
    n = length(input$assets)
    initial_point =matrix(rep(1/n,n)) 
    
    # Exécution de l'algorithme
    
    result <- penalis(obj_func,alpha, initial_point,  epsilon, max_iterations )
    rownames(result)<-colnames(S)
    
    Varpond = S * (result %*%t(result)) 
    
    a =tibble(
      `rendement optimale ` =  t(r)%*%result,
      `Total variance` = sum(Varpond),
      `Total std` = sqrt(sum(Varpond)),
      `Annuelle std ` = sqrt(sum(Varpond))*sqrt(12),
      `Ratio ` = as.numeric(t(r)%*%result) /  (sqrt(sum(Varpond))*sqrt(12) ) 
    )
    
    out2 = reactive({a})
    
    out1 = reactive({Varpond})  
    
    out = reactive({t(result)})
    
    output$matrixx <- renderTable({
      out()
    }, digits = 5,caption = "Les poids optimaux par : méthode de pénalisation")
    
    
    output$matrixxx <- renderTable({
      out1()
    }, digits = 5,caption = "Matrice de variance covariance ponderée pour chaque couple d'action")
    
    output$matrixxx <- renderTable({
      out2()
    }, digits = 5,caption = "Statsitique génerales")
    
    
    
  })

  observeEvent(input$sol2, {
    returns  <- reactive({
      data()  %>% select(input$assets) %>% 
        mutate(across(everything(),function(col){(col-lag(col))/lag(col)} ) ) %>% 
        na.omit()
    })
    
    x=returns() %>% pivot_longer(everything(),values_to = "Return",names_to = "Asset" ) %>% 
      group_by(Asset) %>% 
      summarise(mean_return = 12*mean(Return))
    
    r=as.matrix(x$mean_return)
    
    x = returns() %>%
      cov() %>% `rownames<-`(colnames(returns()))
    
    S=as.matrix(x)
    
    #########################################
    ### METHODE  DE de bibliotheque ############
    #########################################
    
    optim_function <- function(x) {
      return( -t(r)%*%x/ (t(x)%*%S)%*%x  )
    }

    constraint_function = function(weights){
      sum(weights)
    }
    
    # Optimization
    allocation <- 1
    starting_weights <- rep(1/length(r), length(r)) # start with uniforme
    outputt <- solnp( starting_weights, # Initialize weights
                     optim_function, # The function to minimize
                     eqfun = constraint_function, # The constraint function ( sum(weights) )
                     eqB = 1, # Constraint function must equal 1
                     LB = rep(0, length(r)), # Lower bound of constraint, weight >= 0
                     UB = rep(1, length(r)) ) # Upper bound of constraint, weight <= 1
    
    
    # Exécution de l'algorithme
    result = matrix(round(outputt$pars, 2) )    

    rownames(result)<-colnames(S)
    
    Varpond = S * (result %*%t(result)) 
    
    a =tibble(
      `rendement optimale ` =  t(r)%*%result,
      `Total variance` = sum(Varpond),
      `Total std` = sqrt(sum(Varpond)),
      `Annuelle std ` = sqrt(sum(Varpond))*sqrt(12),
      `Ratio ` = as.numeric(t(r)%*%result) /  (sqrt(sum(Varpond))*sqrt(12) ) 
    )
    
    out2 = reactive({a})
    
    out1 = reactive({Varpond})  
    
    out = reactive({t(result)})
    
    output$matrixx <- renderTable({
      out()
    }, digits = 5,caption = "Les poids optimaux obtenues par bibliothèque")
    
    
    output$matrixxx <- renderTable({
      out1()
    }, digits = 5,caption = "Matrice de variance covariance ponderée pour chaque couple d'action")
    
    output$matrixxx <- renderTable({
      out2()
    }, digits = 5,caption = "Statsitique génerales")
    
  })
  
}


shinyApp(ui = ui, server = server)
