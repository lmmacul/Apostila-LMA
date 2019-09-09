#### Aula de Nutri??o e Crescimento Animal####

library(deSolve)

#### Inputs ####
inp <- list(

)

#### Initial State ####
st <- list(

)

#### Constants ####
ct <- list(
  
  ) 
  
#### Parameters ####
par <- list(
  
  ) 

#### Switches ####
op <- c() 

sw <- list(

) 

#### Functions ####


#### Algebraic (Auxiliary) Equations ####
# Diet energy concentration (MCal/kg DM)

f.aux <- function(){
  
  assign("aux", value = aux, envir = .GlobalEnv)
}

#### Differential Equations ####

f.ODE <- function (t, y, par)
{
  st <- as.list(y)

  f.aux() # Calculate auxiliary
  
  return(list(c(), aux))  # outputs a list with the differential as required by deSolve::ode funcion                                                                       
  
}

#### Initialize ####

aux <- list()

# Initialize State

# Initialize Parameters


#### Simulate ####

#Traj <- ode(y = unlist(st), times = seq(1, 100, 1), func = f.ODE, parms = par)


