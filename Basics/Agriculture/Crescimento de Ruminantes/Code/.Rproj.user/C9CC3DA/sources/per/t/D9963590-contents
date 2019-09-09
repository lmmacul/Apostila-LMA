#### Aula de Nutri??o e Crescimento Animal####

library(deSolve)

#### Inputs ####
inp <- list(
  LWi = 350,
  BCSi = 5,            # Body condition score (dimensionless, 1 - 9)
  DMI = 7.41,         # Dry Matter Intake Rate (kg/day) as a fixed value or as a function of time (using approxfun)
  Frame = 1,          # Multiplier of final liveweight (dimensionless)
  LWGp = 0.5,
  Feed_MEC = 2.84,
  Feed_NEm = 1.9,
  Feed_NEg = 1.26
)

st <- list(
  Prot = NA,
  Fat = NA,
  DNA = NA
)

#### Constants ####
ct <- list(EPROT = 5.539,      # Protein Energy Concentration (Mcal/kg)
           EFAT = 9.385,        # Fat Energy Concentration
           LEAN_PROT = 0.2201,  # Proportion of protein in lean mass
           EBWMref = 750,       # Ref EBW for k1 correction
           DNAmax_Ref = 385     # Ref DNAmax for Frame corrections
  ) 
  
#### Parameters ####
par <- list(kD = 0.00493,        # Maturing rate parameter
            kPs = 0.0444,         # Protein synthesis parameter
            kPd = 0.143,          # Protein degradation parameter 
            alpha = 0.0841,      # Individual maintenance parameter
            DNAmax = 385,        # Maximum DNA mass (g)
            EBWM = 750,          # Mature body weight
            MEI_0 = 0.438,       # First parameter of the equation of reference ME intake
            MEI_1 = 0.2615,      # Second parameter of the equation of reference ME intake
            beta.D_0 = -0.70,      # First parameter of the equation of the effect of nutrition on rate of de DNA deposition
            beta.D_1 = 1.7,        # Second parameter of the equation of the effect of nutrition on rate of de DNA deposition
            gamma.P_0 = 0.83,       # First parameter of the equation of the effect of nutrition on the rate of protein synthesis
            gamma.P_1 = 0.20,       # Second parameter of the equation of the effect of nutrition on the rate of protein synthesis
            gamma.P_2 = 0.15,       # Third parameter of the equation of the effect of nutrition on the rate of protein synthesis
            LW_to_EBW = 0.96* 0.891     # Empty Body Weight as a propotion of Liveweight (dimensionless)
  ) 

#### Switches ####
op <- c() 

sw <- list(

) 

#### Functions ####

f.Michalis.Menten <- function(x, y.intcpt = 0, y.max = 1, K = 0.5){
  y.intcpt + (y.max - y.intcpt)*x/(K + x)
}

f.FatProp <- function(EQEBW, EBWM, BCS, EBWM_ref = 750){
  BW_Eq = (EBWM_ref/EBWM)*EQEBW 
  return((0.333+0.0833*BCS)*(BW_Eq-54.6)/(826+BW_Eq))
}

#### Algebraic (Auxiliary) Equations ####
# Diet energy concentration (MCal/kg DM)

f.aux <- function(){
  aux$MEI <- inp$DMI * inp$Feed_MEC
  aux$EBW <- st$Prot/ct$LEAN_PROT + st$Fat
  aux$Maturity <- st$Prot/par$Protmax
  aux$MEInorm <- (par$MEI_0 - par$MEI_1*aux$Maturity)*aux$EBW^0.73
  aux$MaintNE <- par$alpha*aux$EBW^0.75                                                      # Net Energy required for maintenance
  aux$MaintDMI <- aux$MaintNE/inp$Feed_NEm                                                   # DMI required for maintenance
  aux$RE <- (inp$DMI - aux$MaintDMI)*inp$Feed_NEg                                            # Retained Energy 
  aux$ER <- aux$MEI/aux$MEInorm
  aux$ND <- par$beta.D_0 + par$beta.D_1*aux$ER
  aux$NP <- f.Michalis.Menten(x = aux$ER, 
                              y.intcpt = par$gamma.P_0, 
                              y.max = par$gamma.P_0 + par$gamma.P_1, 
                              K = par$gamma.P_2) 
  assign("aux", value = aux, envir = .GlobalEnv)
}

#### Differential Equations ####

Oltjen.Diff <- function (t, y, par)
{
  st <<- as.list(y) 
  
  f.aux() # Compute auxiliary

  # Compute differentials
  dDNA <- par$kD*aux$ND*(par$DNAmax - st$DNA)                                # d DNA mass/dt
  dPROT <- par$kPs * aux$NP * st$DNA^0.73 - par$kPd * st$Prot^(0.73)         # d Protein mass/dt
  dFAT <- (aux$RE - dPROT * ct$EPROT)/ct$EFAT                                # d Fat mass/dt
  
  return(list(c(dPROT = dPROT, dFAT = dFAT, dDNA = dDNA), aux))              # outputs a list with the differential as required by deSolve:ode funcion                                                                       
  
}

#### Initialize ####

aux <- list()
init <- list()

# Initialize State
init$EBW <- inp$LWi*par$LW_to_EBW
init$EQEBW <- init$EBW/inp$Frame
init$Fatfrac <- f.FatProp(EQEBW = init$EQEBW, EBWM = inp$Frame*ct$EBWMref, 
                          BCS = inp$BCS, EBWM_ref = ct$EBWMref) 
st$Fat <- init$EBW * init$Fatfrac 
st$Prot <- (init$EBW -  st$Fat) * ct$LEAN_PROT 
init$RE <- 0.0625 * init$EQEBW^0.75 * (inp$LWGp* par$LW_to_EBW)^1.097
init$PIG <- 0.248 - 0.0264 * init$RE/inp$LWGp   
st$DNA <- ((par$kPd*st$Prot^0.73 + inp$LWGp*par$LW_to_EBW * init$PIG)/par$kPs)^(1/0.73)  

# Initialize Parameters
par$Protmax <- par$DNAmax*(par$kPs/par$kPd)^1/0.73


#### Simulate ####

Traj <- ode(y = unlist(st), times = seq(1, 300, 1), func = Oltjen.Diff, parms = par)

#### Testing and Visualization ####
Fatp <- Traj[, "Fat"]/ Traj[, "EBW"]

View(Traj)
colnames(Traj)
par(mfcol= c(2, 2))
plot(Traj[, "time"], Traj[, "EBW"])
plot(Traj[, "time"], c(diff(Traj[, "EBW"]), NA))
plot(Traj[, "EBW"], Fatp)
