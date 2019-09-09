#### Aula de Nutri??o e Crescimento Animal####


#### Inputs ####
inp <- list(
  Dig = 0.75,  # Feed Dry Matter Digestibility (ul., DM)
  LW = 400     # Liveweight (kg)
)

#### Constants ####
ct <- list(
  
)

#### Parameters ####
par <- list(
  GE = 4.42, # Feed Gross Energy Concentration Mcal kg DM
  DEtoME = 0.82, # NRC Factor to convert ME to DE
  LWtoSBW = 0.96,
  SBWtoEBW = 0.891,
  alphaMaint = 0.077
#    
  ) 

#### Switches ####
op <- c() 

sw <- list(

) 

#### Functions ####

# NRC function to calculate NEm from ME (MCal/kg DM)
f.NRC.MEtoNEm <- function(ME){1.37*ME-0.138*ME^2+0.0105*ME^3-1.12}

# NRC function to calculate NEg from ME (MCal/kg DM)
f.NRC.MEtoNEg <- function(ME){1.42*ME-0.174*ME^2+0.0122*ME^3-1.65}

#### Algebraic (Auxiliary) Equations ####
# Diet energy concentration (MCal/kg DM)
a <- list()
a$ED <- par$GE*inp$Dig
a$ME <- a$ED*par$DEtoME
a$NEm <- f.NRC.MEtoNEm(ME = a$ME)
a$NEg <-  f.NRC.MEtoNEg(ME = a$ME)
a$SBW <- par$LWtoSBW * inp$LW
a$EBW <- par$SBWtoEBW * a$SBW
a$Maint <- par$alphaMaint * a$SBW^0.75
a$DMI <- a$SBW^0.75*(0.1493*a$NEm - 0.046*a$NEm^2 - 0.0196) 
a$RE <- (a$DMI - a$Maint/a$NEm)*a$NEg
a$EQEBW <- a$EBW
a$EBG <- 12.341*a.EQEBW^-0.6837*a$RE^0.9116
a$LWG <- a$EBG*inp$LW/a$EBW




#### Testing and Visualization ####
a

unlist(a)
#plot(f.NRC.MEtoNEm, from = 1, to = 2)
  
