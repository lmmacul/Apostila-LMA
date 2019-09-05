library(fields)
library(ggplot2)
library(plotrix)
library(purrr)
library(animation)

SeparateMat <- function(Mat,flag){
  
  NewMat = matrix(0 , ncol = ncol(Mat), nrow = nrow(Mat))
  for(u in 1:nrow(Mat)){
    for(o in 1:ncol(Mat)){
      
      NewMat[u,o] <- Mat[u,o][[1]][flag]
      
    }
  }
  return(NewMat)
}
# 
SeparateAg <- function(Age){
  Aux <- list()
  for(y in 1:length(Age)){
    Aux[[y]] <- c(Age[[y]][1],Age[[y]][2])
  }
  return(Aux)
}

rotate <- function(x) t(apply(x, 2, rev))
# 
SavePlo <- function(ListOfEspAndAgent){
  
  ListOfEspAndAgent <- Keep
  ListEsp <- list()
  ListAg <- list()
  
  for(q in 1:length(ListOfEspAndAgent)){
    ListEsp[[q]] <- ListOfEspAndAgent[[q]][1]
    ListAg[[q]] <- ListOfEspAndAgent[[q]][2]
  }
  
  
  frames = length(ListOfEspAndAgent)
  for(w in 1:frames){
    # creating a name for each plot file with leading zeros
    if (w < 10) {name = paste('000',w,'plot.png',sep='')}
    
    if (w < 100 && w >= 10) {name = paste('00',w,'plottest.png', sep='')}
    if (w >= 100) {name = paste('0', w,'plot.png', sep='')}
    
    #saves the plot as a .png file in the working directory
    png(name)
    x1 <-1:ncol(ListEsp[[1]][[1]])
    
    y1 <- 1:nrow(ListEsp[[1]][[1]])
    aux1 <- SeparateAg(ListAg[[w]][[1]])
    z <- rotate(SeparateMat(ListEsp[[w]][[1]],2))
    if(w == 1){
      print("1")
      image.plot(x1,y1,z, col = c("khaki","yellow","yellow3","gold","gold3"))
      grid(nx = ncol(ListEsp[[1]][[1]]) , ny = nrow(ListEsp[[1]][[1]]) ,lty = 1)
      for(e in 1:length(aux1)){
        points(aux1[[e]][2],aux1[[e]][1],col = "purple",pch = 19 ,cex = 1)
      }
    }
    image.plot(x1,y1,z, col = c("khaki","yellow","yellow3","gold","gold3"))
    grid(nx = ncol(ListEsp[[1]][[1]]) , ny = nrow(ListEsp[[1]][[1]]) ,lty = 1)
    for(r in 1:length(aux1)){
      points(aux1[[r]][2],aux1[[r]][1],col = "purple",pch = 19 ,cex = 1)
    }
    
    dev.off()
  }
  
}

CoordMatr2 <- function(Number, NumberofRow,NumberofCol){
  for(i in 1:NumberofRow){
    if(Number <= i*NumberofCol){
      Lin = i
      break
    }
  }
  Col =  Number - (i-1)*NumberofCol
  if(Number == Lin*NumberofCol){
    Col = NumberofCol
  }
  return(c(Linha = Lin,Coluna = Col))
}

Esp <- function(NumberOfRows, NumberofCol) {
  
  #SugarinPlace = sample(c(0:5),size = NumberOfRows*NumberofCol , replace  = T)
  #Capacidade = sample(c(1:5),size = NumberOfRows*NumberofCol , replace  = T)

   SugarinPlace = c(c(rep(0,17),rep(1,9),rep(2,7),rep(3,14),rep(2,4)),
                    c(rep(0,17),rep(1,9),rep(2,6),rep(3,16),rep(2,3)),
                      c(rep(0,16),rep(1,9),rep(2,6),rep(3,18),rep(2,2)),
                    c(rep(0,16),rep(1,9),rep(2,6),rep(3,19),rep(2,1)),
                    c(rep(0,16),rep(1,9),rep(2,6),rep(3,5),rep(4,8),rep(3,6),rep(2,1)),
                    c(rep(0,16),rep(1,9),rep(2,6),rep(3,5),rep(4,9),rep(3,6)),
                    c(rep(0,15),rep(1,10),rep(2,5),rep(3,5),rep(4,9),rep(3,7)),
                    c(rep(0,15),rep(1,9),rep(2,5),rep(3,6),rep(4,9),rep(3,7)),
                    c(rep(0,14),rep(1,10),rep(2,6),rep(3,5),rep(4,12),rep(3,4)),
                    c(rep(0,13),rep(1,12),rep(2,5),rep(3,5),rep(4,12),rep(3,4)),
                    c(rep(0,11),rep(1,14),rep(2,5),rep(3,5),rep(4,12),rep(3,4)),
                    c(rep(0,10),rep(1,15),rep(2,5),rep(3,5),rep(4,12),rep(3,4)),
                    c(rep(0,8),rep(1,17),rep(2,5),rep(3,5),rep(4,12),rep(3,4)),
                    c(rep(0,3),rep(1,21),rep(2,6),rep(3,5),rep(4,12),rep(3,4)),
                    c(rep(0,1),rep(1,20),rep(2,7),rep(3,7),rep(4,9),rep(3,6),rep(2,1)),
                    c(rep(0,1),rep(1,20),rep(2,7),rep(3,7),rep(4,9),rep(3,6),rep(2,1)),
                    c(rep(1,19),rep(2,14),rep(3,6),rep(4,4),rep(3,6),rep(2,2)),
                    c(rep(1,19),rep(2,14),rep(3,15),rep(2,3)),
                    c(rep(1,16),rep(2,14),rep(3,16),rep(2,5)),
                    c(rep(1,14),rep(2,15),rep(3,16),rep(2,6)),
                  rev(c(rep(0,17),rep(1,9),rep(2,7),rep(3,14),rep(2,4))),
                    rev(c(rep(0,17),rep(1,9),rep(2,6),rep(3,16),rep(2,3))),
                    rev(c(rep(0,16),rep(1,9),rep(2,6),rep(3,18),rep(2,2))),
                    rev(c(rep(0,16),rep(1,9),rep(2,6),rep(3,19),rep(2,1))),
                    rev( c(rep(0,16),rep(1,9),rep(2,6),rep(3,5),rep(4,8),rep(3,6),rep(2,1))),
                    rev( c(rep(0,16),rep(1,9),rep(2,6),rep(3,5),rep(4,9),rep(3,6))),
                    rev(  c(rep(0,15),rep(1,10),rep(2,5),rep(3,5),rep(4,9),rep(3,7))),
                    rev(c(rep(0,15),rep(1,9),rep(2,5),rep(3,6),rep(4,9),rep(3,7))),
                    rev(c(rep(0,14),rep(1,10),rep(2,6),rep(3,5),rep(4,12),rep(3,4))),
                    rev(c(rep(0,13),rep(1,12),rep(2,5),rep(3,5),rep(4,12),rep(3,4))),
                    rev(c(rep(0,11),rep(1,14),rep(2,5),rep(3,5),rep(4,12),rep(3,4))),
                    rev(c(rep(0,10),rep(1,15),rep(2,5),rep(3,5),rep(4,12),rep(3,4))) ,
                    rev(c(rep(0,8),rep(1,17),rep(2,5),rep(3,5),rep(4,12),rep(3,4))),
                    rev(c(rep(0,3),rep(1,21),rep(2,6),rep(3,5),rep(4,12),rep(3,4))),
                    rev(c(rep(0,1),rep(1,20),rep(2,7),rep(3,7),rep(4,9),rep(3,6),rep(2,1))),
                    rev(c(rep(0,1),rep(1,20),rep(2,7),rep(3,7),rep(4,9),rep(3,6),rep(2,1))),
                    rev( c(rep(1,19),rep(2,14),rep(3,6),rep(4,4),rep(3,6),rep(2,2))),
                    rev(c(rep(1,19),rep(2,14),rep(3,15),rep(2,3))),
                    rev(c(rep(1,16),rep(2,14),rep(3,16),rep(2,5))),
                    rev( c(rep(1,14),rep(2,15),rep(3,16),rep(2,6))))
   Capacidade = SugarinPlace

  #Matr <- matrix(sample(c(0:5),size = NumberOfRows*NumberofCol , replace  = T),nrow = NumberOfRows , ncol = NumberofCol)
  Aux <- list()
  for(j in 1:(NumberofCol*NumberOfRows)){
    Aux[[j]] = c( SugarinPlace = SugarinPlace[j],Capacidade = Capacidade[j],Ocupado = 1)
    
  }
  Matr <- matrix(Aux,nrow = NumberOfRows , ncol = NumberofCol)
  
  return(Matr)
}

##Lembrar que temos que definir a capacidade maxima
#vision - 1:6
Agt <- function(NumberofAgents,NumberofRow,NumberofCol){
  
  Agents <- list()
  # Lugares <- list()
  LugarNoGrid <- sample(c(1:(NumberofRow*NumberofCol)),size = NumberofAgents)
  
  # for(j in 1:NumberofAgents){
  #   Lugares[[j]] <- c(CoordMatr2(LugarNoGrid[j],NumberofRow,NumberofCol))
  # }
  
  for(i in 1:NumberofAgents){
    
    Agents[[i]] <-c(CoordMatr2(LugarNoGrid[i],NumberofRow,NumberofCol), c(Sugar = sample(c(5:25),size = 1),Metabolism = sample(c(1:4),size = 1), Vision = 1 ))
  }
  
  
  
  return(Agents)
}

GridR <- function(Grid){
  #Every 1 set of time
  for(l in 1:nrow(Grid)){
    for(k in 1:ncol(Grid)){
      if(Grid[l,k][[1]][3] == 1){
        if(Grid[l,k][[1]][1] == 0 ){
          SugarGrowBack = Grid[l,k][[1]][2]
          Grid[l,k][[1]][1] = Grid[l,k][[1]][1]  + SugarGrowBack
        }
      }
      
    }
  }
  return(Grid)
}

MakeOccup <- function(Grid,Agent){
  for(i in 1:nrow(Grid)){
    for(j in 1:ncol(Grid)){
      
      Grid[i,j][[1]][3] = 1
    }
  }
  
  for(i in 1:length(Agent)){
    Grid[Agent[[i]][1],Agent[[i]][2]][[1]][3] = 0
    Grid[Agent[[i]][1],Agent[[i]][2]][[1]][1] = 0
  }
  return(Grid)
}

MoveUp <- function(Agtn,Grid){

  Grid[Agtn[1],Agtn[2]][[1]][3] = 1
  Grid[Agtn[1],Agtn[2]][[1]][1] = 0
  Agtn[1] <- Agtn[1] - 1

  Grid[Agtn[1],Agtn[2]][[1]][3] = 0
  Agtn[3] = Agtn[3] + Grid[Agtn[1],Agtn[2]][[1]][1]
  Grid[Agtn[1],Agtn[2]][[1]][1] = 0

  return(list(c(Agtn[1],Agtn[2]),Grid))
}

MoveDown <- function(Agtn,Grid){
  Grid[Agtn[1],Agtn[2]][[1]][3] = 1
  Grid[Agtn[1],Agtn[2]][[1]][1] = 0
  Agtn[1] <- Agtn[1] + 1
  
  Grid[Agtn[1],Agtn[2]][[1]][3] = 0
  Agtn[3] = Agtn[3] + Grid[Agtn[1],Agtn[2]][[1]][1]
  Grid[Agtn[1],Agtn[2]][[1]][1] = 0
  
  return(list(c(Agtn[1],Agtn[2]),Grid)) 
}

MoveLeft <- function(Agtn,Grid){
  Grid[Agtn[1],Agtn[2]][[1]][3] = 1 
  Grid[Agtn[1],Agtn[2]][[1]][1] = 0
  Agtn[2] <- Agtn[2] - 1
  
  Grid[Agtn[1],Agtn[2]][[1]][3] = 0
  Agtn[3] = Agtn[3] + Grid[Agtn[1],Agtn[2]][[1]][1]
  Grid[Agtn[1],Agtn[2]][[1]][1] = 0
  
  return(list(c(Agtn[1],Agtn[2]),Grid)) 
}

MoveRight <- function(Agtn,Grid){
  Grid[Agtn[1],Agtn[2]][[1]][3] = 1 
  Grid[Agtn[1],Agtn[2]][[1]][1] = 0
  Agtn[2] <- Agtn[2] + 1
  
  Grid[Agtn[1],Agtn[2]][[1]][3] = 0
  Agtn[3] = Agtn[3] + Grid[Agtn[1],Agtn[2]][[1]][1]
  Grid[Agtn[1],Agtn[2]][[1]][1] = 0
  
  return(list(c(Agtn[1],Agtn[2]),Grid)) 
}

L1C1 <- function(Quanti,Habilita,EspGrid,Agtns){
  
  Quanty <- unname(Quanti)
  Habid <- unname(Habilita)
  Aux = Quanty*Habid
  MaxPos <- which.max(Aux)
  
  if(Aux[MaxPos] != 0 ){
    if(MaxPos == 1){
      AuxLin = MoveRight(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveRight(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveRight(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 2){
      AuxLin = MoveDown(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveDown(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveDown(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
  }
  return(list(Agtns,EspGrid))
}

L1CL <- function(Quanti,Habilita,EspGrid,Agtns){
  
  Quanty <- unname(Quanti)
  Habid <- unname(Habilita)
  Aux = Quanty*Habid
  
  MaxPos <- which.max(Aux)
  
  if(Aux[MaxPos] != 0 ){
      if(MaxPos == 1){
      AuxLin = MoveDown(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveDown(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveDown(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 2){
      AuxLin = MoveLeft(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveLeft(Agtns,EspGrid)[[1]][2]
      AuxMa   = MoveLeft(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
   
  }
  return(list(Agtns,EspGrid))
}

LLCL <- function(Quanti,Habilita,EspGrid,Agtns){
  
  Quanty <- unname(Quanti)
  Habid <- unname(Habilita)
  Aux = Quanty*Habid
  
  MaxPos <- which.max(Aux)
  
  if(Aux[MaxPos] != 0 ){
    if(MaxPos == 1){
      AuxLin = MoveUp(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveUp(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveUp(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    
    if(MaxPos == 2){
      AuxLin = MoveLeft(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveLeft(Agtns,EspGrid)[[1]][2]
      AuxMa   = MoveLeft(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    
  }
  return(list(Agtns,EspGrid))
}

LLC1 <- function(Quanti,Habilita,EspGrid,Agtns){
  
  Quanty <- unname(Quanti)
  Habid <- unname(Habilita)
  Aux = Quanty*Habid
  
  MaxPos <- which.max(Aux)
  
  if(Aux[MaxPos] != 0 ){
    if(MaxPos == 1){
      AuxLin = MoveUp(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveUp(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveUp(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 2){
      AuxLin = MoveRight(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveRight(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveRight(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    
  }
  return(list(Agtns,EspGrid))
}

L1 <- function(Quanti,Habilita,EspGrid,Agtns){
  
  Quanty <- unname(Quanti)
  Habid <- unname(Habilita)
  Aux = Quanty*Habid
  
  MaxPos <- which.max(Aux)
  
  if(Aux[MaxPos] != 0 ){
    if(MaxPos == 1){
      AuxLin = MoveRight(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveRight(Agtns,EspGrid)[[1]][2]
      AuxMa  = MoveRight(Agtns,EspGrid)[[2]]
      
       Agtns[1] = AuxLin
       Agtns[2] = AuxCol
       EspGrid = AuxMa
    }
    if(MaxPos == 2){
      AuxLin = MoveDown(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveDown(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveDown(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 3){
      AuxLin = MoveLeft(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveLeft(Agtns,EspGrid)[[1]][2]
      AuxMa  = MoveLeft(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
     
    }
  }
  return(list(Agtns,EspGrid))
}

LL <- function(Quanti,Habilita,EspGrid,Agtns){
  
  Quanty <- unname(Quanti)
  Habid <- unname(Habilita)
  Aux = Quanty*Habid
  
  MaxPos <- which.max(Aux)
  
  if(Aux[MaxPos] != 0 ){
    if(MaxPos == 1){
      AuxLin = MoveUp(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveUp(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveUp(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 2){
      AuxLin = MoveRight(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveRight(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveRight(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
      
    }
    if(MaxPos == 3){
      AuxLin = MoveLeft(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveLeft(Agtns,EspGrid)[[1]][2]
      AuxMa   = MoveLeft(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
      
    }
  }
  return(list(Agtns,EspGrid))
}

C1 <- function(Quanti,Habilita,EspGrid,Agtns){
  
  Quanty <- unname(Quanti)
  Habid <- unname(Habilita)
  Aux = Quanty*Habid
  
  MaxPos <- which.max(Aux)
  
  if(Aux[MaxPos] != 0 ){
    if(MaxPos == 1){
      AuxLin = MoveUp(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveUp(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveUp(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 2){
      AuxLin = MoveRight(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveRight(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveRight(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 3){
      AuxLin = MoveDown(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveDown(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveDown(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
  }
  return(list(Agtns,EspGrid))
}

CL <- function(Quanti,Habilita,EspGrid,Agtns){
  
  Quanty <- unname(Quanti)
  Habid <- unname(Habilita)
  Aux = Quanty*Habid
  
  MaxPos <- which.max(Aux)
  
  if(Aux[MaxPos] != 0 ){
    if(MaxPos == 1){
      AuxLin = MoveUp(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveUp(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveUp(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 2){
      AuxLin = MoveDown(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveDown(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveDown(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 3){
      AuxLin = MoveLeft(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveLeft(Agtns,EspGrid)[[1]][2]
      AuxMa   = MoveLeft(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
  }
  return(list(Agtns,EspGrid))
}
#0 é ocupapdo e 1 é desocupado
Middle <- function(Quanti,Habilita,EspGrid,Agtns){
  
  Quanty <- unname(Quanti)
  Habid <- unname(Habilita)
  Aux = Quanty*Habid

  MaxPos <- which.max(Aux)

  if(Aux[MaxPos] != 0 ){
    if(MaxPos == 1){
      AuxLin = MoveUp(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveUp(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveUp(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 2){
      AuxLin = MoveRight(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveRight(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveRight(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
    }
    if(MaxPos == 3){
      AuxLin = MoveDown(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveDown(Agtns,EspGrid)[[1]][2]
      AuxMa = MoveDown(Agtns,EspGrid)[[2]]
      
      Agtns[1] = AuxLin
      Agtns[2] = AuxCol
      EspGrid = AuxMa
     
    }
    if(MaxPos == 4){
      
      AuxLin = MoveLeft(Agtns,EspGrid)[[1]][1]
      AuxCol = MoveLeft(Agtns,EspGrid)[[1]][2]
      AuxMa   = MoveLeft(Agtns,EspGrid)[[2]]
     
     Agtns[1] = AuxLin
     Agtns[2] = AuxCol
     EspGrid = AuxMa
    
      
    }
  }
  return(list(Agtns,EspGrid))
}

AgentsRUles <- function(Grid,Agent){
  
  PosicLin <- Agent[1]
  PosicCol <- Agent[2]
  
  if(PosicLin == 1 && PosicCol == 1){
    Hab <- c(Grid[1,2][[1]][3],Grid[2,1][[1]][3])
    Acuca <-  c(Grid[1,2][[1]][1],Grid[2,1][[1]][1])
    NewGrid = L1C1(Acuca,Hab,Grid,Agent)[[1]]
    NewAgent = L1C1(Acuca,Hab,Grid,Agent)[[2]]
   # print("1")
    
  }
  if(PosicLin == 1 && PosicCol == ncol(Grid)){
    #print("2")
    Hab <- c(Grid[2,ncol(Grid)][[1]][3],Grid[1,ncol(Grid)-1][[1]][3])
    Acuca <-  c(Grid[2,ncol(Grid)][[1]][1],Grid[1,ncol(Grid)-1][[1]][1])
    NewGrid = L1CL(Acuca,Hab,Grid,Agent)[[1]]
    NewAgent = L1CL(Acuca,Hab,Grid,Agent)[[2]]
  }
  if(PosicLin == nrow(Grid) && PosicCol == 1){
    #print("3")
    Hab <- c(Grid[nrow(Grid)-1,1][[1]][3],Grid[nrow(Grid),2][[1]][3])
    Acuca <- c(Grid[nrow(Grid)-1,1][[1]][1],Grid[nrow(Grid),2][[1]][1])
    NewGrid = LLC1(Acuca,Hab,Grid,Agent)[[1]]
    NewAgent = LLC1(Acuca,Hab,Grid,Agent)[[2]]
    
  }
  if(PosicLin == nrow(Grid) && PosicCol == ncol(Grid)){
   # print("4")
    Hab <- c(Grid[nrow(Grid)-1,ncol(Grid)][[1]][3],Grid[nrow(Grid),ncol(Grid)-1][[1]][3])
    Acuca <- c(Grid[nrow(Grid)-1,ncol(Grid)][[1]][1],Grid[nrow(Grid),ncol(Grid)-1][[1]][1])
    NewGrid = LLCL(Acuca,Hab,Grid,Agent)[[1]]
    NewAgent = LLCL(Acuca,Hab,Grid,Agent)[[2]]
    
  }
  if(PosicLin == 1 && PosicCol != 1 && PosicCol != ncol(Grid)){
    #print("5")
    Hab <- c(Grid[PosicLin,PosicCol+1][[1]][3],Grid[PosicLin+1,PosicCol][[1]][3],Grid[PosicLin,PosicCol-1][[1]][3])
    Acuca <- c(Grid[PosicLin,PosicCol+1][[1]][1],Grid[PosicLin+1,PosicCol][[1]][1],Grid[PosicLin,PosicCol-1][[1]][1])
    NewGrid = L1(Acuca,Hab,Grid,Agent)[[1]]
    NewAgent = L1(Acuca,Hab,Grid,Agent)[[2]]
    
  }
  
  if(PosicLin == nrow(Grid) && PosicCol != 1 &&  PosicCol != ncol(Grid)){
   # print("6")
    Hab <- c(Grid[PosicLin-1,PosicCol][[1]][3],Grid[PosicLin,PosicCol+1][[1]][3],Grid[PosicLin,PosicCol-1][[1]][3])
    Acuca <- c(Grid[PosicLin-1,PosicCol][[1]][1],Grid[PosicLin,PosicCol+1][[1]][1],Grid[PosicLin,PosicCol-1][[1]][1])
    NewGrid = LL(Acuca,Hab,Grid,Agent)[[1]]
    NewAgent = LL(Acuca,Hab,Grid,Agent)[[2]]
      }
  if(PosicCol == 1 && PosicLin != 1 && PosicLin != nrow(Grid)){
    #print("7")
    Hab <- c(Grid[PosicLin-1,PosicCol][[1]][3],Grid[PosicLin,PosicCol+1][[1]][3],Grid[PosicLin-1,PosicCol][[1]][3])
    Acuca <- c(Grid[PosicLin-1,PosicCol][[1]][1],Grid[PosicLin,PosicCol+1][[1]][1],Grid[PosicLin-1,PosicCol][[1]][1])
    NewGrid = C1(Acuca,Hab,Grid,Agent)[[1]]
    NewAgent = C1(Acuca,Hab,Grid,Agent)[[2]]
    
  }
  if(PosicCol == ncol(Grid) && PosicLin != 1 && PosicLin != nrow(Grid)){
    #print("8")
    Hab <- c(Grid[PosicLin-1,PosicCol][[1]][3],Grid[PosicLin+1,PosicCol][[1]][3],Grid[PosicLin,PosicCol-1][[1]][3])
    Acuca <- c(Grid[PosicLin-1,PosicCol][[1]][1],Grid[PosicLin+1,PosicCol][[1]][1],Grid[PosicLin,PosicCol-1][[1]][1])
    NewGrid = CL(Acuca,Hab,Grid,Agent)[[1]]
    NewAgent = CL(Acuca,Hab,Grid,Agent)[[2]]
    
  }
  if(PosicCol != ncol(Grid) && PosicLin != 1 && PosicLin != nrow(Grid) && PosicCol != 1){
    #print("9")
    Hab <- c(Grid[PosicLin-1,PosicCol][[1]][3],Grid[PosicLin,PosicCol+1][[1]][3],Grid[PosicLin+1,PosicCol][[1]][3],Grid[PosicLin,PosicCol-1][[1]][3])
    Acuca <- c(Grid[PosicLin-1,PosicCol][[1]][1],Grid[PosicLin,PosicCol+1][[1]][1],Grid[PosicLin+1,PosicCol-1][[1]][1],Grid[PosicLin,PosicCol-1][[1]][1])
    NewGrid = Middle(Acuca,Hab,Grid,Agent)[[1]]
    NewAgent = Middle(Acuca,Hab,Grid,Agent)[[2]]
  }
  
  return(list(NewGrid,NewAgent))
}

MainProgram <- function(Time,Grid,Agentes){
  
  NewAgentes <- list()
  Aux <- list()
 
   for(i in 1:length(Agentes)){
     Agentes[[i]][3] =  Agentes[[i]][3] + Grid[Agentes[[i]][1],Agentes[[i]][2]][[1]][1]
     Grid[Agentes[[i]][1],Agentes[[i]][2]][[1]][1]= 0
   }
   NewGrid <- MakeOccup(Grid,Agentes)
  
   for(t in 1:Time){
     #NewGrid <- GridR(NewGrid)
     for(k in 1:length(Agentes)){
       Aux <- AgentsRUles(NewGrid,Agentes[[k]])
       Agentes[[k]] <- Aux[[1]]
       NewGrid <- Aux[[2]]
     }
      for(m in 1:length(Agentes)){
        Agentes[[m]][3] = Agentes[[m]][3] - Agentes[[m]][4]
   
        if(Agentes[[m]][3] <= 0){
         NewGrid[Agentes[[m]][1],Agentes[[m]][2]][[1]][3] = 1
          NewAgentes  <- Agentes[-m]
   
        }
        
      }
      if(length(NewAgentes) != 0 ){
        Agentes = NewAgentes
      }
     NewGrid <- MakeOccup(Grid,Agentes)
     NewGrid <- GridR(NewGrid)
     for(i in 1:length(Agentes)){
       Grid[Agentes[[i]][1],Agentes[[i]][2]][[1]][1]= 0
     }
    }

  return(list(NewGrid,Agentes))
}

a <- Esp(51,40)
b <- Agt(151,51,40)
# c <- MakeOccup(a,b)
# d1 <- MainProgram(1,a,b)
# d2 <- MainProgram(2,a,b)
# d3 <- MainProgram(3,a,b)
# d4 <- MainProgram(4,a,b)
# d5 <- MainProgram(5,a,b)
#  hh1 <- d[[1]]
#  hh2 <- d[[2]]
# 
#  
# xx <- 1:nrow(a)
# yy <- 1:ncol(a)
# 
# #gg <- SeparateAg(b)
# gg <- SeparateAg(d2[[2]])
# #bb <- SeparateMat(c,2)
# bb <- SeparateMat(d2[[1]],1)
# #cc <- Invert(bb)
# #cc <- rotate(Invert(rotate(bb)))


image.plot(xx,yy,bb,col = c("white","khaki","yellow","yellow3","gold","gold3"))
for(j in 1:length(gg)){
  points(gg[[j]][2],gg[[j]][1],col = "purple",pch = 19 ,cex = 1)
  grid(nx = nrow(a), ny = ncol(a) , lty = 1)
}


Gi <- function(Ag,Be,time){
  
  tenta <-list()
  At <- MakeOccup(Ag,Be)
  tenta[[1]]<- list(At,Be)
  for(qw in 2:time){
    tenta[[qw]] <-MainProgram(1,tenta[[qw-1]][[1]],tenta[[qw-1]][[2]])
  }
  
  for(z in 1:length(tenta)){
    ti <- tenta[[z]][[1]]
    te <- tenta[[z]][[2]]
    
    gg <- SeparateAg(te)
    bb <- SeparateMat(ti,1)
    #cc <- Invert(bb)
    cc <- bb
    #cc <- rotate(rotate(bb))
    
    xx <- 1:nrow(a)
    yy <- 1:ncol(a)
    image.plot(xx,yy,cc,col = c("white","khaki","yellow","yellow3","gold","gold3"))
    grid(nx = nrow(a) , ny = ncol(a) ,lty =1)
    for(j in 1:length(gg)){
      points(gg[[j]][1],gg[[j]][2],col = "purple",pch = 19 ,cex = 1)
    }
    # image.plot(xx,yy,bb,col = c("white","khaki","yellow","yellow3","gold","gold3"))
    # grid(nx = nrow(a) , ny = ncol(a) ,lty =1)
    # for(j in 1:length(gg)){
    #   points(gg[[j]][1],gg[[j]][2],col = "purple",pch = 19 ,cex = 1)
    # }
  }
  return(tenta)
}

Gi(a,b,2)
rt <- Gi(a,b,50)
saveGIF(Gi(a,b,50))
