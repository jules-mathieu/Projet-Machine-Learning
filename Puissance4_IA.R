require(plotrix)
require(gtools)

### my.circle.P4 prend 3 arguments
### - x : l'abscisse du centre du cercle
### - y : l'ordonnee du centre du cercle
### - r : rayon du cercle
### - lwd : le type de la ligne du cercle
### - col : une valeur de couleur pour le disque
###
### my.circle.P4 ne revoie rien mais affiche un cercle à un graphique existant
###
my.circle.P4 <- function(x,y,r,lwd=1,col=1){
  angle <- seq(0,2*pi,length=100)
  xx <- x+r*cos(angle)
  yy <- y+r*sin(angle)
  polygon(xx,yy,col=col,border=1)
}

### EndOfGame.Puissance4 prend 1 argument
### - M : une matrice de taille 6x7
###
### EndOfGame.Puissance4 revoie un boolean (TRUE: si plus de case vide, FALSE: sinon)
###
EndOfGame.Puissance4 <- function(M){
  if (any(is.na(M))){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}



### Affiche.Puissance4 prend 1 argument
### - M : une matrice de taille 3x3
###
### Affiche.Puissance4 ne revoie rien mais affiche le jeu dans l'etat de la matrice M
###
Affiche.Puissance4 <- function(M){
  M <- t(M)
  mar=c(0.1,0.1,0.1,1.1)
  plot(c(0,7),c(0,6),xlim=c(0,7),ylim=c(0,6),col=0,yaxt="n",xaxt="n",xlab="",ylab="")
  segments(0,0,0,6)
  segments(0,0,7,0)
  segments(7,6,7,0)
  segments(7,6,0,6)
  for (i in 1:7){
    for (j in 1:6){
      if (is.na(M[i,j])){
        rect(i-1,j-1,i,j,col=0)
      } else if (M[i,j]=="+"){
        rect(i-1,j-1,i,j,col=0)
        my.circle.P4(x=i-0.5,y=j-0.5,r=0.3,lwd=2,col=2)
      } else if (M[i,j]=="-"){
        rect(i-1,j-1,i,j,col=0)
        my.circle.P4(x=i-0.5,y=j-0.5,r=0.3,lwd=2,col=7)
      }
    }
  }
}

### caseVide prend 3 arguments
### - M : une matrice de taille 3x3
### - i : l'indice de la ligne
### - j : l'indice de la colonne
###
### caseVide revoie un boolean (TRUE: si la case (i,j) du jeu M est NA, FALSE: sinon)
###
caseVide <- function(M,i,j){
  return(is.na(M[i,j]))
}

### Gagnant.Puissance4 prend 3 arguments
### - M : une matrice de taille 3x3
### - i : un indice de ligne
### - j : un indice de colonne
###
### Gagnant.Puissance4 ne revoie un boolean si le case (i,j) dans la matrice M fait partie d'une serie de 4 pions gagnantes
###
Gagnant.Puissance4 <- function(M,i,j){
  res <- FALSE ### Par defaut la case (i,j) n'est pas dans une configuration gagnante
  ### Dans la suite, toutes les possibilites sont testees
  ##### Cas 1 : Horizontal si 1 <= j <= 4
  if (j<=4){ 
    tmp <- all(M[i,1:4]==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 2 : Horizontal si 2 <= j <= 5
  if (j>=2 & j <= 5){
    tmp <- all(M[i,2:5]==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 3 : Horizontal si 3 <= j <= 6
  if (j>=3 & j <= 6){
    tmp <- all(M[i,3:6]==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 4 : Horizontal si 4 <= j <= 7
  if (j>=4 & j <= 7){
    tmp <- all(M[i,4:7]==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 5 : Vertical si 1 <= i <= 4
  if (i<=4){
    tmp <- all(M[1:4,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 6 : Vertical si 2 <= i <= 5
  if (i>=2 & i <= 5){
    tmp <- all(M[2:5,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 7 : Vertical si 3 <= i <= 6
  if (i>=3 & i <= 6){
    tmp <- all(M[3:6,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 8 : Diagonal Droite si 1 <= i <= 4 et 1 <= j <= 4
  if ( (i+3 <= 6) & (j+3 <= 7)){
    tmp <- all(c(M[i,j],M[i+1,j+1],M[i+2,j+2],M[i+3,j+3])==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 9 : Diagonal Droite si 2 <= i <= 5 et 2 <= j <= 5
  if ( (i+2 <= 6) & (j+2 <= 7) & (i-1 >=1) & (j-1 >= 1)){
    tmp <- all(c(M[i-1,j-1],M[i,j],M[i+1,j+1],M[i+2,j+2])==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 10 : Diagonal Droite si 3 <= i <= 6 et 3 <= j <= 7
  if ( (i+1 <= 6) & (j+1 <= 7) & (i-2 >=1) & (j-2 >= 1)){
    tmp <- all(c(M[i-2,j-2],M[i-1,j-1],M[i,j],M[i+1,j+1])==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 11 : Diagonal Droite si 4 <= i <= 6 et 4 <= j <= 7
  if ( (i-3 >=1) & (j-3 >= 1)){
    tmp <- all(c(M[i-3,j-3],M[i-2,j-2],M[i-1,j-1],M[i,j])==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ## Diagonal Gauche - identique aux diagnoles droite mais dans l'autre sens
  if ( (i+3 <= 6) & (j-3 >= 1)){
    tmp <- all(c(M[i,j],M[i+1,j-1],M[i+2,j-2],M[i+3,j-3])==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  if ( (i+2 <= 6) & (j-2 >= 1) & (i-1 >=1) & (j+1 <=7)){
    tmp <- all(c(M[i-1,j+1],M[i,j],M[i+1,j-1],M[i+2,j-2])==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  if ( (i+1 <= 6) & (j-1 >= 1) & (i-2 >=1) & (j+2 <=7)){
    tmp <- all(c(M[i-2,j+2],M[i-1,j+1],M[i,j],M[i+1,j-1])==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  if ( (i-3 >=1) & (j+3 <=7)){
    tmp <- all(c(M[i-3,j+3],M[i-2,j+2],M[i-1,j+1],M[i,j])==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  return(res)
}

### Gagnant.Puissance4.plot prend 3 arguments
### - M : une matrice de taille 3x3
### - i : un indice de ligne
### - j : un indice de colonne
###
### Gagnant.Puissance4.plot est identique a la fonction Gagnant.Puissance4 sauf qu'elle renvoie les coordonnees de la serie de 4 pions gagnante
###
Gagnant.Puissance4.plot <- function(M,i,j){
  ## Horizontal
  res <- FALSE
  if (j<=4){
    tmp <- all(M[i,1:4]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=rep(i,times=4),j=1:4))}
  }
  if (j>=2 & j <= 5){
    tmp <- all(M[i,2:5]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=rep(i,times=4),j=2:5))}
  }
  if (j>=3 & j <= 6){
    tmp <- all(M[i,3:6]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=rep(i,times=4),j=3:6))}
  }
  if (j>=4 & j <= 7){
    tmp <- all(M[i,4:7]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=rep(i,times=4),j=4:7))}
  }
  ## Vertical
  if (i<=4){
    tmp <- all(M[1:4,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=1:4,j=rep(j,times=4)))}
  }
  if (i>=2 & i <= 5){
    tmp <- all(M[2:5,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=2:5,j=rep(j,times=4)))}
  }
  if (i>=3 & i <= 6){
    tmp <- all(M[3:6,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=3:6,j=rep(j,times=4)))}
  }
  ## Diagonal Droite
  if ( (i+3 <= 6) & (j+3 <= 7)){
    tmp <- all(c(M[i,j],M[i+1,j+1],M[i+2,j+2],M[i+3,j+3])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=i:(i+3),j=j:(j+3)))}
  }
  if ( (i+2 <= 6) & (j+2 <= 7) & (i-1 >=1) & (j-1 >= 1)){
    tmp <- all(c(M[i-1,j-1],M[i,j],M[i+1,j+1],M[i+2,j+2])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-1):(i+2),j=(j-1):(j+2)))}
  }
  if ( (i+1 <= 6) & (j+1 <= 7) & (i-2 >=1) & (j-2 >= 1)){
    tmp <- all(c(M[i-2,j-2],M[i-1,j-1],M[i,j],M[i+1,j+1])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-2):(i+1),j=(j-2):(j+1)))}
  }
  if ( (i-3 >=1) & (j-3 >= 1)){
    tmp <- all(c(M[i-3,j-3],M[i-2,j-2],M[i-1,j-1],M[i,j])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-3):i,j=(j-3):j))}
  }
  ## Diagonal Gauche
  if ( (i+3 <= 6) & (j-3 >= 1)){
    tmp <- all(c(M[i,j],M[i+1,j-1],M[i+2,j-2],M[i+3,j-3])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=i:(i+3),j=j:(j-3)))}
  }
  if ( (i+2 <= 6) & (j-2 >= 1) & (i-1 >=1) & (j+1 <=7)){
    tmp <- all(c(M[i-1,j+1],M[i,j],M[i+1,j-1],M[i+2,j-2])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-1):(i+2),j=(j+1):(j-2)))}
  }
  if ( (i+1 <= 6) & (j-1 >= 1) & (i-2 >=1) & (j+2 <=7)){
    tmp <- all(c(M[i-2,j+2],M[i-1,j+1],M[i,j],M[i+1,j-1])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-2):(i+1),j=(j+2):(j-3)))}
  }
  if ( (i-3 >=1) & (j+3 <=7)){
    tmp <- all(c(M[i-3,j+3],M[i-2,j+2],M[i-1,j+1],M[i,j])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-3):i,j=(j+3):j))}
  }
  return(list(i=NA,j=NA))
}

########
## FONCTIONS IA ##
########


Evaluation.puissance4 <- function(symbol, M, i, j, state=TRUE, call="Max"){
  if (state){
    if (call == "Min" ){
    tmp<- -1000
    }
    else{ 
      tmp <- 1000}
  }
  else {
      tmp <- 0
    }
  return(tmp)
}
 


Min.Puissance4 <- function(M,symbol="+",Profondeur){
  ### new.symbol correspond au symbol du coup d'après (c'est à dire le symbole de l'adversaire). Il va etre appele dans la fonction Min
  if (symbol=="-"){
    new.symbol <- "+"
  } else {
    new.symbol <- "-"
  }
  
  if (Profondeur==0 | EndOfGame.Puissance4(M) ){
    End <- FALSE
    M.tmp <- M
    eval.M <- rep(NA,times=7)
    map.i <- rep(NA,times=7)
    for (j in 1:7){
      M.tmp <- M
      tmp.vide <- which(is.na(M[,j]))
      if (length(tmp.vide) > 0){
        i <- min(tmp.vide)
        map.i[j] <- i
        M.tmp[i,j] <- symbol
        if (Gagnant.Puissance4(M.tmp,i,j)){eval.M[j] <- Evaluation.puissance4(symbol,M.tmp,i,j,state=TRUE, call = "Min") } 
        else {eval.M[j] <-  Evaluation.puissance4(symbol,M.tmp,i,j,state=FALSE, call = "Min")}
      }
    }
  }
  
  if (Profondeur > 0){
    End <- FALSE
    M.tmp <- M
    eval.M <- rep(NA,times=7)
    map.i <- rep(NA,times=7)
    for (j in 1:7){
      M.tmp <- M
      tmp.vide <- which(is.na(M[,j]))
      if (length(tmp.vide) > 0){
        i <- min(tmp.vide)
        map.i[j] <- i
        M.tmp[i,j] <- symbol
        if (Gagnant.Puissance4(M.tmp,i,j)){
          eval.M[j] <- Evaluation.puissance4(symbol,M.tmp,i,j,state=TRUE, call = "Min")
        } 
        else {
          eval.M[j] <- Max.Puissance4(M.tmp,symbol=new.symbol,Profondeur = Profondeur - 1)
        }
      }
    }
  }
  
  wmin <- which(eval.M == min(eval.M,na.rm=TRUE))
  ind.min <- sample(1:length(wmin),1)
  min.j <- wmin[ind.min]
  eva.tmp <- eval.M[min.j]

  return(eva.tmp)
}
  

Max.Puissance4 <- function(M,symbol="+",Profondeur){
  ### new.symbol correspond au symbol du coup d'après (c'est à dire le symbole de l'adversaire). Il va etre appele dans la fonction Min
  if (symbol=="-"){
    new.symbol <- "+"
  } else {
    new.symbol <- "-"
  }
  
  if (Profondeur==0 | EndOfGame.Puissance4(M) ){
    End <- FALSE
    M.tmp <- M
    eval.M <- rep(NA,times=7)
    map.i <- rep(NA,times=7)
    for (j in 1:7){
      M.tmp <- M
      tmp.vide <- which(is.na(M[,j]))
      if (length(tmp.vide) > 0){
        i <- min(tmp.vide)
        map.i[j] <- i
        M.tmp[i,j] <- symbol
        
        if (Gagnant.Puissance4(M.tmp,i,j)){eval.M[j] <- Evaluation.puissance4(symbol,M.tmp,i,j,state=TRUE, call ="Max") } 
        else {eval.M[j] <- Evaluation.puissance4(symbol,M.tmp,i,j,state=FALSE, call = "Max")}
      }
    }
  }
  
  if (Profondeur > 0){
    End <- FALSE
    M.tmp <- M
    eval.M <- rep(NA,times=7)
    map.i <- rep(NA,times=7)
    for (j in 1:7){
      M.tmp <- M
      tmp.vide <- which(is.na(M[,j]))
      if (length(tmp.vide) > 0){
        i <- min(tmp.vide)
        map.i[j] <- i
        M.tmp[i,j] <- symbol
        if (Gagnant.Puissance4(M.tmp,i,j)){
          eval.M[j] <- Evaluation.puissance4(symbol,M.tmp,i,j,state=TRUE, call ="Max")
        }
        else {
          eval.M[j] <- Min.Puissance4(M.tmp,symbol=new.symbol,Profondeur = Profondeur - 1)
        }
      }
    }
  }

  wmax <- which(eval.M == max(eval.M,na.rm=TRUE))
  ind.max <- sample(1:length(wmax),1)
  max.j <- wmax[ind.max]
  eva.tmp <- eval.M[max.j]

  return(eva.tmp)
}

Play.Oneshot.Puissance4 <- function (M, symbol = "-", Profondeur){
  if (symbol=="-"){
    new.symbol <- "+" 
  } else {
    new.symbol <- "-"
  }
  
  max.i <- NA ### variable stockant l'indice de la ligne dans laquelle le coup va etre joue
  max.j <- NA ### variable stockant l'indice de la colonne dans laquelle le coup va etre joue

  if (Profondeur==0 | EndOfGame.Puissance4(M) ){
    End <- FALSE
    M.tmp <- M
    eval.M <- rep(NA,times=7)
    map.i <- rep(NA,times=7)
    for (j in 1:7){
      M.tmp <- M
      tmp.vide <- which(is.na(M[,j]))
      if (length(tmp.vide) > 0){
        i <- min(tmp.vide)
        map.i[j] <- i
        M.tmp[i,j] <- symbol
        
        if (Gagnant.Puissance4(M.tmp,i,j)){eval.M[j] <- Evaluation.puissance4(symbol,M.tmp,i,j,state =TRUE, call = "Max") } 
        else {eval.M[j] <- Evaluation.puissance4(symbol,M.tmp,i,j,state=FALSE, call = "Min")}
      }
    }
    print(eval.M)
  }
  
  if (Profondeur > 0){
    End <- FALSE
    M.tmp <- M
    eval.M <- rep(NA,times=7)
    map.i <- rep(NA,times=7)
    for (j in 1:7){
      M.tmp <- M
      tmp.vide <- which(is.na(M[,j]))
      if (length(tmp.vide) > 0){
        i <- min(tmp.vide)
        map.i[j] <- i
        M.tmp[i,j] <- symbol
        if (Gagnant.Puissance4(M.tmp,i,j)){
          eval.M[j] <- Evaluation.puissance4(symbol,M.tmp,i,j,state=TRUE, call = "Max")
        } 
        else {
          eval.M[j] <- Min.Puissance4(M.tmp,symbol=new.symbol,Profondeur= Profondeur - 1)
          
        }
      }
    }
    print(eval.M)
  }
  
  wmax <- which(eval.M == max(eval.M,na.rm=TRUE))
  ind.max <- sample(1:length(wmax),1)
  max.j <- wmax[ind.max]
  max.i <- map.i[max.j]
  eva.tmp <- eval.M[max.j]
  print(eva.tmp)
  
  return(list(i = max.i, j= max.j))
}
  
  






### Play.Computers.Puissance4 prend 3 arguments
### - POSA : Une fonction qui correspond a la fonction Play.OneShot.Random en terme d'arguments d'entree et de sortie
### - POSB : Une fonction qui correspond a la fonction Play.OneShot.Random en terme d'arguments d'entree et de sortie
### - Profondeur : un entier indiquant la profondeur d'évaluation, c'est à dire le nombre de coup à l'avance anticipé pour l'évaluation
###
### La fonction Play.Computers.Puissance4 joue la partie en alternant la fonction POSA et POSB, c'est a dire en alternant les deux joueurs.
### La fonction Play.Computers.Puissance4 renvoie le vainqueur du jeu ou Match nul
###
Play.Computers.Puissance4 <- function(POS.A,POS.B,Profondeur=2){
  name.A <- ask(msg="Nom du joueur A")
  name.B <- ask(msg="Nom du joueur B")
  start <- sample(x=c(1,2),size=1)
  col1 <- "Jaune"
  col2 <- "Rouge"
  if (start==1){
    Player1 <- name.A
    POS1 <- POS.A
    Player2 <- name.B
    POS2 <- POS.B
  } else {
    Player1 <- name.B
    POS1 <- POS.B
    Player2 <- name.A
    POS2 <- POS.A
  }
  cat(Player1,":",col1, "## ",Player2,":",col2, "- \n")
  M <- matrix(NA,ncol=7,nrow=6)
  tour <- 1
  Affiche.Puissance4(M)
  End <- FALSE
  nb.coup <- 0
  while(!End & nb.coup <= 42){
    ask("Press Enter")
    nb.coup <- nb.coup+1
    #print(tour)
    if (tour%%2==0){
      print(paste("Au tour de",Player2,"##",col2))
      new.ind <- POS2(M,symbol="+",Profondeur=Profondeur)
      M[new.ind$i,new.ind$j] <- "+"
      G <- Gagnant.Puissance4.plot(M,new.ind$i,new.ind$j)
      if (!is.na(G$i[1])){
        Affiche.Puissance4(M)
        segments(x0=G$j[1]-0.5,y0=G$i[1]-0.5,x1=G$j[4]-0.5,y1=G$i[4]-0.5)
        print(paste("VAINQUEUR:",Player2))
        return(list(M=M,new.ind=new.ind))
      }
    } else {
      print(paste("Au tour de",Player1,"##",col1))
      new.ind <- POS1(M,symbol="-",Profondeur=Profondeur)
      M[new.ind$i,new.ind$j] <- "-"
      G <- Gagnant.Puissance4.plot(M,new.ind$i,new.ind$j)
      if (!is.na(G$i[1])){
        Affiche.Puissance4(M)
        segments(x0=G$j[1]-0.5,y0=G$i[1]-0.5,x1=G$j[4]-0.5,y1=G$i[4]-0.5)
        print(paste("VAINQUEUR:",Player1))
        return(list(M=M,new.ind=new.ind))
      }
    }
    #    print(M)
    #cat("La partie continue:",EndOfGame.Puissance4(M),"\n")
    cat("La partie continue:","\n")
    if (EndOfGame.Puissance4(M)){
      print("Pas Vainqueur")
      End <- TRUE
      return(list(M=M,new.ind=list(i=NA,j=NA)))
    }
    Affiche.Puissance4(M)
    tour <- tour+1
  }
}



M.final <- Play.Computers.Puissance4(Play.Oneshot.Puissance4,Play.Oneshot.Puissance4,Profondeur = 3)

TM.test <- matrix(NA,ncol=7,nrow=6)
TM.test[1,] <- c(NA,NA,NA,NA,NA,NA,NA)
Affiche.Puissance4(M.test)
Play.Oneshot.Puissance4(M.test,symbol="+",Profondeur = 3)
Play.OneShot.ME.simple(M.test,symbol="+")










