# setam un seed pentru a avea rezultate consistente la testare daca vrem
# set.seed(123456)

# functia pentru generarea variabilelor aleatoare si completarea partiala a matricei
frepcomgen <- function(n, m) {
  # verificam daca numarul de elemente cerute este mai mic decat limita 
  if (n > 201 || m > 201) {
    stop("Prea multe elemente cerute, am hardcodat sa fie 100 limita la o va, se poate schimba tho")
  }
  
  # generam probabilitati aleatorii pentru X și le normalizam pentru a avea suma 1
  pX <- runif(n)
  pX <- round(pX / sum(pX), 6)  
  
  # similar pentru Y
  pY <- runif(m)
  pY <- round(pY / sum(pY), 6)  
  
  # cream dataframe-uri pentru X și Y cu valori si probabilitati
  X <- data.frame(val = sample(-100:100, n, replace = FALSE), p = pX)
  Y <- data.frame(val = sample(-100:100, m, replace = FALSE), p = pY)
  
  # initializam matricea de repartitie comuna cu NA not allocated gen
  P <- matrix(NA, nrow = m + 1, ncol = n + 1)
  # setam probabilitatile marginale in matrice
  P[m+1, 1:n] <- pX
  P[1:m, n+1] <- pY
  
  # completam aleatoriu unele valori in matricea de repartitie comuna
  for (j in 1:m) {
    for (i in 1:n) {
      if (runif(1) < 0.5) {
        P[j, i] <- round(runif(1, min = 0, max = min(P[m+1, i], P[j, n+1])), 3)
      }
    }
  }
  # setam totalul probabilitatilor la 1
  P[m+1, n+1] <- 1
  
  # returnam o lista cu variabilele aleatorii X, Y și matricea P
  list(X = X, Y = Y, P = P)
}

# functia pentru completarea matricei P cu valorile lipsa
fcomplrepcom <- function(P, X, Y) {
  m <- nrow(P) - 1
  n <- ncol(P) - 1
  
  # calculam valorile lipsa in matrice
  for (j in 1:m) {
    for (i in 1:n) {
      if (is.na(P[j, i])) {
        # calculam deficitul pentru fiecare coloana și rand
        col_deficit <- P[j, n+1] - sum(P[j, 1:n], na.rm = TRUE)
        row_deficit <- P[m+1, i] - sum(P[1:m, i], na.rm = TRUE)
        val_missing <- min(col_deficit, row_deficit)
        P[j, i] <- max(val_missing, 0)
      }
    }
  }
  
  # ajustam sumele pentru fiecare coloana si rand
  P <- adjust_sums(P, m, n)
  
  # redenumim coloanele pentru variabilele aleatorii și returnarea rezultatelor
  names(X) <- c("x", "p")
  names(Y) <- c("y", "q")
  list(X = X, Y = Y, Pij = P)
}

# functia pentru ajustarea sumelor în matrice
adjust_sums <- function(P, m, n) {
  for (i in 1:n) {
    sum_col <- sum(P[1:m, i], na.rm = TRUE)
    if (sum_col != P[m+1, i]) {
      distribute_adjustment(P[1:m, i], P[m+1, i] - sum_col)
    }
  }
  for (j in 1:m) {
    sum_row <- sum(P[j, 1:n], na.rm = TRUE)
    if (sum_row != P[j, n+1]) {
      distribute_adjustment(P[j, 1:n], P[j, n+1] - sum_row)
    }
  }
  return(P)
}

# functia pentru distribuirea ajustarii în celulele necompletate ale matricei
distribute_adjustment <- function(cells, adjustment) {
  na_cells <- which(is.na(cells))
  if (length(na_cells) > 0 && adjustment != 0) {
    adjustment_per_cell <- adjustment / length(na_cells)
    cells[na_cells] <- adjustment_per_cell
  }
}

# teste
rezultat <- frepcomgen(3, 2)
tabel_completat <- fcomplrepcom(rezultat$P, rezultat$X, rezultat$Y)

# afișare rezultate teste
print(tabel_completat$Pij)
print(tabel_completat$X)
print(tabel_completat$Y)
print(rezultat)
print(tabel_completat)

#SUBPUNCTUL C)
frepmarginal <- function(tabel_va) {
  repCom <- tabel_va$Pij
  m <- nrow(repCom)-1 #(initializare pt cazul in care deja avem pe n+1 si m+1 repartitiile marginale)
  n <- ncol(repCom)-1
  
  #initializam pentru vectorii repartitiilor marginale
  rep_margin_X <- numeric(n) 
  rep_margin_Y <- numeric(m) 
  
  # calculam repartitia marginala pentru Y
  for(i in 1:m) {
    rep_margin_Y[i] <- sum(repCom[i,1:n ]) # suma de elem de pe coloanele corespunzatoare cele de a i-a linie
  }
  
  # repartitia marginala pt X
  for(j in 1:n) {
    rep_margin_X[j] <- sum(repCom[1:m, j]) # suma de pe linii pentru colana j
  }
  
  list(X = rep_margin_X, Y = rep_margin_Y)
}
repartitii_marginale <- frepmarginal(tabel_completat)
# Afișarea repartițiilor marginale
print(repartitii_marginale$X)
print(repartitii_marginale$Y)



#SUBPUNCTUL D)
#calculam media unei variabile aleatoare la patrat
fMedPatrat<- function(valori, probabilitati) {
  # Combinam valorile patrate si probabilitatile intr-un cadru de date
  data_combined <- data.frame(x_patrat = valori^2, prob = probabilitati)
  
  # Vector pentru rezultate
  rezultat <- numeric(length(unique(valori^2)))
  
  # iteram valorile patrate unive
  for (i in seq_along(rezultat)) {
    valoare_patrata <- unique(data_combined$x_patrat)[i]
    
    # adunam probabilitatile daca se repeta vreo valoare 
    rezultat[i] <- sum(data_combined$prob[data_combined$x_patrat == valoare_patrata])
  }
  
  # calculam suma valorilor patrate inmultite cu probabilitatile
  rezultat_final <- sum(unique(data_combined$x_patrat) * rezultat)
  
  return(rezultat_final)
}

fpropcov<-function(a,b,c,d,tabel_va){
  m <- nrow(tabel_va$Pij)-1
  n <- ncol(tabel_va$Pij)-1
  #pasul 1. calculam mediile celor doua variabile
  E_X<-sum(tabel_va$X$x*tabel_va$X$p)
  E_Y<-sum(tabel_va$Y$y*tabel_va$Y$q)
  
  #pasul 2. calculam mediile variabilelor la patrat
  E_X_patrat<-fMedPatrat(tabel_va$X$x,tabel_va$X$p)
  E_Y_patrat<-fMedPatrat(tabel_va$Y$y,tabel_va$Y$q)
  
  #pasul 3. calculam variantele
  Var_X<-E_X_patrat-(E_X*E_X)
  Var_Y<-E_Y_patrat-(E_Y*E_Y)
  
  #pasul 4. calculam media XY
  XY<-numeric(n*m)
  probXY<-numeric(n*m)
  for(i in 1:n){
    for(j in 1:m)
      XY<-c(tabel_va$X$x[i]*tabel_va$Y$y[j])
    probXY<-c(tabel_va$Y$q[j]*tabel_va$X$p[i])
  }
  covXY<-(sum(XY*probXY)-(E_X*E_Y))
  
  #pasul 5. Aplicam formula de la laborator
  return((a*c*Var_X)+(b*d*Var_Y)+(a*d+b*c)*covXY)
}

(fpropcov(5,9,-3,-2,tabel_completat))


#SUBPUNCTUL E)
# P(X|Y=yi) si P(Y|X=xi) daca probCond=1 conditionam pe X la Y, daca probCond=0 conditionam pe Y la X
fPcond <- function(tabel_va, probCond, xi = -Inf, yi = -Inf) {
  repCom <- tabel_va$Pij
  m <- nrow(repCom)
  n <- ncol(repCom)
  
  if (probCond == 1 && yi %in% tabel_va$Y$y) { #verificam sa existe yi in repartitie si cum vrem sa conditionam variabilele intre ele
    poz_yi <- match(yi, tabel_va$Y$y)         #aflam pozitia lui yi in repartitia comuna
    rep_X_conditionat_Y <- numeric(n-1)
    for (i in 1:(n-1)) {
      rep_X_conditionat_Y[i] <- repCom[poz_yi, i] / repCom[poz_yi, n] #pentru fiecare variabila de la linia corespunzatoare pozitiei lui yi facem impartirea cu probabilitatea lui x de la linia yi
    }
    return(rep_X_conditionat_Y)
  } else if (probCond == 0 && xi %in% tabel_va$X$x) { #facem acelasi lucru si pentru xi, numai ca aici pozitia coloanei ramane neschimbata si se schimba pozitia pozitiile liniilor
    poz_xi <- match(xi, tabel_va$X$x)
    rep_Y_conditionat_X <- numeric(m-1)
    for (i in 1:(m-1)) {
      rep_Y_conditionat_X[i] <- repCom[i, poz_xi] / repCom[m, poz_xi]
    }
    return(rep_Y_conditionat_X)
  }
}

(X_cond_Y<-fPcond(tabel_completat,1,yi=tabel_completat$Y$y[1]))
(Y_condX<-fPcond(tabel_completat,0,xi=tabel_completat$X$x[1]))


#SUBPUNCTUL F)
# cazul in care vrem P(X=xi,Y=yi)
fComun<-function(tabel_va,xi,yi){
  repCom <- tabel_va$Pij
  
  # verificam existenta valorilor in reprezentarea variabilelor
  if(yi %in% tabel_va$Y$y && xi %in% tabel_va$X$x){
    return(repCom[match(yi,tabel_va$Y$y),match(xi,tabel_va$X$x)]) #daca exista, afisam valoarea de la pozitia [yi,xi]
  }
  else{
    return("Valorile nu exista in repartitiile variabilelor")
  }
}

(rezultat<-fComun(tabel_completat,xi=tabel_completat$X$x[1],yi=tabel_completat$Y$y[1]))
(rezultat<-fComun(tabel_completat,xi=tabel_completat$X$x[2],yi=tabel_completat$Y$y[2]))

#SUBPUNCTUL H)
#tabelul contine si repartitiile marginale ale lui X si Y
fverind<-function(repCom){
  m <- nrow(repCom)
  n <- ncol(repCom)
  for(i in 1:(m-1)){
    for(j in 1:(n-1)){
      if(repCom[i,j]!=(repCom[i,n]*repCom[m,j]))
      {return("X si Y sunt dependente!")}
    }
  }
  return("X si Y sunt independente!")
}
(rezultat<-fverind(tabel_completat$Pij))

fvernecor<-function(tabel_va){
  #intai trebuie sa calculam coeficientul de corelatie al celor doua variabile
  #pentru asta calculam media, varianta si covarianta a celor 2 variabile
  repCom<-tabel_va$Pij
  
  m <- nrow(repCom)-1
  n <- ncol(repCom)-1
  #pasul 1. calculam mediile celor doua variabile
  E_X<-sum(tabel_va$X$x*tabel_va$X$p)
  E_Y<-sum(tabel_va$Y$y*tabel_va$Y$q)
  
  #pasul 2. calculam mediile variabilelor la patrat
  E_X_patrat<-fMedPatrat(tabel_va$X$x,tabel_va$X$p)
  E_Y_patrat<-fMedPatrat(tabel_va$Y$y,tabel_va$Y$q)
  
  #pasul 3. calculam variantele
  Var_X<-E_X_patrat-(E_X*E_X)
  Var_Y<-E_Y_patrat-(E_Y*E_Y)
  
  #pasul 4. calculam media XY
  XY<-numeric(n*m)
  probXY<-numeric(n*m)
  for(i in 1:n){
    for(j in 1:m)
      XY<-c(tabel_va$X$x[i]*tabel_va$Y$y[j])
    probXY<-c(tabel_va$Y$q[j]*tabel_va$X$p[i])
  }
  EXY<-sum(XY*probXY)
  
  #pasul 5. Calculam formula coeficientului de corelatie
  coef_cor<-(EXY-(E_X*E_Y))/sqrt(Var_X*Var_Y)
  if(coef_cor==0)
    return("Variabile aleatoare necorelate")
  else return("Variabile aleatoare corelate")
}

(fvernecor(tabel_completat))

