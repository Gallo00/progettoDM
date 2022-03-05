
#funzione che prende in input un oggetto fclust e stampa su console
#le informazioni principali riguardo ad esso

info.principali.fclust <- function(fclust.obj){
  if(class(fclust.obj) != "fclust")
  {
    stop("Non è un oggetto fclust")
  }
  
  str <- paste("Tipo fclust: ",toString(fclust.obj$call[1])," \n")
  cat(str)
  
  str <- paste("Numero oggetti: ",toString(nrow(fclust.obj$U)),"\n")
  cat(str)
  
  str <- paste("Numero cluster: ",toString(fclust.obj$k),"\n")
  cat(str)
  #il contenuto dell'if è eseguito solo se si tratta di un oggetto
  #con estensione ai noise points
  if((grep("noise",toString(fclust.obj$call[1]),fixed = TRUE)) == 1)
  {
    cat("L'oggetto ammette la presenza di noise points, vale a dire che c'è un ulteriore cluster, esso contiene i noise points\n\n")
  }
  cat("Centroidi:\n")
  print(fclust.obj[["H"]])
  
  str <- paste("Indice di fuzzy Silhouette: ",
               toString(Fclust.index(fclust.obj,index="SIL.F",alpha = 1)),"\n")
  cat(str)
}