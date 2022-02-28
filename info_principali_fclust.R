
#funzione che prende in input un oggetto fclust e stampa su console
#le informazioni principali riguardo ad esso

info.principali.fclust <- function(fclust.obj){
  if(class(fclust.obj) != "fclust")
  {
    print("Non è un oggetto fclust")
    stop()
  }
  
  str <- paste("Tipo fclust: ",toString(fclust.obj$call[1])," \n")
  cat(str)
  
  str <- paste("Numero oggetti: ",toString(nrow(fclust.obj$U))," \n")
  cat(str)
  
  str <- paste("Numero cluster: ",toString(fclust.obj$k)," \n")
  cat(str)
  
  str <- paste("Indice di fuzzy Silhouette: ",
               toString(Fclust.index(fclust.obj,index="SIL.F",alpha = 1))," \n")
  cat(str)
}