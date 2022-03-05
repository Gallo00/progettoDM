### validazione oggetto fclust

#passando print = FALSE si disattivano le stampe(ad eccezione del coeff di fuzzy SIL.)
validazione.fclust.SILF <- function(fclust.obj,print = TRUE)
{
  if(class(fclust.obj) != "fclust")
  {
    stop("Oggetto in input NON fclust")
  }
  
  if(print)
  {
    cat("L'indice di fuzzy Silhouette è un valore tra -1 e 1\n")
    cat("Più è vicino ad 1 e meglio è, un valore uguale a 0 significa che l'oggetto\n")
    cat("è a metà trai cluster, cerchiamo valori maggiori di 0\n") 
    
    cat("In genere un valore maggiore o uguale a 0.5 è considerato abbastanza buono\n\n")
  }
  
  SILF <- Fclust.index(fclust.obj,index="SIL.F",alpha = 1)
  
  if(print)
  {
    cat("La funzione torna uno dei seguenti valori: 0,1,2,3,4,5\n")
    cat("dove 0 indica una pessima suddivisione dei punti e 5 una suddivisione perfetta\n")
  } 
  
  cat("SILF = ",SILF,"\n")
  if(SILF <= 0)
  {
    if(print)
    {
      cat("L'indice di fuzzy Silhouette ha valore minore o uguale a 0\n")
      cat("Non vi è stata una buona suddivisione dei punti\n") 
    }
    
    return(0)
  }
  else if(SILF < 0.5)
  {
    if(print)
    {
      cat("L'indice di fuzzy Silhouette ha valore compreso tra 0 e 0.5\n") 
    }
    
    return(1)
  }
  else if(SILF < 0.75)
  {
    if(print)
    {
      cat("L'indice di fuzzy Silhouette ha valore maggiore o uguale a 0.5\n")
      cat("La suddivisione dei punti è stata abbastanza buona\n") 
    }
    
    return(2)
  }
  else if(SILF < 0.90)
  {
    if(print)
    {
      cat("L'indice di fuzzy Silhouette ha valore maggiore o uguale a 0.75\n")
      cat("La suddivisione dei punti è stata molto buona\n") 
    }
    
    return(3)
  }
  else if(SILF < 1)
  {
    if(print)
    {
      cat("L'indice di fuzzy Silhouette ha valore molto vicino ad 1\n")
      cat("La suddivisione dei punti è stata praticamente perfetta\n") 
    }
    
    return(4)
  }
  else
  {
    #siamo nel caso SILF = 1, con dataset abbastanza grandi questo caso
    #è quasi impossibile
    if(print)
    {
      cat("L'indice di fuzzy Silhouette ha valore uguale ad 1\n")
      cat("La suddivisione dei punti è stata perfetta\n") 
    }
    
    return(5)
  }
}