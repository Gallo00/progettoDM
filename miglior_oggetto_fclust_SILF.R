#Creiamo una funzione che esegue vari algoritmi di clustering,
#confronta i relativi indici di silhouette e restituisce come risultato
#l'oggetto fclust(� il tipo ritornato da ogni funzione) con l'indice pi� alto
#Prende in input anche il risultato del clustering con la funzione di base FKM

#la funzione � molto banale, � una serie di if per verificare 
#quale algoritmo � qualitativamente(si vede con il coeff di Silhouette)
#il migliore
miglior.oggetto.fclust.SILF <- function(df,res.fkm,k){
  input_ok <- 0
  if(class(res.fkm) == "fclust")
  {
    if(toString(res.fkm$call[1]) == "FKM")
    {
      input_ok <- 1
    }
  }
  
  if(input_ok == 0)
  {
    stop("L'oggetto in input non � un oggetto fclust o non � FKM\n")
  }
  if(missing(k))
  {
    k=2:6
  }
  
  eps <- 0.05 #se la differenza tra FKM e FKM.noise ad esempio � inferiore
  # a questo numero valuto come migliore FKM.noise
  
  #prendo l'istante corrente per poi successivamente capire quale �
  #stato il tempo di esecuzione della funzione
  t1 <- Sys.time()
  
  #disattivo i warnings che sono in genere relativi al fatto
  #che non si � arrivati a convergenza per i limiti imposti alla funzione
  #che potrebbero anche essere modificati come RS, conv e maxit
  options(warn = - 1)
  
  fclust.output <- res.fkm
  fclust.output.SilF <- Fclust.index(fclust.output,index="SIL.F",alpha = 1)
  rm(res.fkm)
  cat("Eseguito 10%\n")
  
  gc()
  #calcolo clustering con regolarizzazione dell'entropia
  #l'implementazione di questa funzione potrebbe generare dei NaN
  #nella matrice U se i dati non sono standardizzati
  #Prima eseguiamo il clustering con standardizzazione
  #Proviamo poi senza, in questo modo capiamo per i successivi algoritmi
  #che usano regolarizzazione dell'entropia se usare o non usare standardizzazione
  res.fkm.ent <- FKM.ent(df,k=k,stand=1,index="SIL.F",alpha=1)
  
  usa_stand <- 0
  tryCatch( {res.fkm.ent <- FKM.ent(df,k=k,index="SIL.F",alpha=1) }
            , error = function(e) 
              {
              cat("Metodi ent e pf.noise eseguiti con stand=1\n")
              usa_stand <<- 1 #operatore necessario per assegnamento variabile
              #in questo caso
              })
  #Avvisiamo l'utente che i metodi ent sono eseguiti con standardizzazione
  
  
  res.fkm.ent.SilF <- Fclust.index(res.fkm.ent,index="SIL.F",alpha = 1)
  
  if(res.fkm.ent.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.ent
    fclust.output.SilF <- res.fkm.ent.SilF
  }
  rm(res.fkm.ent)
  rm(res.fkm.ent.SilF)
  cat("Eseguito 20%\n")

  
  gc()
  #calcolo clustering con estensione noise points
  res.fkm.noise <- FKM.noise(df,k=k,index="SIL.F",alpha=1)
  res.fkm.noise.SilF <- Fclust.index(res.fkm.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.noise.SilF > fclust.output.SilF - eps){
    fclust.output <- res.fkm.noise
    fclust.output.SilF <- res.fkm.noise.SilF
  }
  rm(res.fkm.noise)
  rm(res.fkm.noise.SilF)
  cat("Eseguito 30%\n")
  
  
  gc()
  #calcolo clustering con estensione noise points e regolarizzazione entropia
  #Per l'implementazione di questa funzione i dati potrebbeero
  #dover essere standardizzati,
  res.fkm.ent.noise <- FKM.ent.noise(df,k=k,stand=usa_stand,index="SIL.F",alpha=1)
  res.fkm.ent.noise.SilF <- Fclust.index(res.fkm.ent.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.ent.noise.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.ent.noise
    fclust.output.SilF <- res.fkm.ent.noise.SilF
  }
  rm(res.fkm.ent.noise)
  rm(res.fkm.ent.noise.SilF)
  cat("Eseguito 40%\n")
  
  
  gc()
  #calcolo clustering con estensione Gustafson Kessel
  res.fkm.gk <- FKM.gk(df,k=k,index="SIL.F",alpha=1)
  res.fkm.gk.SilF <- Fclust.index(res.fkm.gk,index="SIL.F",alpha = 1)
  
  
  if(res.fkm.gk.SilF > fclust.output.SilF - eps ){
    fclust.output <- res.fkm.gk
    fclust.output.SilF <- res.fkm.gk.SilF
  }
  rm(res.fkm.gk)
  rm(res.fkm.gk.SilF)
  cat("Eseguito 50%\n")
  
  
  gc()
  #calcolo clustering con estensione Gustafson Kessel e regolarizzazione
  #entropia
  #Per l'implementazione di questa funzione i dati potrebbeero
  #dover essere standardizzati,
  res.fkm.gk.ent <- FKM.gk.ent(df,k=k,stand=usa_stand,index="SIL.F",alpha=1)
  res.fkm.gk.ent.SilF <- Fclust.index(res.fkm.gk.ent,index="SIL.F",alpha = 1)
  
  if(res.fkm.gk.ent.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gk.ent
    fclust.output.SilF <- res.fkm.gk.ent.SilF
  }
  
  rm(res.fkm.gk.ent)
  rm(res.fkm.gk.ent.SilF)
  cat("Eseguito 60%\n")
  
  
  gc()
  #calcolo clustering con estensione Gustafson Kessel e estensione noise points
  res.fkm.gk.noise <- FKM.gk.noise(df,k=k,index="SIL.F",alpha=1)
  res.fkm.gk.noise.SilF <- Fclust.index(res.fkm.gk.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.gk.noise.SilF > fclust.output.SilF - eps){
    fclust.output <- res.fkm.gk.noise
    fclust.output.SilF <- res.fkm.gk.noise.SilF
  }
  rm(res.fkm.gk.noise)
  rm(res.fkm.gk.noise.SilF)
  cat("Eseguito 70%\n")
  
  
  gc()
  #calcolo clustering con estensione Gustafson Kessel e regolarizzazione
  #entropia e estensione noise points
  #Per l'implementazione di questa funzione i dati potrebbeero
  #dover essere standardizzati,
  res.fkm.gk.ent.noise <- FKM.gk.ent.noise(df,k=k,stand=1,index="SIL.F",alpha=1)
  res.fkm.gk.ent.noise.SilF <- Fclust.index(res.fkm.gk.ent.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.gk.ent.noise.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gk.ent.noise
    fclust.output.SilF <- res.fkm.gk.ent.noise.SilF
  }
  rm(res.fkm.gk.ent.noise)
  rm(res.fkm.gk.ent.noise.SilF)
  cat("Eseguito 80%\n")

  
  gc() 
  #calcolo clustering con fuzzifier polinomiale
  res.fkm.pf <- FKM.pf(df,k=k,index="SIL.F",alpha=1)
  res.fkm.pf.SilF <- Fclust.index(res.fkm.pf,index="SIL.F",alpha = 1)
  
  if(res.fkm.pf.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.pf
    fclust.output.SilF <- res.fkm.pf.SilF
  }
  rm(res.fkm.pf)
  rm(res.fkm.pf.SilF)
  cat("Eseguito 90%\n")
  
  
  gc()
  #calcolo clustering con fuzzifier polinomiale e estensione noise point
  #Per l'implementazione di questa funzione i dati potrebbeero
  #dover essere standardizzati,
  #mettiamo stand = 1 nel caso, 
  # non da errore ma ogni punto risulterebbe un noise point
  res.fkm.pf.noise <- FKM.pf.noise(df,k=k,stand=usa_stand,index="SIL.F",alpha=1)
  res.fkm.pf.noise.SilF <- Fclust.index(res.fkm.pf.noise,index="SIL.F",alpha = 1)
  
  x <- fclust.output.SilF
  if((grepl("pf",toString(fclust.output$call[1]),fixed = TRUE))==1)
  {
    x <- fclust.output.SilF - eps
    #capiamo se l'oggetto attualmente migliore � di tipo pf
  }
  
  
  if(res.fkm.pf.noise.SilF > x){
    fclust.output <- res.fkm.pf.noise
    fclust.output.SilF <- res.fkm.pf.noise.SilF
  }
  rm(res.fkm.pf.noise)
  rm(res.fkm.pf.noise.SilF)
  cat("Eseguito 100%\n")
  
  #eseguiti tutti gli algoritmi di clustering, scriviamo sulla console
  # chi � il stato il migliore
  
  str <- paste("Il miglior algoritmo su questo dataset � stato ",toString(fclust.output$call[1]),
               "\nIl coefficiente di fuzzy Silhouette � ",toString(fclust.output.SilF))
  cat(str)
  
  #calcolo tempo per eseguire la funzione
  t2 <- Sys.time()
  secs <- round(as.numeric(t2) - as.numeric(t1))
  mins <- secs %/% 60 #divisione intera
  secs <- secs %% 60 #resto della divisione intera
  str <- paste("\nLa funzione � terminata in ",
               toString(mins)," minuti e ", toString(secs)," secondi\n")
  cat(str)
  
  #riattiviamo i warnings
  options(warn = 0)
  return(fclust.output)
}
