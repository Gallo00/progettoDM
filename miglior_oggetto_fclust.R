#Creiamo una funzione che esegue vari algoritmi di clustering,
#confronta i relativi indici di silhouette e restituisce come risultato
#l'oggetto fclust(è il tipo ritornato da ogni funzione) con l'indice più alto
#Prende in input anche il risultato del clustering con la funzione di base FKM

#la funzione è molto banale, è una serie di if per verificare 
#quale algoritmo è qualitativamente(si vede con il coeff di Silhouette)
#il migliore
miglior.oggetto.fclust <- function(df_norm,res.fkm){
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
    cat("L'oggetto in input non è un oggetto fclust o non è FKM\n")
    stop()
  }
  
  #prendo l'istante corrente per poi successivamente capire quale è
  #stato il tempo di esecuzione della funzione
  t1 <- Sys.time()
  
  #disattivo i warnings che sono in genere relativi al fatto
  #che non si è arrivati a convergenza per i limiti imposti alla funzione
  #che potrebbero anche essere modificati come RS, conv e maxit
  options(warn = - 1)
  
  fclust.output <- res.fkm
  fclust.output.SilF <- Fclust.index(fclust.output,index="SIL.F",alpha = 1)
  rm(res.fkm)
  cat("Eseguito 7%\n")
  
  #calcolo clustering con regolarizzazione dell'entropia
  res.fkm.ent <- FKM.ent(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.ent.SilF <- Fclust.index(res.fkm.ent,index="SIL.F",alpha = 1)
  
  if(res.fkm.ent.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.ent
    fclust.output.SilF <- res.fkm.ent.SilF
  }
  rm(res.fkm.ent)
  rm(res.fkm.ent.SilF)
  cat("Eseguito 14%\n")

  
   
  #calcolo clustering con estensione noise points
  res.fkm.noise <- FKM.noise(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.noise.SilF <- Fclust.index(res.fkm.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.noise.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.noise
    fclust.output.SilF <- res.fkm.noise.SilF
  }
  rm(res.fkm.noise)
  rm(res.fkm.noise.SilF)
  cat("Eseguito 21%\n")
  
  
   
  #calcolo clustering con estensione noise points e regolarizzazione entropia
  res.fkm.ent.noise <- FKM.ent.noise(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.ent.noise.SilF <- Fclust.index(res.fkm.ent.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.ent.noise.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.ent.noise
    fclust.output.SilF <- res.fkm.ent.noise.SilF
  }
  rm(res.fkm.ent.noise)
  rm(res.fkm.ent.noise.SilF)
  cat("Eseguito 28%\n")
  
  
   
  #calcolo clustering con estensione Gustafson Kessel
  res.fkm.gk <- FKM.gk(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.gk.SilF <- Fclust.index(res.fkm.gk,index="SIL.F",alpha = 1)
  
  if(res.fkm.gk.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gk
    fclust.output.SilF <- res.fkm.gk.SilF
  }
  rm(res.fkm.gk)
  rm(res.fkm.gk.SilF)
  cat("Eseguito 35%\n")
  
  
   
  #calcolo clustering con estensione Gustafson Kessel e regolarizzazione
  #entropia
  res.fkm.gk.ent <- FKM.gk.ent(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.gk.ent.SilF <- Fclust.index(res.fkm.gk.ent,index="SIL.F",alpha = 1)
  
  if(res.fkm.gk.ent.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gk.ent
    fclust.output.SilF <- res.fkm.gk.ent.SilF
  }
  rm(res.fkm.gk.ent)
  rm(res.fkm.gk.ent.SilF)
  cat("Eseguito 42%\n")
  
  
   
  #calcolo clustering con estensione Gustafson Kessel e estensione noise points
  res.fkm.gk.noise <- FKM.gk.noise(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.gk.noise.SilF <- Fclust.index(res.fkm.gk.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.gk.noise.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gk.noise
    fclust.output.SilF <- res.fkm.gk.noise.SilF
  }
  rm(res.fkm.gk.noise)
  rm(res.fkm.gk.noise.SilF)
  cat("Eseguito 49%\n")
  
  
   
  #calcolo clustering con estensione Gustafson Kessel e regolarizzazione
  #entropia e estensione noise points
  res.fkm.gk.ent.noise <- FKM.gk.ent.noise(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.gk.ent.noise.SilF <- Fclust.index(res.fkm.gk.ent.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.gk.ent.noise.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gk.ent.noise
    fclust.output.SilF <- res.fkm.gk.ent.noise.SilF
  }
  rm(res.fkm.gk.ent.noise)
  rm(res.fkm.gk.ent.noise.SilF)
  cat("Eseguito 56%\n")
  
   
  #calcolo clustering con estensione Gustafson Kessel e Babuska
  res.fkm.gkb <- FKM.gkb(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.gkb.SilF <- Fclust.index(res.fkm.gkb,index="SIL.F",alpha = 1)
  
  if(res.fkm.gkb.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gkb
    fclust.output.SilF <- res.fkm.gkb.SilF
  }
  rm(res.fkm.gkb)
  rm(res.fkm.gkb.SilF)
  cat("Eseguito 63%\n")
  
   
  #calcolo clustering con estensione Gustafson Kessel e Babuska e regolarizzazione
  #entropia
  res.fkm.gkb.ent <- FKM.gkb.ent(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.gkb.ent.SilF <- Fclust.index(res.fkm.gkb.ent,index="SIL.F",alpha = 1)
  
  if(res.fkm.gkb.ent.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gkb.ent
    fclust.output.SilF <- res.fkm.gkb.ent.SilF
  }
  rm(res.fkm.gkb.ent)
  rm(res.fkm.gkb.ent.SilF)
  cat("Eseguito 70%\n")
  
  
   
  #calcolo clustering con estensione Gustafson Kessel e Babuska e estensione noise points
  res.fkm.gkb.noise <- FKM.gkb.noise(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.gkb.noise.SilF <- Fclust.index(res.fkm.gkb.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.gkb.noise.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gkb.noise
    fclust.output.SilF <- res.fkm.gkb.noise.SilF
  }
  rm(res.fkm.gkb.noise)
  rm(res.fkm.gkb.noise.SilF)
  cat("Eseguito 77%\n")
  
  
   
  #calcolo clustering con estensione Gustafson Kessel e Babuska e regolarizzazione
  #entropia e estensione noise points
  res.fkm.gkb.ent.noise <- FKM.gkb.ent.noise(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.gkb.ent.noise.SilF <- Fclust.index(res.fkm.gkb.ent.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.gkb.ent.noise.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.gkb.ent.noise
    fclust.output.SilF <- res.fkm.gkb.ent.noise.SilF
  }
  rm(res.fkm.gkb.ent.noise)
  rm(res.fkm.gkb.ent.noise.SilF)
  cat("Eseguito 84%\n")
  
  
   
  #calcolo clustering con fuzzifier polinomiale
  res.fkm.pf <- FKM.pf(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.pf.SilF <- Fclust.index(res.fkm.pf,index="SIL.F",alpha = 1)
  
  if(res.fkm.pf.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.pf
    fclust.output.SilF <- res.fkm.pf.SilF
  }
  rm(res.fkm.pf)
  rm(res.fkm.pf.SilF)
  cat("Eseguito 91%\n")
  
  #calcolo clustering con fuzzifier polinomiale e estensione noise point
  res.fkm.pf.noise <- FKM.pf.noise(df_norm,k=2:6,index="SIL.F",alpha=1)
  res.fkm.pf.noise.SilF <- Fclust.index(res.fkm.pf.noise,index="SIL.F",alpha = 1)
  
  if(res.fkm.pf.noise.SilF > fclust.output.SilF){
    fclust.output <- res.fkm.pf.noise
    fclust.output.SilF <- res.fkm.pf.noise.SilF
  }
  rm(res.fkm.pf.noise)
  rm(res.fkm.pf.noise.SilF)
  cat("Eseguito 100%\n")
  
  #eseguiti tutti gli algoritmi di clustering, scriviamo sulla console
  # chi è il stato il migliore
  
  str <- paste("Il miglior algoritmo su questo dataset è stato ",toString(fclust.output$call[1]),
               "\nIl coefficiente di fuzzy Silhouette è ",toString(fclust.output.SilF))
  cat(str)
  
  #calcolo tempo per eseguire la funzione
  t2 <- Sys.time()
  secs <- round(as.numeric(t2) - as.numeric(t1))
  mins <- secs %/% 60 #divisione intera
  secs <- secs %% 60 #resto della divisione intera
  str <- paste("\nLa funzione è terminata in ",
               toString(mins)," minuti e ", toString(secs)," secondi\n")
  cat(str)
  
  #riattiviamo i warnings
  options(warn = 0)
  return(fclust.output)
}
