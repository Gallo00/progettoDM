gc()

#Carichiamo le librerie necessarie

library(fclust)
library(mlbench)
library(ggplot2)
library(lattice)
library(caret)
library(factoextra)
library(cluster)
#Carichiamo il dataset, conservato in csv, lo "convertiamo" in dataframe

dati_csv <- read.csv("wine.csv")
#dati_csv <- read.csv("wine.csv")
df <- data.frame(dati_csv)
rm(dati_csv)
print(df)


#Se nel dataframe è presente una colonna(o più) che contiene degli id, ovviamente 
# la scartiamo
rimuovi_colonne <- function(df)
{

  #rimuovere tutte le colonne non numeric
  nums <- unlist(lapply(df, is.numeric))
  df <- df[ , nums]
  cat("Ho rimosso le colonne NON numeric\n")
  if(ncol(df) < 20) #ovviamente se le colonne sono troppe non ha senso eseguire questo if
  {
    cat("Rimangono le seguenti colonne:\n")
    print(colnames(df))
    #tramite input da tastiera chiedere all'utente quali altre colonne vuole rimuovere
    while((col <- readline(prompt="Inserisci nome colonna da eliminare(stop_remove per fermarsi): "))!="stop_remove")
    {
      df <- df[, !(names(df) %in% col)]
    }
  }
  return(df)
}

df <- rimuovi_colonne(df)
rm(rimuovi_colonne)
#il dataset potrebbe avere valori mancanti 
#rappresentati con NA, scartiamo per semplicità le righe
#contenenti valori NA
df <- na.omit(df)

#usiamo un numero inferiore di tuple per semplicità se il dataset è grande
riduzione_df <- function(df)
{
  perc <- readline(prompt="Inserisci un numero da 0 a 1: ")
  perc <- as.double(perc)
  if(perc < 0.0 | perc > 1.0)
  {
    print("Numero non compreso tra 0 e 1, la percentuale è stata impostata a 1")
    return(df)
  }
  #decidiamo la percentuale di righe che vogliamo usare
  num_tuple <- round(perc*nrow(df))
  set.seed(111222333)
  pos_rand <- sample(1:nrow(df),num_tuple)
  
  return(df[pos_rand,])
}

df <- riduzione_df(df)
rm(riduzione_df)


#features selection
#Per semplificare il modello scartiamo le features che non sono importanti
#ovvero non aiutano nella distinzione degli oggetti

#Potremmo scartare features ridondanti
#Sfrutteremo il metodo findCorrelation per trovare le variabili
#con forti gradi di correlazione(in genere con forti si intende con
#grado maggiore o uguale 0.75)
#https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
corr <- cor(df[,1:ncol(df)])
print(corr)
variabili.molto.correlate <- findCorrelation(corr,cutoff = 0.75)
if(length(variabili.molto.correlate) > 0 )
{
  str <- paste("Le seguenti variabili sono molto correlate alle altre, dunque le scartiamo \n",toString(variabili.molto.correlate))
  cat(str)
  
  #con questo semplice for troviamo i nomi delle colonne da scartare
  var <- c()
  for(i in 1:length(variabili.molto.correlate)) {
    var <- c(var,colnames(df)[variabili.molto.correlate[i]])
  }
  
  
  #questa operazione permette di rimuovere da df le colonne
  #con i nomi trovati nel for
  df <- df[, !(names(df) %in% var)]
  
  rm(str)
  rm(var)
  
} else{
  cat("Nessuna variabile scartata")
}


rm(i)
rm(variabili.molto.correlate)
rm(corr)


gc()
res.fkm <- FKM(df,k=3,index="SIL.F",alpha=1)
#stampiamo il coeff di fuzzy Silhouette
Fclust.index(res.fkm,index="SIL.F",alpha = 1)
#Inseriamo k=2:6,index="SIL.F",alpha=1 anche se non è necessario
#per evitare la stampa a schermo della funzione

#di default viene provato come numero di cluster da 2 a 6, si tiene poi
# il migliore

#Grazie alla funzione summary visualizziamo tutte le informazioni dell'oggetto
#tornato in output dalla funzione FKM
sum.res.fkm <- summary(res.fkm)

#una funzione abbastanza simile è la print eseguita su un oggetto fclust
#darà diverse diverse informazioni riguardo l'output come la summary
#però ne darà di meno e in maniera più compatta
print(res.fkm)

#per permetterci di vedere un "riassunto" dei risultati creiamo
#la funzione info.principali.fclust che conserviamo in un altro file R
source("info_principali_fclust.R")
info.principali.fclust(res.fkm)

#il plot offerto dalla libreria fclust permette di visualizzare i cluster
#tenendo in considerazione 2 variabili

#Il flag pca se impostato a true permette di visualizzare i cluster
#in base alle componenti principali. 
#L'analisi delle componenti principali è una tecnica di 
#riduzione della dimensionalità usata per la visualizzazione dati

plot(res.fkm,pca=TRUE)
#plot.fclust(res.fkm,pca=TRUE)
#la funzione non è estremamente intuitiva e funzionale per quanto
#riguarda la visualizzazione

#Tramite la funzione Fclust.index con i seguenti parametri otteniamo
# il valore di Silhouette fuzzy, un parametro che ci indica la qualità 
#dei nostri risultati
res.SilF.FKM <- Fclust.index(res.fkm,index="SIL.F",alpha = 1)
str <- paste("Il coefficiente di fuzzy Silhouette è ", toString(res.SilF.FKM))
cat(str)
rm(str)
#Abbiamo usato la versione base dell'algoritmo, ora dobbiamo usare anche le sue
#estensioni per valutare quale fra esse si comporti meglio sul dataset.
#Per il confronto useremo l'indice di Silhouette fuzzy

source("miglior_oggetto_fclust_SILF.R")
gc()
miglior_clustering_SILF <- miglior.oggetto.fclust.SILF(df,res.fkm)

source("info_principali_fclust.R")
info.principali.fclust(miglior_clustering_SILF)
plot(miglior_clustering_SILF,pca=TRUE)

### VALIDAZIONE  ###

source("validazione_fclust_SILF.R")
#passando print = FALSE si disattivano le stampe(tranne il coeff. di fuzzy sil.)
out <- validazione.fclust.SILF(miglior_clustering_SILF,print = FALSE)

