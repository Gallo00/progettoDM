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

dati_csv <- read.csv("CC_GENERAL.csv")
CC_df <- data.frame(dati_csv)
rm(dati_csv)
print(CC_df)
#Nel dataframe è presente una colonna che contiene degli id, ovviamente 
# la scartiamo
CC_df <- CC_df[,-1]

#il dataset è abbastanza grande, scartiamo per semplicità le righe
#contenenti valori NA
#il numero di righe scartate è poco meno di 200 su 9000, è un buon compromesso
CC_df <- na.omit(CC_df)

#usiamo un numero inferiore di tuple per semplicità
perc <- 0.10 #nel dataframe ci sono circa 8600 tuple, decidiamo la percentuale
#che vogliamo usare
num_tuple <- round(perc*nrow(CC_df))
set.seed(111222333)
pos_rand <- sample(1:nrow(CC_df),num_tuple)

CC_df <- CC_df[pos_rand,]
rm(perc)
rm(num_tuple)
rm(pos_rand)
#features selection
#Per semplificare il modello scartiamo le features che non sono importanti
#ovvero non aiutano nella distinzione degli oggetti

#Quello che possiamo fare è scartare features ridondanti
#Sfrutteremo il metodo findCorrelation per trovare le variabili
#con forti gradi di correlazione(in genere con forti si intende con
#grado maggiore o uguale 0.75)
corr <- cor(CC_df[,1:17])
print(corr)
variabili.molto.correlate <- findCorrelation(corr,cutoff = 0.75)
str <- paste("Le seguenti variabili sono molto correlate alle altre, dunque le scartiamo \n",toString(variabili.molto.correlate))
cat(str)

#con questo semplice for troviamo i nomi delle colonne da scartare
var <- c()
for(i in 1:length(variabili.molto.correlate)) {
  var <- c(var,colnames(CC_df)[variabili.molto.correlate[i]])
}

#questa operazione permette di rimuovere da CC_df le colonne
#con i nomi trovati nel for
CC_df <- CC_df[, !(names(CC_df) %in% var)]

rm(str)
rm(i)
rm(var)
rm(variabili.molto.correlate)
rm(corr)
#definiamo la funzione di normalizzazione min max
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#applichiamo la min max sui nostri dati
CC_df_norm <- as.data.frame(lapply(CC_df[1:ncol(CC_df)], min_max_norm))
rm(min_max_norm)
rm(CC_df)

gc()
res.fkm <- FKM(CC_df_norm,k=2:6,index="SIL.F",alpha=1)
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

source("miglior_oggetto_fclust.R")
gc()
miglior_clustering <- miglior.oggetto.fclust(CC_df_norm,res.fkm)

info.principali.fclust(miglior_clustering)


