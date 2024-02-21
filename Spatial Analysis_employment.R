#### INSTALLA ####
install.packages("maptools")
install.packages("tripack")
install.packages("spdep")
install.packages("moments")
install.packages("lmtest")
install.packages("nortest")
install.packages("car")
install.packages("spatialreg")
install.packages("leaflet")
install.packages("RColorBrewer")
install.packages("spData")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("sf")
install.packages("rcompanion")
install.packages("mapview")
#### CARICA   ####
library(tripack)
library(spdep)
library(moments)
library(lmtest)
library(nortest)
library(car)
library(spatialreg)
library(leaflet)
library(RColorBrewer)
library(spData)
library(ggplot2)
library(stargazer)
library(readxl)
library(sf)
library(rcompanion)
library(mapview)

library(labstatR)
library(moments)
library(e1071)
library(prettyR)
library(lmtest)
library(nortest)

## IMPORTAZIONE DEL DATASET e MERGE TRA I FILE
data <- read_excel("C:/Users/Francesco/OneDrive - Uniparthenope/Punzo_python/data.xlsx", 
                   col_types = c("text",
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric",
                                 "numeric"))
View(data)
path=file.choose()
shp=st_read(path)#la funzione st_read importa lo shp file sottoforma di dataset
View(shp)
colnames(shp)[colnames(shp) == "DEN_UTS"] <- "Provincia"
dataset=merge(shp,data, by="Provincia")

str(dataset)
View(dataset)

#### ANALISI DESCRITTIVA ####

summary(dataset$`Tasso di occupazione giovanile (15-29 anni)`)
boxplot(dataset$`Tasso di occupazione giovanile (15-29 anni)`)
attach(dataset)
hist(`Tasso di occupazione giovanile (15-29 anni)`, probability = T,xlab = "Tasso di occupazione giovanile (15-29 anni)",    main = "Tasso di occupazione giovanile (15-29 anni)")
plotNormalHistogram(`Tasso di occupazione giovanile (15-29 anni)`,main="Livelli di Tasso di occupazione giovanile (15-29 anni) - frequenze") 
skewness(`Tasso di occupazione giovanile (15-29 anni)`)
kurtosis(`Tasso di occupazione giovanile (15-29 anni)`)

###AGGIUNGERE ALTRI ELEMENTI DESCRITTIVI, Normalità?...
shapiro.test(`Tasso di occupazione giovanile (15-29 anni)`)##

#### OLS #### 
## Creazione di un modello di REGRESSIONE
###MODELLO OLS###, considero tutte le variabili
var_espl <- `Tasso di occupazione giovanile (15-29 anni)` ~ `Competenza numerica non adeguata (studenti classi III scuola secondaria primo grado)`+
  `Laureati e altri titoli terziari (25-39 anni)`+
  `Mobilità dei laureati italiani (25-39 anni)`+
  `Organizzazioni non profit`+
  `Partecipazione alla formazione continua`+
  `Partecipazione elettorale`+
  `Persone con almeno il diploma (25-64 anni)`+
  `Raccolta differenziata dei rifiuti urbani`
chi.ols <- lm(var_espl, data = dataset)
summary(chi.ols) ## Noto che non tutte le variabili sono significative
AIC(chi.ols)

###STEPWISE###, mi restituisce un nuovo modello di regressione multipla
stepwise=step(chi.ols, directory="both")
summary(stepwise)
var_step = `Tasso di occupazione giovanile (15-29 anni)` ~ 
  `Competenza numerica non adeguata (studenti classi III scuola secondaria primo grado)` + 
  `Mobilità dei laureati italiani (25-39 anni)` + `Organizzazioni non profit` + 
  `Partecipazione elettorale` + `Persone con almeno il diploma (25-64 anni)` + 
  `Raccolta differenziata dei rifiuti urbani`
ols.new <- lm(var_step, data = dataset)
summary(ols.new)
AIC(ols.new)

#### ASSUNZIONI ####

### Verifica delle Assunzioni del Modello OLS
## Test per la normalità dei residui
res = ols.new$residuals
shapiro.test(res)
jarque.test(res)
## Test per l'omoschedasticità
## Breusch-Pagan
bptest(ols.new)
##Test di Durbin-Watson, per verificare un'eventuale correlazione tra i residui
dwtest(ols.new)

## Multicollinearità
library(car)
vif(ols.new) ## VIF
library(olsrr)
ols_vif_tol(ols.new) ## Tolleranza







## MORAN LM
moran.lm<-lm.morantest(ols.new, listw, alternative="two.sided") #verifica la presenza di autocorrelazione spaziale nei residui del modello lineare stimato
moran.lm



#### ANALISI SPAZIALE #####


## GRAFICI
plot(st_geometry(dataset),col='grey')
mapview(dataset)
mapview(dataset, zcol= 'Tasso di occupazione giovanile (15-29 anni)')
palette <- brewer.pal(5, "OrRd")
mapview(dataset, zcol='Tasso di occupazione giovanile (15-29 anni)', layer.name='Valori del Tasso',at=quantile(`Tasso di occupazione giovanile (15-29 anni)`), col.regions = palette)




## CREAZIONE DELLA MATRICE DI CONTIGUITA'
## Criterio della Regina
mapview(dataset)
list.queen<-poly2nb(dataset, queen=TRUE) #poly2nb crea una lista dei vicini
list.queen
summary(list.queen)
coords<-st_centroid(st_geometry(dataset),of_largest_polygon = TRUE) #la funzione definisce il centroide di ciascun poligono
plot(st_geometry(dataset),col='lightgrey')
plot(list.queen,coords=coords,add=T,col='blue',lwd=2) ## plot con tutti i collegamenti tra le regioni considerate vicine

## Criterio della torre
list.rook<-poly2nb(dataset, queen=F) ## per applicare il criterio impongo queen=False
summary(list.rook)
plot(st_geometry(dataset),col='lightgrey')
plot(list.rook,coords=coords,add=T,col='red',lwd=2)

#### Matrice di Distanze
#listwqueen
listw<-nb2listw(list.queen, style="B", zero.policy=TRUE)
View(listw)
#matqueen
W<-nb2mat(list.queen, style="B", zero.policy=TRUE)
View(W)

# Distanza
coords<-st_centroid(st_geometry(dataset),of_largest_polygon = TRUE) ### calcolo dei centroidi
W_dist <- dnearneigh(coords,0,80000) ## per calcolare le distanze tra i centroidi
W_dist
plot(st_geometry(dataset),col='lightgrey')
plot(W_dist,coords=coords,add=T,col='green',lwd=2)

## Confronto tra i tre criteri
par(mfrow = c(1, 3)) 
plot(st_geometry(dataset),col='lightgrey',main="QUEEN CRITERION" )
plot(list.queen,coords=coords,add=T,col='blue',lwd=2)
plot(st_geometry(dataset),col='lightgrey',main="ROOK CRITERION" )
plot(list.rook,coords=coords,add=T,col='red',lwd=2)
plot(st_geometry(dataset),col='lightgrey',main="DISTANCE CRITERION" )
plot(W_dist,coords=coords,add=T,col='green',lwd=2)
dev.off()

#### INDICI DI AUTOCORRELAZIONE SPAZIALE ######


##### Calcolo degli indici di autocorrelazione spaziale
#Moran's I, compreso tra -1 e 1
listw<-nb2listw(list.queen, style="W", zero.policy=TRUE)
moran(`Tasso di occupazione giovanile (15-29 anni)`,listw=listw, length(`Tasso di occupazione giovanile (15-29 anni)`),Szero(listw), zero.policy=T)
moran.test(`Tasso di occupazione giovanile (15-29 anni)`, listw, zero.policy=T) ## il valore indica una correlazione spaziale positiva

#Plot, il coefficiente angolare della retta è uguale al valore della statistica I.Moran
moran.plot(`Tasso di occupazione giovanile (15-29 anni)`,listw,zero.policy=T, pch=20, col="blue")
mp<-moran.plot(`Tasso di occupazione giovanile (15-29 anni)`,listw, zero.policy=T, pch=20, col="blue")
summary(mp)


#Plot valori standardizzati
S_TASSO<- scale(`Tasso di occupazione giovanile (15-29 anni)`)
moran.plot(as.vector(S_TASSO),listw,zero.policy = T,pch=20,col="blue")
W_S_TASSO<- scale(lag.listw(listw, `Tasso di occupazione giovanile (15-29 anni)`))
plot(x = S_TASSO, y = W_S_TASSO, main = "Moran Scatterplot I = ###")
abline(h = 0, v = 0,lty=2,col="blue") ## Creo un sistema di assi cartesiani
abline(lm(W_S_TASSO~S_TASSO), lty=1, lwd=2, col="red")

#Geary C, indice alternativo a quello di moran, 
## Varia tra 0 e 2
geary.test(`Tasso di occupazione giovanile (15-29 anni)`,listw,zero.policy = T) ## il risultato è coerente con il test dell'Indice di Moran

## Calcolo del Moran Locale
#Local Moran's I - LISA
library(rgeoda)
tasso_occ_giov = dataset["Tasso di occupazione giovanile (15-29 anni)"]
listw1=queen_weights(dataset) 
summary(listw1)
lisa <- local_moran(listw1, tasso_occ_giov) ## Creo LISA
lisa ## Mi da anche le etichette, quindi Alto-alto, basso-basso, alto-basso, basso-alto

lms <- lisa_values(gda_lisa = lisa) #per ottenere i valori del Moran locale
lms

pvals <- lisa_pvalues(lisa) #per otterere i valori p di significatività del Moran locale
pvals

lisa_colors <- lisa_colors(lisa)
lisa_colors
#eeeeee corrisponde a "lightgray"
#FF0000 corrisponde a "red"
#0000FF corrisponde a "blue"
#a7adf9 potrebbe essere descritto come una tonalit? di blu pallido o lilla chiaro.
#f4ada8 potrebbe essere descritto come una tonalit? di rosa pallido o salmone chiaro.
#464646 corrisponde a "dimgray" o "gray30" (una tonalit? di grigio scuro).
#999999 corrisponde a "gray60" (una tonalit? di grigio medio).

lisa_labels <- lisa_labels(lisa)
lisa_labels

lisa_clusters <- lisa_clusters(lisa)
lisa_clusters

##Costruzione Plot
plot(st_geometry(dataset), 
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.2)
title(main = "Local Moran Map of Tasso di Occupazione Giovanile") ## Titolo
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee") ## legenda
## i valori in grigio evidenziano la presenza di valori non significativi

pvals <- lisa_pvalues(lisa)
p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
p_colors <- c("#eeeeee", "#84f576", "#53c53c", "#348124")
plot(st_geometry(dataset), 
     col=sapply(pvals, function(x){
       if (x <= 0.001) return(p_colors[4])
       else if (x <= 0.01) return(p_colors[3])
       else if (x <= 0.05) return (p_colors[2])
       else return(p_colors[1])
     }), 
     border = "#333333", lwd=0.2)
title(main = "Local Moran Map of Tasso di Occupazione Giovanile")
legend('topright', legend = p_labels, fill = p_colors, border = "#eeeeee")

#### MODELLI SPAZIALI ####


## LM sta per Lagrange Multiplayer
## Per scegliere tra SAR e SEM ci sono dei Test che si possono fare, prima della creazione del modello
## Ad esempio facendo l'indice di moran sulla variabile Y, o sui residui, quindi su Epsilon.
## Con i test posso vedere qual'è il modello che mi conviene calcolare

#LM test, ulteriore test per vedere che modello che mi conviene calcolare
## Sfrutta i valori chiave per i due modelli
## ro per il SAR, quindi vedo se il ro è significativo. Se ro è significativo provo a stimare un SAR. Il test è un lm-leg?
## Lambda nel caso del SEM, quindi vedo se lambda è significativo. Se lambda è significativo provo a stimare un SEM. Il test è un lm-error?
### SE LAMBDA E RO SONO ENTRAMBI NON SIGNIFICATIVI VALUTO SEMPLICEMENTE UN MODELLO OLS TRADIZIONALE

## Sono stati creati altri due test, simili al LM-LAG e LM-ERROR, ma nella loro versione ROBUSTA.
## I testi robusti tendono ad essere più conservativi, tendono a non rigetttare l'ipotesi nulla
## Però se con questi test la variabile è significativa è veramente significativa.
## In teoria gli stimo se entrambi i primi due test sono entrambi significativi.
## I test si riconducono a delle chi-quadrato.
## Se entrambi i test comunque portano a la significativitò delle variabili allora
## stimo il modello che presenta la statistica test più elevata, o anche un modello che tenga conto di ro e lambda contemporaneamente, in cui il ritardo spaziale è considerato sia nella variabile dipendente che in quella dei residui. Gli chiamiamo modelli SAC, nei modelli si serie storiche invece ARMA.
## Per stimare questo SAC, alcuni utilizzano due matrice dei pesi differenti per Y e i residui, per risolvere alcuni processi nel calcolo delle STIME



LM_tests<-lm.LMtests(ols.new,listw,test="all",zero.policy = T) ## test=all, cioè stima tutti i test
LM_tests 

#SAR, il ritardo autospaziale sta nella variabile dipendente Y
sar.chi<-lagsarlm(var_step, data=dataset, listw,tol.solve = 6.2817e-17) ## tol.solve serve quando andiamo a stimare gli effetti? Ci potrebbero essere problemi col determinante che risolve con questo valore di p-value molto basso. E' una forzatura per far byassare un problema nel calcolo del determinante della matrice inversa. Il determinante mi serve diverso da zero. Serve per calcolare gli effettiv diretti e indiretti del modello.
summary(sar.chi)
AIC(sar.chi) 

# Stima di un modello SEM, considera un ritardo autospaziale nei residui
sem.chi<-errorsarlm(var_step, data=dataset, listw)
summary(sem.chi,Nagelkerke=TRUE)
AIC(sem.chi)

## Funzione visiva per confrontare i tre modelli OLS, SAR e SEM
stargazer(ols.new, sar.chi, sem.chi,type="text",title = "OLS - SAR - SEM RESULTS")
AIC(ols.new)

# Stima di un modello SAC
sac.chi<-sacsarlm(var_step, data=dataset, listw, method="LU")
summary(sac.chi,Nagelkerke=TRUE)
AIC(sac.chi)
## Stima di un modello SDM, ritardi spaziali nella variabile Y e nelle Covariate
sdm.chi<-lagsarlm(var_step, data=dataset, listw, type = "mixed", tol.solve = 1.10e-20)
summary(sdm.chi,Nagelkerke=TRUE)
AIC(sdm.chi)
# Stima di un modello SDEM, è un estensione del SEM, ha ritardi spaziali nella covariate e nei residui
sdem.chi<-errorsarlm(var_step, data=dataset, listw, etype="mixed")
summary(sdem.chi,Nagelkerke=TRUE) 
AIC(sdem.chi)

#SLX, Spacial Lag X, ritardo spaziale solo nelle covariate
slx.chi<-lmSLX(var_step, data=dataset, listw)
summary(slx.chi)
AIC(slx.chi)

#GNS / Manski Model - General Nesting Spatial, è il modello più completo, quindi ritardo spaziale nella Y, nelle covariate e nei residui
gns.chi<-sacsarlm(var_step, data=dataset, listw,type = "mixed")
summary(gns.chi,Nagelkerke=TRUE) 
AIC(gns.chi)


AIC(sar.chi)
AIC(sem.chi)
AIC(sac.chi)
AIC(sdm.chi)
AIC(sdem.chi)
AIC(slx.chi)
AIC(gns.chi)


#Effetti diretti e indiretti SAR , SAC e SDM
## Costruisco prima la matrice
## Effetti diretti sono sulla diagonale
## Effetti indiretti fuori dalla diagonale
W2<-as(listw, 'CsparseMatrix')
trMat<-trW(W2,type="mult")

## Impatti modello SAR
impacts_sar<-impacts(sar.chi,tr=trMat,R=100)
impacts_sar
summary(impacts_sar, zstats=TRUE, short=TRUE)


impacts_sac<-impacts(sac.chi,tr=trMat,R=100)
impacts_sac
summary(impacts_sac, zstats=TRUE, short=TRUE)
