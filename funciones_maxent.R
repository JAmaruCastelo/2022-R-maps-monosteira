###### este es un codigo para crear los AIC; AICc Y BIC similar a los realizados por ENMTOols

library(raster)

calAIC <- function(csvfile, grdfile, lambdasfile){
  nparams = 0
  probsum = 0
  loglikelihood = 0
  AICcscore = 0
  AICscore = 0
  BICscore = 
    
  lambdases <- read.csv(glue("{lambdasfile}/species.lambdas"), header=FALSE)
  nparams <- nrow(lambdases[lambdases$V2 != 0, ])
  nparams = nparams - 4
  
  layerRaw <- raster(grdfile)
  probsum <- cellStats(layerRaw, sum)
  
  latitud_longitud
  
  points <- data.frame(csvfile)
  npoints <- nrow(points)
  layerValues <- raster::extract(layerRaw, points[c("long", "lat")])
  loglikelihood <- sum(log(layerValues / probsum))
  
  if (nparams >= npoints - 1) {
    AICcscore <- "x"
    AICscore <- "x"
    BICscore <- "x"
  } else {
    AICcscore = (2 * nparams - 2 * loglikelihood) + (2 * (nparams) * (nparams + 1) / (npoints - nparams - 1))
    AICscore = 2 * nparams - 2 * loglikelihood
    BICscore = nparams * log(npoints) - 2 * loglikelihood
  }
  
  ICs <- c(latitud_longitud, grdfile, loglikelihood, nparams, npoints, AICscore, AICcscore, BICscore)
  return(ICs)}

calculate <- function(lista, latitud_longitud) {
  # esta es una funcion para calcular y evaluar cada uno de los modelos se basa en lo obtenido
  # mediante eval de ENMtools
  # necesita un archivo lista: que es una lista de carpetas con modelos maxent a analizar
  # tambien un archivo con longitudes_latitudes ves los posibles errores que puede tener con este archivo
  csvfile=latitud_longitud
  data<-data.frame("name"=c(),"Log-Likelihood"=c(),"Parameters"=c(),"Sample_Size"=c(),
                   "AIC_score"=c(),"AICc_score"=c(), "BIC_score"=c())
  for (e in lista){
    modelo=import_maxent(e)
    prediccion=predict(modelo,pca1$rasters)
    grdfile=prediccion
    lambdasfile=e
    cal=calAIC(csvfile, grdfile , lambdasfile)
    data2<-data.frame("name"=e,"Log_Likelihood"=c(cal[[4]]),"Parameters"=c(cal[[5]]),"Sample_Size"=c(cal[[6]]),
                      "AIC_score"=c(cal[[7]]),"AICc_score"=c(cal[[8]]), "BIC_score"=c(cal[[9]]))
    data <- rbind (data, data2)
    print("hecho")}
  return (data)
}

##### esta es una funcion que podria usarse mas adelante si se necesitara sacar de elementos creados por el modelo propio de maxent
##### ver esta funcion para corregirla mas adelante

getAICs <- function(modelfile) {
  models <- read.csv(modelfile, header=FALSE, as.is=TRUE, col.names=c("csvfile", "grdfile", "lambdasfile"))
  AICs <- mapply(calAIC, models$csvfile, models$grdfile, models$lambdasfile, USE.NAMES=FALSE)
  AICs <- t(AICs)
  colnames(AICs) <- c("Points", "ASCII file", "Log Likelihood", "Parameters", "Sample Size", "AIC score", "AICc score", "BIC score")
  outfile <- gsub(".csv", "_model_select.csv", modelfile)
  write.csv(AICs, outfile, row.names=FALSE)
}

