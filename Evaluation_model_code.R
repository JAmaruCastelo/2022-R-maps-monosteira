########################################################################################################################
####### Evaluaciones de los modelos con niche tool box #################################################################
#########################################################################################################################
##### librerias que se estan utilizando
library ("easypackages")
libraries (c("dismo", "maps", "maptools", "rgdal", "rgeos", "sp", "virtualspecies", "corrplot", 
             "raster", "usdm", "gtools", "tidyverse", "rJava", "ENMeval", "rgbif",
             "ENMTools", "geodata", "devtools", "RStoolbox", "glue", "rmaxent", "ENMeval",
             "sf", "xlsx", "rgl"))
library(ellipsenm)
library(ntbox)

######################################################################################################
####### partial ROC ##################################################################################
#####################################################################################################
######### para los valores de suitability del modelo de MAXENT
r<-raster("C:/Users/amaru/OneDrive/Escritorio/prueba/LQ05/modelo_presente_paleartico.asc")
c<-read_csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/nueva_calcular/Monosteira_unicostata_registros_puntuales_data.csv")
c<-c[c("long","lat")] 
colnames(c)<-c("longitude","latitude")
partial_roc_maxent<-partial_roc(r,data.frame(c),longitude = "longitude", latitude = "latitude")


####### para los valores de suitability del modelo de MVE 
r2<-raster("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/MVE_results/suitability_index.asc")
c2<-read_csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/nueva_calcular/Monosteira_unicostata_registros_puntuales_data.csv")
c2<-c2[c("long","lat")] 
colnames(c2)<-c("longitude","latitude")
partial_roc_MVE<-partial_roc(r2,data.frame(c2),longitude = "longitude", latitude = "latitude")



##################################################################################################
#### calculamos el tSS y CRR para evaluar el modelo que hemos utilizafo###########################
##################################################################################################
##### encontrar los threshold que maximizen el TSS de mis datos

####### para Mi modelo en Maxent creado ##########################################################3
# corresponde al valor de las presencias "c"
# corresponde al valor de las ausencias "a" ## se esta considerando el background como ausencias
# corresponde al modelo continuo "r"
ausencias <- randomPoints (r, 500)
colnames(ausencias)<-c("longitude", "latitude")
ausencias<-cbind(ausencias, c(0))
colnames(ausencias)<-c("longitude","latitude","presence_absence")
presencias<-data.frame(c)
presencias<-cbind(presencias, c(1))
colnames(presencias)<-c("longitude","latitude","presence_absence")
vali<-rbind(ausencias,presencias)

tss_threshold <- confu_mat_optim(sdm_raster = r,valData = vali,
                                 longitude = "longitude",latitude = "latitude",
                                 pres_abs = "presence_absence",optim_by = "tss",
                                 th_range = c(0.005,1),step = 0.005)
### creamos el modelo con el maximo valor
maximo_thre_maxent<-tss_threshold$threshold[1] #### el valor maximo de threshols te 
modelo_binario_maxent<- r >= maximo_thre_maxent


####### para Mi modelo en MVE creado ##########################################################3
# corresponde al valor de las presencias "c"
# corresponde al valor de las ausencias "a" ## se esta considerando el background como ausencias
# corresponde al modelo continuo "r"
ausencias2 <- randomPoints (r2, 500)
colnames(ausencias2)<-c("longitude", "latitude")
ausencias2<-cbind(ausencias2, c(0))
colnames(ausencias2)<-c("longitude","latitude","presence_absence")
presencias2<-data.frame(c2)
presencias2<-cbind(presencias2, c(1))
colnames(presencias2)<-c("longitude","latitude","presence_absence")
vali2<-rbind(ausencias2,presencias2)

tss_threshold2 <- confu_mat_optim(sdm_raster = r2,valData = vali2,
                                  longitude = "longitude",latitude = "latitude",
                                  pres_abs = "presence_absence",optim_by = "tss",
                                  th_range = c(0.005,1),step = 0.005)
### creamos el modelo con el maximo valor
maximo_thre_MVE<-tss_threshold2$threshold[1] #### el valor maximo de threshols te 
modelo_binario_MVE<- r2 >= maximo_thre_MVE





###################################################################################################
###### guardamos la informacion mas importante ####################################################
###################################################################################################
#pROC de Maxent
setwd("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Modelos_evaluation")
pROC_general_maxent<-data.frame("Prediction_partial_AUC"=partial_roc_maxent$pROC_results$Prediction_partial_AUC,
                                "Random_curve_partial_AUC"=partial_roc_maxent$pROC_results$Random_curve_partial_AUC,
                                "AUC_ratio"=partial_roc_maxent$pROC_results$AUC_ratio)
pROC_summary_maxent<-data.frame(partial_roc_maxent$pROC_summary)
write.csv(pROC_general_maxent, "pROC_general_maxent.csv")
write.csv(pROC_summary_maxent, "pROC_summary_maxent.csv")

#pROC de MVE
pROC_general_MVE<-data.frame("Prediction_partial_AUC"=partial_roc_MVE$pROC_results$Prediction_partial_AUC,
                             "Random_curve_partial_AUC"=partial_roc_MVE$pROC_results$Random_curve_partial_AUC,
                             "AUC_ratio"=partial_roc_MVE$pROC_results$AUC_ratio)
pROC_summary_MVE<-data.frame(partial_roc_MVE$pROC_summary)
write.csv(pROC_general_maxent, "pROC_general_MVE.csv")
write.csv(pROC_summary_maxent, "pROC_summary_MVE.csv")


#### guardamos la informacion necesarias de los mapas binarios de las evaluaciones
writeRaster(raster(modelo_binario_maxent), filename = "binario_maxent.tif", format = "GTiff", overwrite=TRUE)
writeRaster(raster(modelo_binario_MVE), filename = "binario_MVE.tif", format = "GTiff", overwrite=TRUE)

### guardamos TSS obtenidos y CRR obtenidos en el analisis
write.csv(tss_threshold, "tss_threshold_maxent.csv")
write.csv(tss_threshold2, "tss_threshold_maxent_MVE.csv")









#####################################################################################################################################################
############################################### SI SE TIENE UN THRESHOLD DADO Y SE QUIERE MODELAR NUEVAMENTE TODO ##################
#####################################################################################################################################################



##### para poder evaluar los modelos bajos estos dos parametros se deben utilizar
#a) modelos binarizados
##### necesitamos volver los datos a mapas binarios para poder analizarlos como TSS o CRS
#### para el modelo creado en MVE ###############################################################
binario_Maxent <- bin_model(r,c,percent = 10) #### minimun training presence ### ntbox
#### para el modelo maxent #####################################################################
binario_MVE <- bin_model(r2,c2,percent = 10) #### minimun training presence  ### ntbox
#### se les esta calculando de esa dorma para poder mas adelante sacar mas cosas diferentes

##########################################################################################
#estamos considerando las ausencias como parte del background de las areas recortadas, para
# ver si podemos reconocer si las presencias presentan mayor indice de suitabilidad que el background
# extraemos background de los datos
#para lo mapas binarios de maxent

backgr <- randomPoints (binario_Maxent, 500) #### extraigo 500 coordenadas generales adicionales
absvals <- raster::extract (binario_Maxent, backgr) #### extrago los valores de las 500 de el binario_maxent
presencias1=raster::extract(binario_Maxent,c) ### para las presencias detectadas
a=sum(data.frame(values=presencias1)$values==1)    #### presencias que fueron detectadas como presencias
b=sum(data.frame(values=presencias1)$values==0)    #### presencias que fueron detectadas como ausencias
c=sum(data.frame(values=absvals)$values==1)    #### ausencias que fueron detectadas como presencias
d=sum(data.frame(values=absvals)$values==0)    #### ausencias que fueron detectadas como ausencias

sens<-sensibilidad(a,c)  
esp<-especificidad(b,d)
kap<-kappa(a,b,c,d)
ts<-tss(a,b,c,d)

#################################################################################################
#para los mapas binarios de MVE##################################################################
#################################################################################################
backgr2 <- randomPoints (binario_MVE, 500)
absvals2 <- raster::extract (binario_MVE, backgr2)
presencias2=raster::extract(binario_Maxent,c2)

a2=sum(data.frame(values=presencias2)$values==1)    #### presencias que fueron detectadas como presencias
b2=sum(data.frame(values=presencias2)$values==0)    #### presencias que fueron detectadas como ausencias
c2=sum(data.frame(values=absvals2)$values==1)    #### ausencias que fueron detectadas como presencias
d2=sum(data.frame(values=absvals2)$values==0)    #### ausnecias que fueron detectadas como ausencias

sens2<-sensibilidad(a2,c2)  
esp2<-especificidad(b2,d2)
kap2<-kappa(a2,b2,c2,d2)
ts2<-tss(a,b,c,d)

#################################################################################################
######## sAVING INFORMATION THAT IT IS NECESSARY#################################################
#################################################################################################

# guardamos los raster con los threshold realizados
writeRaster(raster(binario_Maxent), filename = "binario_maxent.tif", format = "GTiff", overwrite=TRUE)
writeRaster(raster(binario_MVE), filename = "binario_MVE.tif", format = "GTiff", overwrite=TRUE)
# guardamos los modelos evaluados en los parametros binarios
evaluacion_general<-data.frame("Maxent"=c(sens,esp,kap,ts), "MVE"=c(sens2,esp2,kap2,ts2))
rownames(evaluacion_general)<-c("sensibilidad", "especificidad", "kappa", "tss")
write.csv(evaluacion_general, "evaluacion_binaria_general.csv")


###### funciones que se tienen que tener en cuenta para evaluar cada una de las variables como threshold  
f_error_com <- function(b,d)    return(b/(b+d))
f_error_om <- function(a,c)  return( c/(a+c))
sensibilidad <- function(a,c)  return(a/(a+c))
especificidad <- function(b,d)  return(d/(b+d))
tas_fals_pos <- function(b,d) return(b/(b+d))
tas_fals_neg <- function(a,c) return(c/(a+c))
posit_pre_pow <- function(a,b) return(a/(a+b))
nega_pre_pow <- function(c,d) return(d/(c+d))
miss_cla_rate <- function(a,b,c,d) return((b+c)/(a+b+c+d))
prevalencia <- function(a,b,c,d) return((b + d)/(a+b+c+d))
correct_class_rate <- function(a,b,c,d) return((a + d)/(a+b+c+d))
tss <- function(a,b,c,d) return(sensibilidad(a,c)+especificidad(b,d)-1)
kappa <- function(a,b,c,d){
  N <- a+b+c+d
  term1 <- ((a+d)-(((a+c)*(a+b)+(b+d)*(c+d))/N))
  term2 <- (N-(((a+c)*(a+b)+(b+d)*(c+d))/N))
  return(term1/term2) }
  
  