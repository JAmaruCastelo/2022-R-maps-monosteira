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
setwd ("D:/prueba/point/Evaluation_minimun training threshold")
######### para los valores de suitability del modelo de MAXENT######################################
#######################################################################################################
# proyectado de introduced
r1<-raster("D:/0001_JAC_monosteira/Modelos_Maxent/Maxent_introduced/Modelos/L05/modelo_presente_calibration.tif")
c1<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/introduced_clean.csv")
c1<-c1[c("Longitud","Latitud")] 
colnames(c1)<-c("longitude","latitude")
partial_roc_maxent_introducido<-partial_roc(r1,data.frame(c1),longitude = "longitude", latitude = "latitude")

# proyectado para paleartico
r1b<-raster("D:/0001_JAC_monosteira/Modelos_Maxent/Maxent_native/Modelos/LQ05/modelo_presente_calibration.tif")
c1b<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/paleartico_clean.csv")
c1b<-c1b[c("Longitud","Latitud")] 
colnames(c1b)<-c("longitude","latitude")
partial_roc_maxent_paleartico<-partial_roc(r1b,data.frame(c1b),longitude = "longitude", latitude = "latitude")


# proyectado para full
r1c<-raster("D:/0001_JAC_monosteira/Modelos_Maxent/Maxent_complete/Modelos/LQ05/modelo_presente_calibration.tif")
c1c<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/total_records_clean.csv")
c1c<-c1c[c("Longitud","Latitud")] 
colnames(c1c)<-c("longitude","latitude")
partial_roc_maxent_full<-partial_roc(r1c,data.frame(c1c),longitude = "longitude", latitude = "latitude")



##########################################################################################################
####### para los valores de suitability del modelo de MVE ###############################################
##########################################################################################################

# proyectado de introduced
r2a<-raster("D:/prueba/point/continuos/introduced_clean_nichea.tiff")
c2a<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/introduced_clean.csv")
c2a<-c2a[c("Longitud","Latitud")] 
colnames(c2a)<-c("longitude","latitude")
partial_roc_MVE_introduced<-partial_roc(r2a,data.frame(c2a),longitude = "longitude", latitude = "latitude")

# proyectado de paleatrico
r2b<-raster("D:/prueba/point/continuos/paleartico_clean_nichea.tiff")
c2b<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/paleartico_clean.csv")
c2b<-c2b[c("Longitud","Latitud")] 
colnames(c2b)<-c("longitude","latitude")
partial_roc_MVE_paleartic<-partial_roc(r2b,data.frame(c2b),longitude = "longitude", latitude = "latitude")

#proyectado de full
r2c<-raster("D:/prueba/point/continuos/total_records_clean_nichea.tiff")
c2c<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/total_records_clean.csv")
c2c<-c2c[c("Longitud","Latitud")] 
colnames(c2c)<-c("longitude","latitude")
partial_roc_MVE_full<-partial_roc(r2c,data.frame(c2c),longitude = "longitude", latitude = "latitude")




##################################################################################################
#### calculamos el tSS y CRR para evaluar el modelo que hemos utilizafo###########################
##################################################################################################
##### encontrar los threshold que maximizen el TSS de mis datos

#############################################################################################################
####### para Mis tres modelo en Maxents creados ###########################################################
###########################################################################################################
# corresponde al valor de las presencias "c"
# corresponde al valor de las ausencias "a" ## se esta considerando el background como ausencias
# corresponde al modelo continuo "r"

#para introduced
ausencias1a <- randomPoints (r1, 500)
colnames(ausencias1a)<-c("longitude", "latitude")
ausencias1a<-cbind(ausencias1a, c(0))
colnames(ausencias1a)<-c("longitude","latitude","presence_absence")
presencias1a<-data.frame(c1)
presencias1a<-cbind(presencias1a, c(1))
colnames(presencias1a)<-c("longitude","latitude","presence_absence")
vali1a<-rbind(ausencias1a,presencias1a)

tss_threshold_1a <- confu_mat_optim(sdm_raster = r1, valData = vali1a,
                                    longitude = "longitude",latitude = "latitude",
                                    pres_abs = "presence_absence",optim_by = "tss",
                                    th_range = c(0.005,1),step = 0.005)
### creamos el modelo con el maximo valor
maximo_thre_maxent_introduced<-tss_threshold_1a $threshold[1] #### el valor maximo de threshols te 
modelo_binario_maxent_calibrado_introducido<- r1 >= maximo_thre_maxent_introduced


# para paleartico
ausencias1b <- randomPoints (r1b, 500)
colnames(ausencias1b)<-c("longitude", "latitude")
ausencias1b<-cbind(ausencias1b, c(0))
colnames(ausencias1b)<-c("longitude","latitude","presence_absence")
presencias1b<-data.frame(c1b)
presencias1b<-cbind(presencias1b, c(1))
colnames(presencias1b)<-c("longitude","latitude","presence_absence")
vali1b<-rbind(ausencias1b,presencias1b)

tss_threshold_1b <- confu_mat_optim(sdm_raster = r1b, valData = vali1b,
                                    longitude = "longitude",latitude = "latitude",
                                    pres_abs = "presence_absence",optim_by = "tss",
                                    th_range = c(0.005,1),step = 0.005)
### creamos el modelo con el maximo valor
maximo_thre_maxent_paleartico<-tss_threshold_1b $threshold[1] #### el valor maximo de threshols te 
modelo_binario_maxent_calibrado_paleartico<- r1b >= maximo_thre_maxent_paleartico



# para full 
ausencias1c <- randomPoints (r1c, 500)
colnames(ausencias1c)<-c("longitude", "latitude")
ausencias1c<-cbind(ausencias1c, c(0))
colnames(ausencias1c)<-c("longitude","latitude","presence_absence")
presencias1c<-data.frame(c1c)
presencias1c<-cbind(presencias1c, c(1))
colnames(presencias1c)<-c("longitude","latitude","presence_absence")
vali1c<-rbind(ausencias1c,presencias1c)

tss_threshold_1c <- confu_mat_optim(sdm_raster = r1c, valData = vali1c,
                                    longitude = "longitude",latitude = "latitude",
                                    pres_abs = "presence_absence",optim_by = "tss",
                                    th_range = c(0.005,1),step = 0.005)
### creamos el modelo con el maximo valor
maximo_thre_maxent_full<-tss_threshold_1c$threshold[1] #### el valor maximo de threshols te 
modelo_binario_maxent_calibrado_full<- r1c >= maximo_thre_maxent_full


#################################################################################################
####### para Mis tres modelos en MVE creados ############################################################
##################################################################################################
# corresponde al valor de las presencias "c"
# corresponde al valor de las ausencias "a" ## se esta considerando el background como ausencias
# corresponde al modelo continuo "r"

#### para introduced
ausencias2a <- randomPoints (r2a, 500)
colnames(ausencias2a)<-c("longitude", "latitude")
ausencias2a<-cbind(ausencias2a, c(0))
colnames(ausencias2a)<-c("longitude","latitude","presence_absence")
presencias2a<-data.frame(c2a)
presencias2a<-cbind(presencias2a, c(1))
colnames(presencias2a)<-c("longitude","latitude","presence_absence")
vali2a<-rbind(ausencias2a,presencias2a)

tss_threshold2_introduced <- confu_mat_optim(sdm_raster = r2a,valData = vali2a,
                                  longitude = "longitude",latitude = "latitude",
                                  pres_abs = "presence_absence",optim_by = "tss",
                                  th_range = c(0.005,1),step = 0.005)
### creamos el modelo con el maximo valor
maximo_thre_MVE_introduced<-tss_threshold2_introduced$threshold[1] #### el valor maximo de threshols te 
modelo_binario_MV_introducedE<- r2a >= maximo_thre_MVE_introduced

modelo_binario_MV_introducedE[modelo_binario_MV_introducedE==0] <- NA
polygono_cortar<-as.polygons(rast(modelo_binario_MV_introducedE))
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "modelo_binario_MV_introducedE.shp")


#para paleartico
ausencias2b <- randomPoints (r2b, 500)
colnames(ausencias2b)<-c("longitude", "latitude")
ausencias2b<-cbind(ausencias2b, c(0))
colnames(ausencias2b)<-c("longitude","latitude","presence_absence")
presencias2b<-data.frame(c2b)
presencias2b<-cbind(presencias2b, c(1))
colnames(presencias2b)<-c("longitude","latitude","presence_absence")
vali2b<-rbind(ausencias2b,presencias2b)

tss_threshold2b <- confu_mat_optim(sdm_raster = r2b,valData = vali2b,
                                  longitude = "longitude",latitude = "latitude",
                                  pres_abs = "presence_absence",optim_by = "tss",
                                  th_range = c(0.005,1),step = 0.005)
### creamos el modelo con el maximo valor
maximo_thre_MVE_paleartico<-tss_threshold2b$threshold[1] #### el valor maximo de threshols te 
modelo_binario_MVE_paleartico<- r2b >= maximo_thre_MVE_paleartico

modelo_binario_MVE_paleartico[modelo_binario_MVE_paleartico==0] <- NA
polygono_cortar<-as.polygons(rast(modelo_binario_MVE_paleartico))
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "modelo_binario_MVE_paleartico.shp")



#para full
ausencias2c <- randomPoints (r2c, 500)
colnames(ausencias2c)<-c("longitude", "latitude")
ausencias2c<-cbind(ausencias2c, c(0))
colnames(ausencias2c)<-c("longitude","latitude","presence_absence")
presencias2c<-data.frame(c2c)
presencias2c<-cbind(presencias2c, c(1))
colnames(presencias2c)<-c("longitude","latitude","presence_absence")
vali2c<-rbind(ausencias2c,presencias2c)

tss_threshold2c <- confu_mat_optim(sdm_raster = r2c,valData = vali2c,
                                  longitude = "longitude",latitude = "latitude",
                                  pres_abs = "presence_absence",optim_by = "tss",
                                  th_range = c(0.005,1),step = 0.005)
### creamos el modelo con el maximo valor
maximo_thre_MVE_full<-tss_threshold2c$threshold[1] #### el valor maximo de threshols te 
modelo_binario_MVE_full<- r2c >= maximo_thre_MVE_full ### se escogio el mejor threshold posible que comparten los tres

modelo_binario_MVE_full[modelo_binario_MVE_full==0] <- NA
polygono_cortar<-as.polygons(rast(modelo_binario_MVE_full))
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "modelo_binario_MVE_full.shp")

###################################################################################################
###### guardamos la informacion mas importante ####################################################
###################################################################################################
#pROC de Maxent####

####introduced
pROC_general_maxent_introducido<-data.frame("Prediction_partial_AUC"=partial_roc_maxent_introducido$pROC_results$Prediction_partial_AUC,
                                            "Random_curve_partial_AUC"=partial_roc_maxent_introducido$pROC_results$Random_curve_partial_AUC,
                                            "AUC_ratio"=partial_roc_maxent_introducido$pROC_results$AUC_ratio)
pROC_summary_maxent_introducido<-data.frame(partial_roc_maxent_introducido$pROC_summary)
write.csv(pROC_general_maxent_introducido, "pROC_general_maxent_introducido.csv")
write.csv(pROC_summary_maxent_introducido, "pROC_summary_maxent_introducido.csv")

#### paleartico
pROC_general_maxent_paleartico<-data.frame("Prediction_partial_AUC"=partial_roc_maxent_paleartico$pROC_results$Prediction_partial_AUC,
                                            "Random_curve_partial_AUC"=partial_roc_maxent_paleartico$pROC_results$Random_curve_partial_AUC,
                                            "AUC_ratio"=partial_roc_maxent_paleartico$pROC_results$AUC_ratio)
pROC_summary_maxent_paleartico<-data.frame(partial_roc_maxent_paleartico$pROC_summary)
write.csv(pROC_general_maxent_paleartico, "pROC_general_maxent_paleartico.csv")
write.csv(pROC_summary_maxent_paleartico, "pROC_summary_maxent_paleartico.csv")

#### full
pROC_general_maxent_completo<-data.frame("Prediction_partial_AUC"=partial_roc_maxent_full$pROC_results$Prediction_partial_AUC,
                                         "Random_curve_partial_AUC"=partial_roc_maxent_full$pROC_results$Random_curve_partial_AUC,
                                         "AUC_ratio"=partial_roc_maxent_full$pROC_results$AUC_ratio)
pROC_summary_maxent_completo<-data.frame(partial_roc_maxent_full$pROC_summary)
write.csv(pROC_general_maxent_completo, "pROC_general_maxent_full.csv")
write.csv(pROC_summary_maxent_completo, "pROC_summary_maxent_full.csv")



#pROC de MVE#########
###### introduced
pROC_general_MVE_introduced<-data.frame("Prediction_partial_AUC"=partial_roc_MVE_introduced$pROC_results$Prediction_partial_AUC,
                             "Random_curve_partial_AUC"=partial_roc_MVE_introduced$pROC_results$Random_curve_partial_AUC,
                             "AUC_ratio"=partial_roc_MVE_introduced$pROC_results$AUC_ratio)
pROC_summary_MVE_introduced<-data.frame(partial_roc_MVE_introduced$pROC_summary)
write.csv(pROC_general_MVE_introduced, "pROC_general_MVE_introduced.csv")
write.csv(pROC_summary_MVE_introduced, "pROC_summary_MVE_introduced.csv")
###### paleartico
pROC_general_MVE_paleartic<-data.frame("Prediction_partial_AUC"=partial_roc_MVE_paleartic$pROC_results$Prediction_partial_AUC,
                                        "Random_curve_partial_AUC"=partial_roc_MVE_paleartic$pROC_results$Random_curve_partial_AUC,
                                        "AUC_ratio"=partial_roc_MVE_paleartic$pROC_results$AUC_ratio)
pROC_summary_MVE_paleartic<-data.frame(partial_roc_MVE_paleartic$pROC_summary)
write.csv(pROC_general_MVE_paleartic, "pROC_general_MVE_paleartic.csv")
write.csv(pROC_summary_MVE_paleartic, "pROC_summary_MVE_paleartic.csv")
###### all
pROC_general_MVE_full<-data.frame("Prediction_partial_AUC"=partial_roc_MVE_full$pROC_results$Prediction_partial_AUC,
                                        "Random_curve_partial_AUC"=partial_roc_MVE_full$pROC_results$Random_curve_partial_AUC,
                                        "AUC_ratio"=partial_roc_MVE_full$pROC_results$AUC_ratio)
pROC_summary_MVE_full<-data.frame(partial_roc_MVE_full$pROC_summary)
write.csv(pROC_general_MVE_full, "pROC_general_MVE_full.csv")
write.csv(pROC_summary_MVE_full, "pROC_summary_MVE_full.csv")



##########################################################################################################
#### guardamos la informacion necesarias de los mapas binarios de las evaluaciones########################
###########################################################################################################
# para el maxent
# para el introduced
writeRaster(modelo_binario_maxent_calibrado_introducido, filename = "modelo_binario_maxent_calibrado_introducido.tif", format = "GTiff", overwrite=TRUE)
# para el paleartico
writeRaster(modelo_binario_maxent_calibrado_paleartico, filename = "modelo_binario_maxent_calibrado_paleartico.tif", format = "GTiff", overwrite=TRUE)
# para all
writeRaster(modelo_binario_maxent_calibrado_full, filename = "modelo_binario_maxent_calibrado_full.tif", format = "GTiff", overwrite=TRUE)


# para el MVE
# para introduced
writeRaster(modelo_binario_MV_introducedE, filename = "binario_MVE_introduced.tif", format = "GTiff", overwrite=TRUE)
# para paleartico
writeRaster(modelo_binario_MVE_paleartico, filename = "binario_MVE_paleartic.tif", format = "GTiff", overwrite=TRUE)
# para all
writeRaster(modelo_binario_MVE_full, filename = "binario_MVE_full.tif", format = "GTiff", overwrite=TRUE)
##### para risk
modelo_binario_MVE_risk<- r2c >= 0.9 ### se escogio el mejor threshold posible que comparten los tres
writeRaster(modelo_binario_MVE_risk, filename = "binario_MVE_risk.tif", format = "GTiff", overwrite=TRUE)




### guardamos TSS obtenidos y CRR obtenidos en el analisis
# para el maxent
write.csv(tss_threshold_1a, "tss_threshold_maxent_introduced.csv")
write.csv(tss_threshold_1b, "tss_threshold_maxent_paleartico.csv")
write.csv(tss_threshold_1c, "tss_threshold_maxent_full.csv")

# para el MVE
# para 
write.csv(tss_threshold2_introduced, "tss_threshold_MVE_introduced.csv")
write.csv(tss_threshold2b, "tss_threshold_MVE_paleartic.csv")
write.csv(tss_threshold2c, "tss_threshold_MVE_all.csv")



############################################################################################
########## PARA LOS PROYECTADOS HACEMOS LOS TRES HOLD QUE SE CUMPLAN EN LOS TRES PARA LOS DIBUJOS###
introducido_world=raster("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Modelos_Maxent/Maxent_introduced/Modelos/L05/modelo_presente.tif")
paleartico_world=raster("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Modelos_Maxent/Maxent_native/Modelos/LQ05/modelo_presente.tif")
all_world=raster("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Modelos_Maxent/Maxent_complete/Modelos/LQ05/modelo_presente.tif")

# para introducido
modelo_binario_maxent_introducido_world<- introducido_world >=tss_threshold_1a$threshold[1] ### se escogio el mejor threshold posible que comparten los tres
writeRaster(modelo_binario_maxent_introducido_world, filename = "modelo_binario_maxent_introducido_world.tif", format = "GTiff", overwrite=TRUE)

# para paleartico
modelo_binario_maxent_paleartico_world<- paleartico_world >=tss_threshold_1b$threshold[1] ### se escogio el mejor threshold posible que comparten los tres
writeRaster(modelo_binario_maxent_paleartico_world, filename = "modelo_binario_maxent_paleartico_world.tif", format = "GTiff", overwrite=TRUE)

# para all
modelo_binario_maxent_all_world<- all_world >=tss_threshold_1c$threshold[1] ### se escogio el mejor threshold posible que comparten los tres
writeRaster(modelo_binario_maxent_all_world, filename = "modelo_binario_maxent_all_world.tif", format = "GTiff", overwrite=TRUE)

modelo_binario_maxent_risk<- all_world >= 0.9 ### se escogio el mejor threshold posible que comparten los tres
writeRaster(modelo_binario_maxent_risk, filename = "modelo_binario_maxent_risk_0.9.tif", format = "GTiff", overwrite=TRUE)



#####################################################################################################################################################
############################################### SI SE TIENE UN THRESHOLD DADO Y SE QUIERE MODELAR NUEVAMENTE TODO ##################
#####################################################################################################################################################



##### para poder evaluar los modelos bajos estos dos parametros se deben utilizar
#a) modelos binarizados
##### necesitamos volver los datos a mapas binarios para poder analizarlos como TSS o CRS
#### para el modelo creado en MVE ###############################################################
binario_Maxent <- bin_model(r,c,percent = 0) #### minimun training presence ### ntbox
#### para el modelo maxent #####################################################################
binario_MVE <- bin_model(r2a,c2a,percent = 0) #### minimun training presence  ### ntbox
#### se les esta calculando de esa dorma para poder mas adelante sacar mas cosas diferentes

######################################################################################
################ otra forma de crear modelos binarizados con el miminim threshold#####
######################################################################################
vector_MVE<-vect(c2b, geom=c("longitude","latitude"))
bio<-raster::extract(rast(r2b),vector_MVE)
colnames(bio)=c("a", "b")
thres<-min(bio$b)

##### minimun thres hold
# introduced
binario_MVE_introduced_the<-r2a>=0.00
# paleartico
binario_MVE_paleartico_the<-r2c>=0.00
#full
binario_MVE_full_the<-r2c>=0.00

writeRaster(binario_MVE_introduced_the, filename = "binario_MVE_introduced_the.tif", format = "GTiff", overwrite=TRUE)
writeRaster(binario_MVE_paleartico_the, filename = "binario_MVE_paleartico_the.tif", format = "GTiff", overwrite=TRUE)
writeRaster(binario_MVE_full_the, filename = "binario_MVE_full_the.tif", format = "GTiff", overwrite=TRUE)


binario_MVE<-binario_MVE_introduced_the
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
presencias2=raster::extract(binario_MVE,c2a)

a2=sum(data.frame(values=presencias2)$values==1)    #### presencias que fueron detectadas como presencias
b2=sum(data.frame(values=presencias2)$values==0)    #### presencias que fueron detectadas como ausencias
c2=sum(data.frame(values=absvals2)$values==1)    #### ausencias que fueron detectadas como presencias
d2=sum(data.frame(values=absvals2)$values==0)    #### ausnecias que fueron detectadas como ausencias

sens2<-sensibilidad(a2,c2)  
esp2<-especificidad(b2,d2)
kap2<-kappa(a2,b2,c2,d2)
ts2<-tss(a2,b2,c2,d2)

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
  
  

###### nota para combertir los modelos generados en simplemente formato shapefile donde solo se consideren ambos tipos de datos
#### para los modelos en maxent tomamos los suitability creados
### america
a<-raster("D:/0001_JAC_monosteira/Modelos_Maxent/Evaluation_general/modelo_binario_maxent_introducido_world.tif")
a[a==0] <- NA
polygono_cortar<-as.polygons(rast(a))
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "modelo_binario_maxent_introducido_world.shp")
### Natural
a<-raster("D:/0001_JAC_monosteira/Modelos_Maxent/Evaluation_general/modelo_binario_maxent_paleartico_world.tif")
a[a==0] <- NA
polygono_cortar<-as.polygons(rast(a))
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "modelo_binario_maxent_paleartico_world.shp")
### Full
a<-raster("D:/0001_JAC_monosteira/Modelos_Maxent/Evaluation_general/modelo_binario_maxent_all_world.tif")
a[a==0] <- NA
polygono_cortar<-as.polygons(rast(a))
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "modelo_binario_maxent_all_world.shp")



