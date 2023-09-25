#########################################################################################################################
###### MODELOS en MAXENT ################################################################################################
#########################################################################################################################

##### librerias que se estan utilizando
library ("easypackages")
libraries (c("dismo", "maps", "maptools", "rgdal", "rgeos", "sp", "virtualspecies", "corrplot", 
           "raster", "usdm", "gtools", "tidyverse", "rJava", "ENMeval", "rgbif",
           "ENMTools", "geodata", "devtools", "RStoolbox", "glue", "rmaxent", "ENMeval",
           "sf", "xlsx", "rgl"))


#### leemos las capas medioambientales a utilizar para cada uno de los archivos
directory <- "C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Modelos_Maxent/capas_medioamb_5"
files<-list.files(directory,full.names=TRUE )
world<-stack(files) 



##### Seleccionamos los raster que se van a usar en las proyecciones
### para el presente
rasters<- worldclim_global("bio", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")
#### para el future
rastersa<- cmip6_world(model="BCC-CSM2-MR", ssp="126",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")
rastersb<- cmip6_world(model="CMCC-ESM2", ssp="126",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")
rastersc<- cmip6_world(model="MIROC6", ssp="126",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")
rastersd<- cmip6_world(model="MPI-ESM1-2-LR", ssp="126",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")
rasterse<- cmip6_world(model="GISS-E2-1-G", ssp="126",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")

rastersa2<- cmip6_world(model="BCC-CSM2-MR", ssp="585",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")
rastersb2<- cmip6_world(model="CMCC-ESM2", ssp="585",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")
rastersc2<- cmip6_world(model="MIROC6", ssp="585",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")
rastersd2<- cmip6_world(model="MPI-ESM1-2-LR", ssp="585",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")
rasterse2<- cmip6_world(model="GISS-E2-1-G", ssp="585",var="bioc",time="2021-2040", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")



#### seleccionamos los registros puntuales almacenados
#### los valores puntuales
latitud_longitud<-read_csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/nueva_calcular/Monosteira_unicostata_registros_puntuales_data.csv")
latitud_longitud<-latitud_longitud[c("long","lat")] 



###### creamos los modelos con enmtools
#### guardamos a las especies necesarias
monosteira<-enmtools.species()
monosteira$species.name<-"monosteira unicostata" ##### este nombre se puede guardar segun se requiera
monosteira$presence.points<-vect(latitud_longitud, geom=c("long","lat")) #### se almacena la informacion de presencia en los puntos
crs(monosteira$presence.points)<-crs(rasters) ##### se define la proyeccion necesaria
monosteira$range<-background.raster.buffer(monosteira$presence.points, 500000, mask=rasters) ##### crea un raster de buffer de 5000 metros
monosteira$background.points<-background.points.buffer(monosteira$presence.points, 
                                                       radius=50000, n= 1000, 
                                                       mask=rasters$wc2.1_5m_bio_1) ##### crea un un buffer points


setwd("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Mapas para guardar")
#interactive.plot(monosteira) #plot iteractivo para mostrar cada uno de los datos
######creamos el poligono para cortar y cortamos las variables climaticas a usar
polygono_cortar<-as.polygons(monosteira$range)


#####guardamos el poligono para cortar
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "range_paleartico.shp")



### corto las variables climaticas necesarias para hacer el pca
raster.crop <- crop(rasters, polygono_cortar)
raster.crop  <- mask(raster.crop , polygono_cortar) #### en alli se ven los cortes mas detallados de cada una de las variables




#### realizo el PcA de mis datos para poder hacer el analisis de maxent
pca1<-raster.pca(raster.crop, 4) #### realizo el PCa y obtengo las primero 4 componentes principales para hacer als predicciones

#### guardo informacion de las capas de Pca1
writeRaster(pca1$rasters$PC1, filename = "PC1.tif", format = "GTiff", overwrite=TRUE)
writeRaster(pca1$rasters$PC2, filename = "PC2.tif", format = "GTiff", overwrite=TRUE)
writeRaster(pca1$rasters$PC3, filename = "PC3.tif", format = "GTiff", overwrite=TRUE)
writeRaster(pca1$rasters$PC4, filename = "PC4.tif", format = "GTiff", overwrite=TRUE)

##### calculo informacion necesaria del PCA para tenerlo anotado en alguna base de datos
carpeta_guardar="C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/PCA/Proporcion de PCA de Rstudio"
setwd(carpeta_guardar)

prop_varianza <- pca1$pca.object$sdev^2 / sum(pca1$pca.object$sdev^2)
informacion_pca<- data.frame("desviacion estandar"=pca1$pca.object$sdev, 
                             "prop_varianza"=prop_varianza, "varianza_acumulada"=cumsum(prop_varianza))
write.xlsx(informacion_pca, "informacion_pca.xlsx")
informacion_pca_rotation <- data.frame(pca1$pca.object$rotation)
write.xlsx(informacion_pca_rotation, "rotation_pca.xlsx")


#################################################################################################
#### proyecto el PCA realizado para los cortes actuales y para las capas del futuro #############
#################################################################################################

#### proyectamos para el mundo con las configuraciones del presente
world_PCA_actual<-raster::predict(rasters, pca1$pca.object)

#### proyecto para el futuro con el pca Corrido para el pasado
### debo setear los nombres de las capas porque o si no te sale error y no te permite correr las siguientes partes
namesa<-c("wc2.1_5m_bio_1","wc2.1_5m_bio_2","wc2.1_5m_bio_3","wc2.1_5m_bio_4","wc2.1_5m_bio_5",
         "wc2.1_5m_bio_6","wc2.1_5m_bio_7","wc2.1_5m_bio_8","wc2.1_5m_bio_9",
         "wc2.1_5m_bio_10","wc2.1_5m_bio_11","wc2.1_5m_bio_12","wc2.1_5m_bio_13",
         "wc2.1_5m_bio_14","wc2.1_5m_bio_15","wc2.1_5m_bio_16",
         "wc2.1_5m_bio_17","wc2.1_5m_bio_18","wc2.1_5m_bio_19")

names(rastersa) <-namesa
world_PCA_futuro_BCC_CSM2_MR_126 <- raster::predict(rastersa, pca1$pca.object)
names(rastersb) <-namesa
world_PCA_futuro_CMCC_ESM2_126 <- raster::predict(rastersb, pca1$pca.object)
names(rastersc) <-namesa
world_PCA_futuro_MIROC6_126 <- raster::predict(rastersc, pca1$pca.object)
names(rastersd) <-namesa
world_PCA_futuro_MPI_ESM1_2_LR_126 <- raster::predict(rastersd, pca1$pca.object)
names(rasterse) <-namesa
world_PCA_futuro_GISS_E2_1_G_126 <- raster::predict(rasterse, pca1$pca.object)
names(rastersa2) <-namesa
world_PCA_futuro_BCC_CSM2_MR_585 <- raster::predict(rastersa2, pca1$pca.object)
names(rastersb2) <-namesa
world_PCA_futuro_CMCC_ESM2_585 <- raster::predict(rastersb2, pca1$pca.object)
names(rastersc2) <-namesa
world_PCA_futuro_MIROC6_585 <- raster::predict(rastersc2, pca1$pca.object)
names(rastersd2) <-namesa
world_PCA_futuro_MPI_ESM1_2_LR_585 <- raster::predict(rastersd2, pca1$pca.object)
names(rasterse2) <-namesa
world_PCA_futuro_GISS_E2_1_G_585 <- raster::predict(rasterse2, pca1$pca.object)





##############################################################################################################################
################## REALIZAMOS LOS MODELOS CON DIFERENTES PARAMETROS Y VALUES EN CADA UNA DE LAS VARIABLES ####################
##############################################################################################################################

set.seed (10000)
path_w="C:/Users/amaru/OneDrive/Escritorio/prueba/"
monosteira<-monosteira
#### correr la otra hoja de R studio #modelos_auto.R para que tengas los modelos maxent corridos modelos_auto para que se apoyen de mejor manera
#### modelos_auto.R tambien se puede cambiar el path donde se desea guardar

##### para el modelo mejor evaluado se deberia colocar make jacknife and curve responses, para poder observar estos dos estadisticos dentro de maxent y ver de mejor manera como funciona este


####################### EVALUATION #####################################################################
####### para comenzar con esta parte se debe leer el R con las funciones de Maxent (funciones_maxent.R)##############
##### realizamos la evaluacion de los modelos generados para elegir el que cumpla mejores las caracteristicas
###########################################################################################
lista<-list.files(path_w, full.names = TRUE)
latitud_longitud #### es el df con las longitudes y latitudes
data<-calculate (lista, latitud_longitud)
data<- data[order(data$AICc_score), ] #### ordenamos la data obtenida

###
setwd("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Mapas para guardar")
write.xlsx(data, "calculate_data.xlsx")






##########################################################################################################
####################### PROYECCION A OTRAS CAPAS AMBIENTALES #############################################
##########################################################################################################
#### ver como automatiza el paso para que se seleccione uno por uno cada una de las especies proyectadas
maximo="C:/Users/amaru/OneDrive/Escritorio/prueba/LQ05"
setwd(maximo)#### toda las capas de proyecciones se van a guardar en la misma carpeta del modelo estudiado
#########################################################################################################
####### leemos el modelo maxent realizado y obtenido con los mejores resultados en el analisis###########
#########################################################################################################
modelo_maxent=import_maxent(maximo)


modelo_presente<-dismo::predict(object=modelo_maxent, x=pca1$rasters )
writeRaster(raster(modelo_presente), filename = "modelo_presente_paleartico.tif", format = "GTiff", overwrite=TRUE)

#########################################################################################################
########## predecimos para diferentes partes del mundo y para el futuro ##################################
##########################################################################################################
#### predecimos apra el presente
modelo_presente<-dismo::predict(object=modelo_maxent, x=world_PCA_actual )
writeRaster(raster(modelo_presente), filename = "modelo_presente.tif", format = "GTiff", overwrite=TRUE)

setwd(maximo) #### para que los archivos se guarden en la carpeta del modelo original

#### predecimos para el futuro
modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_BCC_CSM2_MR_126 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_BCC_CSM2_MR_126.tif", format = "GTiff", overwrite=TRUE)

modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_CMCC_ESM2_126 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_CMCC_ESM2_126.tif", format = "GTiff", overwrite=TRUE)

modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_MIROC6_126 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_MIROC6_126.tif", format = "GTiff", overwrite=TRUE)

modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_MPI_ESM1_2_LR_126 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_MPI_ESM1_2_LR_126.tif", format = "GTiff", overwrite=TRUE)

modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_GISS_E2_1_G_126 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_GISS_E2_1_G_126.tif", format = "GTiff", overwrite=TRUE)

modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_BCC_CSM2_MR_585 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_CSM2_MR_585.tif", format = "GTiff", overwrite=TRUE)

modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_CMCC_ESM2_585 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_CMCC_ESM2_585.tif", format = "GTiff", overwrite=TRUE)

modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_MIROC6_585 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_MIROC6_585.tif", format = "GTiff", overwrite=TRUE)

modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_MPI_ESM1_2_LR_585 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_MPI_ESM1_2_LR_585.tif", format = "GTiff", overwrite=TRUE)

modelo_proyectado<- dismo::predict(object=modelo_maxent, x=world_PCA_futuro_GISS_E2_1_G_585 )
writeRaster(raster(modelo_proyectado), filename = "modelo_futuro_GISS_E2_1_G_585.tif", format = "GTiff", overwrite=TRUE)



