#########################################################################################################################
###### MODELOS en MAXENT ################################################################################################
#########################################################################################################################

##### librerias que se estan utilizando
library ("easypackages")
libraries (c("dismo", "maps", "maptools", "rgdal", "rgeos", "sp", "virtualspecies", "corrplot", 
           "raster", "usdm", "gtools", "tidyverse", "rJava", "ENMeval", "rgbif",
           "ENMTools", "geodata", "devtools", "RStoolbox", "glue", "rmaxent", "ENMeval",
           "sf", "xlsx", "rgl"))

#### seleccionamos la carpeta para guardar toda la información
path="D:/0001_JAC_monosteira/Modelos_Maxent/Maxent_introduced"
setwd(path)
##### Seleccionamos los raster que se van a usar en las proyecciones
### para el presente
rasters<- worldclim_global("bio", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")



#### seleccionamos los registros puntuales almacenados
#### los valores puntuales
registros_puntuales="D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/introduced_clean.csv"

latitud_longitud<-read_csv(registros_puntuales)
latitud_longitud<-latitud_longitud[c("Longitud","Latitud")] 
colnames(latitud_longitud)<-c("long", "lat")


###### creamos los modelos con enmtools
#### guardamos a las especies necesarias
monosteira<-enmtools.species()
monosteira$species.name<-"monosteira unicostata" ##### este nombre se puede guardar segun se requiera
monosteira$presence.points<-vect(latitud_longitud, geom=c("long","lat")) #### se almacena la informacion de presencia en los puntos
crs(monosteira$presence.points)<-crs(rasters) ##### se define la proyeccion necesaria
monosteira$range<-background.raster.buffer(monosteira$presence.points, 500000, mask=rasters) ##### crea un raster de buffer de 5000 metros
monosteira$background.points<-background.points.buffer(monosteira$presence.points, 
                                                       radius=500000, n= 1000, 
                                                       mask=rasters$wc2.1_5m_bio_1) ##### crea un un buffer points



#interactive.plot(monosteira) #plot iteractivo para mostrar cada uno de los datos
######creamos el poligono para cortar y cortamos las variables climaticas a usar
polygono_cortar<-as.polygons(monosteira$range)


#####guardamos el poligono para cortar
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "range.shp")



### corto las variables climaticas necesarias para hacer el pca
raster.crop <- crop(rasters, polygono_cortar)
raster.crop  <- mask(raster.crop , polygono_cortar) #### en alli se ven los cortes mas detallados de cada una de las variables



#### realizo el PcA de mis datos para poder hacer el analisis de maxent
pca1<-raster.pca(raster.crop, 4) #### realizo el PCa y obtengo las primero 4 componentes principales para hacer als predicciones

#### guardo informacion de las capas de Pca1
writeRaster(pca1$rasters$PC1, filename = "PC1.tif", overwrite=TRUE)
writeRaster(pca1$rasters$PC2, filename = "PC2.tif", overwrite=TRUE)
writeRaster(pca1$rasters$PC3, filename = "PC3.tif", overwrite=TRUE)
writeRaster(pca1$rasters$PC4, filename = "PC4.tif", overwrite=TRUE)

##### calculo informacion necesaria del PCA para tenerlo anotado en alguna base de datos

prop_varianza <- pca1$pca.object$sdev^2 / sum(pca1$pca.object$sdev^2)
informacion_pca<- data.frame("desviacion estandar"=pca1$pca.object$sdev, 
                             "prop_varianza"=prop_varianza, "varianza_acumulada"=cumsum(prop_varianza))
write.xlsx(informacion_pca, "informacion_pca.xlsx")
informacion_pca_rotation <- data.frame(pca1$pca.object$rotation)
write.xlsx(informacion_pca_rotation, "rotation_pca.xlsx")


##############################################################################################################################
################## REALIZAMOS LOS MODELOS CON DIFERENTES PARAMETROS Y VALUES EN CADA UNA DE LAS VARIABLES ####################
##############################################################################################################################
set.seed (10000)
monosteira<-monosteira
#### correr la otra hoja de R studio #modelos_auto.R para que tengas los modelos maxent corridos modelos_auto para que se apoyen de mejor manera
#### modelos_auto.R tambien se puede cambiar el path donde se desea guardar

####### NOTA PARA EL MEJOR MDELO SE PUEDE VOLVER A CORRER Y COLOCAR JACKNIFE TRUE Y CURVES RESPONSES TRUE
##### para el modelo mejor evaluado se deberia colocar make jacknife and curve responses, para poder observar estos dos estadisticos dentro de maxent y ver de mejor manera como funciona este


#### path donde estan guardadas los paths#########################################################################
##################################################################################################################
path_w="C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Modelos_Maxent/Maxent_introduced/Modelos"
##################################################################################################################

####################### EVALUATION y SELECCIONAR EL MEJOR #####################################################################
####### para comenzar con esta parte se debe leer el R con las funciones de Maxent (funciones_maxent.R)##############
##### realizamos la evaluacion de los modelos generados para elegir el que cumpla mejores las caracteristicas
###########################################################################################
lista<-list.files(path_w, full.names = TRUE)
latitud_longitud #### es el df con las longitudes y latitudes
data<-calculate (lista, latitud_longitud)
data<- data[order(data$AICc_score), ] #### ordenamos la data obtenida

############################
write.xlsx(data, "calculate_data.xlsx")



###### luego se debe continuar con las proyecciones a los diferentes modelos





