################################################################################################################################
########################################### ANALISIS AMBIENTAL DE CADA VARIABLE PARA VER CUAL ES LA DIFERENTE ##################
################################################################################################################################
library ("easypackages")
libraries (c("dismo", "maps", "maptools", "rgdal", "rgeos", "sp", "virtualspecies", "corrplot", 
             "raster", "usdm", "gtools", "tidyverse", "rJava", "ENMeval", "rgbif",
             "ENMTools", "geodata", "devtools", "RStoolbox", "glue", "rmaxent", "ENMeval",
             "sf", "xlsx", "rgl"))

########### VARIABLES CLIMATICAS A UTILIZARSE EN LA COMPARACION DE CADA UNO DE LOS PUNTOS ##################

rasters<- worldclim_global("bio", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")

#########################################################################################################
### ABRIMOS LOS DATOS CLIMATICOS DE LAS LOCALIDADES QUE SE VAN A COMPARAR ################################
#####################################################################################################
##### PUNTOS EN LA REGION PALEARTICA
latitud_longitud<-read_csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/nueva_calcular/Monosteira_unicostata_registros_puntuales_data.csv")
latitud_longitud<-latitud_longitud[c("long","lat")] 
vector_palea<-vect(latitud_longitud, geom=c("long","lat"))


##### PUNTOS EN AMERICA LATINA ###########################################################
latitud_longitud2<-read_csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/puntos_externos.csv")
latitud_longitud2<-latitud_longitud2[c("longitude","latitude")] 
vector<-vect(latitud_longitud2, geom=c("longitude","latitude"))

##### analisis de boxplot de cada variable ambiental #################################################
#### para detectar en que variable es la que esta cambiando el rango original de no original#####
##################################################################################################
path="C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Boxplot_variables"
setwd(path)

#### primero se debe setar donde se quiere guardar cada una de las variables necesarias

for (e in names(rasters)){
  name=e
  bio_1<-raster::extract(rasters[[name]],monosteira$presence.points)
  bio_1b<-raster::extract(rasters[[name]],vector)
  df<-data.frame("paleartico"=bio_1[name])
  colnames(df)<-"Paleartico"
  stacked_df <- stack(df)
  df2<-data.frame("introduced"=bio_1b[name])
  colnames(df2)<-"Introduced"
  stacked_df2 <- stack(df2)
  DatosTotal = rbind(stacked_df, stacked_df2 )
  if (e=="wc2.1_5m_bio_1"){
    namep="BIO1:Annual Mean Temperature"
    name2="Temperature (°c)"}else if (e=="wc2.1_5m_bio_2"){
      namep="BIO2:Mean Diurnal Range (Mean of monthly (max t°- min t°))"
      name2="values"}else if (e=="wc2.1_5m_bio_3"){
        namep="BIO3:Isothermality (BIO2/BIO7) (×100)"
        name2="values"}else if (e=="wc2.1_5m_bio_4"){
          namep="BIO4:Temp. Seasonality (standard deviation ×100)"
          name2="values"}else if (e=="wc2.1_5m_bio_5"){
            namep="BIO5:Max Temperature of Warmest Month"
            name2="Temperature (°c)"}else if (e=="wc2.1_5m_bio_6"){
              namep="BIO6:Min Temperature of Coldest Month"
              name2="Temperature (°c)"}else if (e=="wc2.1_5m_bio_7"){
                namep="BIO07:Temp. Annual Range (BIO5-BIO6)"
                name2="Temperature (°c)"}else if (e=="wc2.1_5m_bio_8"){
                  namep="BIO8:Mean Temp. of Wettest Quarter"
                  name2="Temperature (°c)"}else if (e=="wc2.1_5m_bio_9"){
                    namep="BIO9:Mean Temp. of Driest Quarter"
                    name2="Temperature (°c)"}else if (e=="wc2.1_5m_bio_10"){
                      namep="BIO10:Mean Temp. of Warmest Quarter"
                      name2="Temperature (°c)"}else if (e=="wc2.1_5m_bio_11"){
                        namep="BIO11:Mean Temp. of Coldest Quarter"
                        name2="Temperature (°c)"}else if (e=="wc2.1_5m_bio_12"){
                          namep="BIO12:Annual Precipitation"
                          name2="Precip (mm)"}else if (e=="wc2.1_5m_bio_13"){
                            namep="BIO13:Precip.of Wettest Month"
                            name2="Precip. (mm)"}else if (e=="wc2.1_5m_bio_14"){
                              namep="BIO14:Precip. of Driest Month"
                              name2="Precip (mm)"}else if (e=="wc2.1_5m_bio_15"){
                                namep="BIO15:Precip. Seasonality"
                                name2="values"}else if (e=="wc2.1_5m_bio_16"){
                                  namep="BIO16:Precip. of Wettest Quarter"
                                  name2="Precip (mm)"}else if (e=="wc2.1_5m_bio_17"){
                                    namep="BIO17:Precip. of Driest Quarter"
                                    name2="Precip (mm)"}else if (e=="wc2.1_5m_bio_18"){
                                      namep="BIO18:Precip. of Warmest Quarter"
                                      name2="Precip (mm)"}else if (e=="wc2.1_5m_bio_19"){
                                        namep="BIO19:Precip. of Coldest Quarter"
                                        name2="Precip (mm)"}
  windowsFonts(A = windowsFont("Times New Roman"))
  boxplot(DatosTotal$values ~ DatosTotal$ind, xlab=namep, ylab=name2,family = "A")}

####### se debe guardar manualmente cada una de las imagenes, aun no se tiene el codigo que te permita guardar automaticament cada una de las variables de estudio