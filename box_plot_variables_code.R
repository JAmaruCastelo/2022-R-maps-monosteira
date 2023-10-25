################################################################################################################################
########################################### ANALISIS AMBIENTAL DE CADA VARIABLE PARA VER CUAL ES LA DIFERENTE ##################
################################################################################################################################
library ("easypackages")
libraries (c("dismo", "maps", "maptools", "rgdal", "rgeos", "sp", "virtualspecies", "corrplot", 
             "raster", "usdm", "gtools", "tidyverse", "rJava", "ENMeval", "rgbif",
             "ENMTools", "geodata", "devtools", "RStoolbox", "glue", "rmaxent", "ENMeval",
             "sf", "xlsx", "rgl"))
library ("stats")
########### VARIABLES CLIMATICAS A UTILIZARSE EN LA COMPARACION DE CADA UNO DE LOS PUNTOS ##################

rasters<- worldclim_global("bio", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")

#########################################################################################################
### ABRIMOS LOS DATOS CLIMATICOS DE LAS LOCALIDADES QUE SE VAN A COMPARAR ################################
#####################################################################################################
##### PUNTOS EN LA REGION PALEARTICA
latitud_longitud<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/paleartico_clean.csv")
latitud_longitud<-latitud_longitud[c("Longitud","Latitud")] 
vector_palea<-vect(latitud_longitud, geom=c("Longitud","Latitud"))


##### PUNTOS EN AMERICA LATINA ###########################################################
latitud_longitud2<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/introduced_clean.csv")
latitud_longitud2<-latitud_longitud2[c("Longitud","Latitud")]
vector<-vect(latitud_longitud2, geom=c("Longitud","Latitud"))

##### analisis de boxplot de cada variable ambiental #################################################
#### para detectar en que variable es la que esta cambiando el rango original de no original#####
##################################################################################################
path="D:/0001_JAC_monosteira/Boxplot_variables"
setwd(path)

#### primero se debe setar donde se quiere guardar cada una de las variables necesarias
names=c()
statistic=c()
p_value=c()
for (e in names(rasters)){
  name=e
  names<-c(names,name)
  bio_1<-raster::extract(rasters[[name]],vector_palea)
  bio_1b<-raster::extract(rasters[[name]],vector)
  df<-data.frame("paleartico"=bio_1[name])
  colnames(df)<-"Paleartico"
  stacked_df <- stack(df)
  df2<-data.frame("introduced"=bio_1b[name])
  colnames(df2)<-"Introduced"
  wil<-wilcox.test (df$Paleartico, df2$Introduced)
  statistic<-c(statistic,wil$statistic)
  p_value=c(p_value,wil$p.value )
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

bb<-cbind(names, statistic, p_value)
write.csv(bb,"satistic.csv")

################################################################################################
######### para solo extraer los puntos y crear los graficos en Python #########################
#################################################################################################


mat = matrix(ncol = 1, nrow = 386)
natural=data.frame(mat)
mat2 = matrix(ncol = 1, nrow = 15)
introduced=data.frame(mat2)
for (e in names(rasters)){
  print (e)
  name=e
  bio_1<-raster::extract(rasters[[name]],vector_palea)
  bio_1<-bio_1[name]
  natural = cbind(natural, bio_1 )
  
  bio_1b<-raster::extract(rasters[[name]],vector)
  bio_1b<-bio_1b[name]
  introduced = cbind(introduced, bio_1b )
}

write.csv(natural,"natural.csv")
write.csv(introduced,"introduced.csv")

#### extraemos para el punto de ontario, para analizarlo mas adelante


ontario<-data.frame(c(-80.6278999999999),c(43.4018999999999))
colnames(ontario)<-c("Longitud","Latitud")
vector<-vect(ontario, geom=c("Longitud","Latitud"))
bio_ontario<-raster::extract(rasters,vector)

write.csv(bio_ontario,"bio_ontario.csv")

###### ver el codigo en python para ver si se puede jugar con este analisis y meter mas cosas.

####### se debe guardar manualmente cada una de las imagenes, aun no se tiene el codigo que te permita guardar automaticament cada una de las variables de estudio