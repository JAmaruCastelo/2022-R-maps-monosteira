##### calculamos el risk area en base a los resultados obtenidos en los otros mapas

library ("easypackages")
libraries (c("dismo", "maps", "maptools", "rgdal", "rgeos", "sp", "virtualspecies", "corrplot", 
             "raster", "usdm", "gtools", "tidyverse", "rJava", "ENMeval", "rgbif",
             "ENMTools", "geodata", "devtools", "RStoolbox", "glue", "rmaxent", "ENMeval",
             "sf", "xlsx", "rgl"))
library(ellipsenm)
library(ntbox)

#### obtenemos los modelos proyectados de todo el mundo para el presente
### para maxent
path="D:/0001_JAC_monosteira/Modelos_Maxent/Maxent_complete/Modelos/LQ05/modelo_presente.tif"
maxent_proyectado<-raster(path)

### para MVE
path2="D:/0001_JAC_monosteira/Minimun Volume Ellipsoid NicheA/point/continuos/total_records_clean_nichea.tiff"
MVE_proyectado<-raster(path2)

##### cuando solo se hacen la multiplicacion #### para promediar los datos
### otra forma de encontrar los risk map que no se realizaron en el otro articulo
a<-maxent_proyectado*MVE_proyectado
binario<- a>=0
final<- a*binario
writeRaster(final, filename="RiskMap_Multiplication.tif", format="GTiff")

########################################################################
a<-(maxent_proyectado+MVE_proyectado)/2 #para solo promediar que tambien se podria usar



##############################################################################
################LEEMOS LA INFORMACION DEL THRESHOLD ##########################
##############################################################################
# para Maxent
thre1=read.csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Modelos_evaluation/tss_threshold_maxent.csv")
corte1=thre1$threshold[1]
corte1=0.9
modelo_binario_maxent<- maxent_proyectado >= corte1
#poligono <- rasterToPolygons(modelo_binario_maxent, fun=function(x){x==1})  #### cortamos solo los que tengan valores de 1
##### para normalizar de 0 a 1 (value-min)/(maximo-minimo)
#### el minimo es el threshold
minimo1=corte1
maximo1=maxent_proyectado@data@max
denominador1=maximo1-minimo1
resta1=maxent_proyectado-minimo1
maxent_risk=resta1/denominador1
#corto los valores del raster que son menores
maxent_risk_no_threshold<-maxent_risk*modelo_binario_maxent


#### para MVE
thre2=read.csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Modelos_evaluation/tss_threshold_maxent_MVE.csv")
corte2=thre2$threshold[1]
corte2<- 0.9
modelo_binario_MVE<- MVE_proyectado >= corte2
# poligono2 <- rasterToPolygons(modelo_binario_MVE, fun=function(x){x==1})  #### cortamos solo los que tengan valores de 1
# Normalizo los datos de 
minimo2=corte2
maximo2=MVE_proyectado@data@max
denominador2=maximo2-minimo2
resta2=MVE_proyectado-minimo2
MVE_risk=resta2/denominador2

# corto los valores del raster que son menores
MVE_risk_no_threshold<-MVE_risk*modelo_binario_MVE



##### risk total multiplication
map_risk_thre_ca<-maxent_risk_no_threshold*MVE_risk_no_threshold

#### risk total mean
mean_risk_thre_ca<-maxent_risk_no_threshold+MVE_risk_no_threshold
mean_risk_thre_ca<-mean_risk_thre_ca/2

##### guardamos los dos mapas para analizarlos
setwd("D:/0001_JAC_monosteira/Risk_Maps")

### escribir raster de cada uno de las variables
writeRaster(map_risk_thre_ca, filename="risk_map_product.tif", format="GTiff")
writeRaster(mean_risk_thre_ca, filename="risk_map_mean.tif", format="GTiff")

#### guardamos los modelosbinarios
writeRaster(modelo_binario_maxent, filename="binario_maxent.tif", format="GTiff")
writeRaster(modelo_binario_MVE, filename="binario_MVE.tif", format="GTiff")

