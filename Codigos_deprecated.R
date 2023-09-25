#### funciones quitadas de la version de prueba de cada uno de los datos


############ prueba de ENMeval
occs.sf <- sf::st_as_sf(latitud_longitud, coords = c("longitude","latitude"), crs = raster::crs(rasters))
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
occs.sf <- sf::st_transform(occs.sf, crs = eckertIV)
occs.buf <- sf::st_buffer(occs.sf, dist = 500000) %>% sf::st_union() %>% 
  sf::st_sf() %>%sf::st_transform(crs = raster::crs(rasters))
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)
plot(occs.sf)
plot(occs.buf)




##### seleccionamos la capa con la se van a cortar el raster para realizar el modelo
p<-readOGR("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/GIS")


#### maxent con dismo

##### datos necesarios
presvals <- raster::extract (pca1$rasters, latitud_longitud )
set.seed (10000)
backgr <- randomPoints (pca1$rasters, 10000)
absvals <- raster::extract (pca1$rasters, backgr)
pb <- c(rep(1,nrow(presvals)), rep(0,nrow(absvals)))
sdmdata.present <- data.frame (cbind (pb, rbind(presvals, absvals)))

##########################################################################################
#### trabajar en esta parte del codigo####################################################
#### escribimos el modelo de maxent para suscribir los datos faltantes
linear<-c("false", "true")
quadratic<-c("false", "true")
product<-c("false", "true")
threshold<-c("false", "true")
hinge<-c("false", "true")
beta<-c("0.5", "1.0", "1,5", "2.5", "3.5")

model.maxent<-dismo::maxent(x=pca1$rasters, 
                            p=data.frame(latitud_longitud),
                            path="C:/Users/amaru/OneDrive/Escritorio/prueba/pp",
                            args=c('betamultiplier=0.5',
                                   'linear=true',
                                   'quadratic=true',
                                   'product=false',
                                   'threshold=false',
                                   'hinge=false'
                            ))
#### se deberia guardar en una lista que añada los nombres de cada una de las especies usadas
mi_vector <- 1:10
mi_matriz <- matrix(1:4, nrow = 2)
mi_df     <- data.frame("num" = 1:3, "let" = c("a", "b", "c"))

mi_lista <- list("un_vector" = mi_vector, "una_matriz" = mi_matriz, "un_df" = mi_df)

mi_lista
#### otra forma de calcular el modelo mas conveniente en el analisis en general
maxentIC <- function(pred.raw, occ, lambdas) {
  # pred.raw: A raster object or a file path to a raw maxent prediction grid.
  #           Providing a RasterStack will result in *IC being calculated for 
  #           each layer. 
  # occ: A matrix/data.frame with 2 columns (lon, lat). 
  # lambdas: A maxent object or a file path to a lambdas file.
  require(raster)
  if(is(lambdas, 'MaxEnt')) lambdas <- textConnection(lambdas@lambdas)
  if(is.character(pred.raw)) pred.raw <- raster(pred.raw)
  lambdas <- read.csv(lambdas, header=FALSE, stringsAsFactors=FALSE) # corrected
  k <- sum(as.numeric(lambdas[, 2]) != 0) - 4 # corrected
  out <- t(sapply(seq_len(nlayers(pred.raw)), function(i) {
    x <- pred.raw[[i]]
    x.std <- x/sum(values(x), na.rm=TRUE)
    occ.nodumaxentIC <- function(pred.raw, occ, lambdas) {
      # pred.raw: A raster object or a file path to a raw maxent prediction grid.
      #           Providing a RasterStack will result in *IC being calculated for 
      #           each layer. 
      # occ: A matrix/data.frame with 2 columns (lon, lat). 
      # lambdas: A maxent object or a file path to a lambdas file.
      require(raster)
      if(is(lambdas, 'MaxEnt')) lambdas <- textConnection(lambdas@lambdas)
      if(is.character(pred.raw)) pred.raw <- raster(pred.raw)
      lambdas <- read.csv(lambdas, header=FALSE, stringsAsFactors=FALSE) # corrected
      k <- sum(as.numeric(lambdas[, 2]) != 0) - 4 # corrected
      out <- t(sapply(seq_len(nlayers(pred.raw)), function(i) {
        x <- pred.raw[[i]]
        x.std <- x/sum(values(x), na.rm=TRUE)
        occ.nodupes <- occ[!duplicated(cellFromXY(x, occ)), ]
        n <- nrow(occ.nodupes)
        ll <- log(prod(extract(x.std, occ.nodupes), na.rm=TRUE))
        AIC <- 2*k - 2*ll
        AICc <- AIC + ((2*k*(k+1))/(n - k - 1))
        BIC <- k*log(n) - 2*ll
        c(n=n, k=k, ll=ll, AIC=AIC, AICc=AICc, BIC=BIC)}))
      row.names(out) <- names(pred.raw)
      out}
    pes <- occ[!duplicated(cellFromXY(x, occ)), ]
    n <- nrow(occ.nodupes)
    ll <- log(prod(extract(x.std, occ.nodupes), na.rm=TRUE))
    AIC <- 2*k - 2*ll
    AICc <- AIC + ((2*k*(k+1))/(n - k - 1))
    BIC <- k*log(n) - 2*ll
    c(n=n, k=k, ll=ll, AIC=AIC, AICc=AICc, BIC=BIC)}))
  row.names(out) <- names(pred.raw)
  out
}



###### codigos deprecated para el Minimun volume ellipsoid porque necesitamos que se corrijan algunos de sus errores
library("ntbox")### usando nihetoolbox para poder corregir algunas cosas
#### Valores de registros puntuales de area original
latitud_longitud<-read_csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/nueva_calcular/Monosteira_unicostata_registros_puntuales_data.csv")
latitud_longitud<-latitud_longitud[c("long","lat")] 
vector1<-vect(latitud_longitud, geom=c("long","lat"))

##### valores de registros del area externa a la original
latitud_longitud2<-read_csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/puntos_externos.csv")
latitud_longitud2<-latitud_longitud2[c("longitude","latitude")] 
vector2<-vect(latitud_longitud2, geom=c("longitude","latitude"))

####### debemos realizar un pca antes y extraer cada uno de las variables ambientales a utilizar


###### extraemos de cada una de los pca realizados los puntos en el paleartico 
bio_1<-na.omit(raster::extract(pca_mundo$rasters,vector1))

##### extraemos de cada uno de los PCA los puntos de otros lugares
bio_1b<-na.omit(raster::extract(pca_mundo$rasters,vector2))

#### creamos el formato de lectura para el programa de ntboc para que cree su elipse en cada uno de los casos
#### creamos el dataframe que puede ser leido
latitud_longitud<-cbind(latitud_longitud,"palea",bio_1$PC1,bio_1$PC2,bio_1$PC3)
colnames(latitud_longitud)<-c("longitude", "latitude", "cluster","PC1", "PC2", "PC3")

latitud_longitud2<-cbind(latitud_longitud2,"new",bio_1b$PC1,bio_1b$PC2,bio_1b$PC3)
colnames(latitud_longitud2)<-c("longitude", "latitude", "cluster","PC1", "PC2", "PC3")

#### unimos las dos tablas de los dataframe
cluster_data = base::rbind(latitud_longitud, latitud_longitud2 )

##### ponemos en formato para que pueda ser leido con el cluster de elipsoides
##### creamos esta funcion que parece tener algo diferentes con los otros modelos porque no es igual al resto
### ver linea por linea cual es el error que puede tener
ellipsoid_clusters <- ellipsoid_cluster_plot_3d(niche_data =cluster_data[,c("PC1","PC2","PC3")],
                                                cluster_ids = cluster_data$cluster,
                                                x = "PC1",y="PC2",z="PC3",mve = T,
                                                ellips = T,alpha = 0.01,
                                                grupos = T,vgrupo =cluster_data$cluster,
                                                cex1 = 1,level = 0.95)

##### realizamos las graficas y almacenamos algo de informacion relevante para realizar la comparacion
rgl::plot3d(rgl::ellipse3d(ellipsoid_clusters$cluster_n_new$covariance,
                           centre = ellipsoid_clusters$cluster_n_new$centroid,
                           level = 0.95),alpha=0.4,col="blue")
rgl::plot3d(rgl::ellipse3d(ellipsoid_clusters$cluster_n_palea$covariance,
                           centre = ellipsoid_clusters$cluster_n_palea$centroid,
                           level = 0.95),alpha=0.4,col="red", add=TRUE)
rgl::points3d(cluster_data[,c("PC1","PC2","PC3")])

##############################################################################################
############################################################################################
### otra forma de crear una elipse
centroid<-cobmve$center
data=data.frame(latitud_longitud)[c("PC1", "PC2", "PC3")]
level=0.95
vari<-cobmve$cov
vars=c("PC1", "PC2", "PC3")
#####probamos es el mas apegado a los resultante de la funcion de MASS
cobrob<-MASS::cov.rob(data.frame(latitud_longitud)[c("PC1", "PC2", "PC3")], quantile.used = nrow(latitud_longitud)*0.95,method = "mve")
cobmve<-MASS::cov.mve(data.frame(latitud_longitud)[c("PC1", "PC2", "PC3")], quantile.used = nrow(latitud_longitud)*0.95)
#########################################################}
#######################################
a<-cov_center(data,mve = TRUE,level=0.95,vars = c("PC1","PC2","PC3"))