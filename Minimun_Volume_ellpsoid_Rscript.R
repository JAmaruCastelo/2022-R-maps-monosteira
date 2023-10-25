library ("easypackages")
libraries (c("dismo", "maps", "maptools", "rgdal", "rgeos", "sp", "virtualspecies", "corrplot", 
             "raster", "usdm", "gtools", "tidyverse", "rJava", "ENMeval", "rgbif",
             "ENMTools", "geodata", "devtools", "RStoolbox", "glue", "rmaxent", "ENMeval",
             "sf", "xlsx", "rgl"))

#devtools::install_github("marlonecobos/ellipsenm")
library(ellipsenm)
set.seed(109)

##### path donde se va a guardar toda la informacion obtenida de este analisis
setwd("D:/0001_JAC_monosteira/MVE_results")
##################################################################################################
################# Data para los analisis de minimun volume ellipsoid##############################
##################################################################################################
# leemos nuevamente las tablas de ocurrencias de cada una de las variables
occurrences1<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/introduced_clean.csv")
occurrences2<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/paleartico_clean.csv")
occurrences3<-read_csv("D:/0001_JAC_monosteira/Bases_de_datos_registros_puntuales/Total_records_clean.csv")

##### variables climaticas necesarias para correr el analisis corresponden a las 19 variables de worldclim
rasters<- worldclim_global("bio", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")


#### realizamos el pca de las 19 variables en todo el mundo
pca_mundo<-raster.pca(rasters, 3)

#### guardamos la información importante de el PcA para que este todo completo
prop_varianza <- pca_mundo$pca.object$sdev^2 / sum(pca_mundo$pca.object$sdev^2)
informacion_pca<- data.frame("desviacion estandar"=pca_mundo$pca.object$sdev, 
                             "prop_varianza"=prop_varianza, "varianza_acumulada"=cumsum(prop_varianza))
write.xlsx(informacion_pca, "informacion_pca.xlsx")
informacion_pca_rotation <- data.frame(pca_mundo$pca.object$rotation)
write.xlsx(informacion_pca_rotation, "rotation_pca.xlsx")



###### do not run
####### si se requiere crear uno por uno sin medir el overlap de los datos tomados
ellips <- ellipsoid_fit(data =occurrences1, longitude = "Longitud",
                        latitude = "Latitud", method = "mve1",
                        level = 99, raster_layers = vars)
###### finish do not run



# convertimos el stack de los rasters de cada una de las variables
vars <- stack(pca_mundo$rasters)

# creo los overlap_object que van a ser analizados colocando como son las caracteristicas de cada uno de las ellipses a analizar

niche1 <- overlap_object(data.frame(occurrences1)[c("Species","Longitud","Latitud")], species =  "Species", longitude = "Longitud", 
                         latitude = "Latitud", method = "mve1", level =95, 
                         variables = vars)
niche2 <- overlap_object(data.frame(occurrences2)[c("Species","Longitud","Latitud")], species =  "Species", longitude = "Longitud", 
                         latitude = "Latitud", method = "mve1", level =95, 
                         variables = vars)
niche3 <- overlap_object(data.frame(occurrences3)[c("Species","Longitud","Latitud")], species =  "Species", longitude = "Longitud", 
                         latitude = "Latitud", method = "mve1", level =95, 
                         variables = vars)

## calculo y modelo los dos nichos para ver su overlap y tener una mejor aproximacion de la data
overlap <- ellipsoid_overlap(niche1,niche2, niche3, overlap_type = "full")


elipsoides<-overlap@ellipsoids ##### cortamos para observar los datos generados en el analisis
### elipsoide del nicho 1 de las ocurrencias 1
nicho1<-elipsoides$Niche_1 ##### el elipsoide del nicho que corresponde al primero
#elipsoide del nicho 2 de las ocurrencias
nicho2<-elipsoides$Niche_2 #### el elipsoide del nicho 2 que corresponde al segundo
#elipsoide del nicho 3 de las ocurrencias
nicho3<-elipsoides$Niche_3 #### el elipsoide del nicho 2 que corresponde al segundo

###############################################################################################
############## REALIZAMOS LAS GRAFICAS DE CADA UNA DE LAS VARIABLES ###########################
###############################################################################################
# se necesitan la matriz de ovarianza y el centroide de los datos
## accede mos almacenados algunos datos almacenados en la informacin
cov_max_1<-slot(nicho1, "covariance_matrix")
centroid1<-slot(nicho1, "centroid")
cov_max_2<-slot(nicho2, "covariance_matrix")
centroid2<-slot(nicho2, "centroid")
cov_max_3<-slot(nicho3, "covariance_matrix")
centroid3<-slot(nicho3, "centroid")

##### realizamos las graficas y almacenamos algo de informacion relevante para realizar la comparacion
rgl::plot3d(rgl::ellipse3d(cov_max_1,
                           centre = centroid1),alpha=0.9,col="black")
rgl::plot3d(rgl::ellipse3d(cov_max_2,
                           centre =centroid2),alpha=0.9,col="yellow", add=TRUE)
rgl::plot3d(rgl::ellipse3d(cov_max_3,
                           centre =centroid3),alpha=0.9,col="red", add=TRUE)

plot_overlap(overlap)

################### PROBAMOS LOS RESULTADOS DE NICHEA ###########################################
########## usamos la informacion de resultado de NicheA
PC1<-c(5.407622978507399,2.5071640047700146,1.2804923317639396)
PC2<-c(2.5071640047700146,1.253642712232795,0.6583982757764698)
PC3<-c(1.2804923317639356,0.6583982757764679,0.9396214752768154)
cov_max_1_Niche<-matrix(c(PC1, PC2, PC3),nrow = 3,ncol = 3)
colnames(cov_max_1_Niche)<- c("PC1","PC2","PC3")
rownames(cov_max_1_Niche)<- c("PC1","PC2","PC3")
centroid1_Niche<-matrix(c(-2.537973135736992, -0.13510001744927483, 0.7562746246918075),nrow = 1,ncol = 3)
colnames(centroid1_Niche)<- c("PC1","PC2","PC3")
PC1<-c(0.24056183182901222,0.14121560433021962,0.08327885607016121)
PC2<-c(0.14121560433021962,0.16088378294780228,0.07525525984582551)
PC3<-c(0.08327885607016122,0.07525525984582553,0.23369007927390806)
cov_max_2_Niche<-matrix(c(PC1, PC2, PC3),nrow = 3,ncol = 3)
colnames(cov_max_2_Niche)<- c("PC1","PC2","PC3")
rownames(cov_max_2_Niche)<- c("PC1","PC2","PC3")
centroid2_Niche<-matrix(c(-1.4570298900772598, -0.7627585664957375, -0.6993065043028718),nrow = 1,ncol = 3)
colnames(centroid3_Niche)<- c("PC1","PC2","PC3")
PC1<-c(0.23344553031602935,0.13749387415128736,0.08908756103910685)
PC2<-c(0.13749387415128736,0.15121046428161924,0.06585025488460511)
PC3<-c(0.08908756103910682,0.0658502548846051,0.2260830205584947)
cov_max_3_Niche<-matrix(c(PC1, PC2, PC3),nrow = 3,ncol = 3)
colnames(cov_max_3_Niche)<- c("PC1","PC2","PC3")
rownames(cov_max_3_Niche)<- c("PC1","PC2","PC3")
centroid3_Niche<-matrix(c(-1.6142512494013135, -0.5789689426937575, -0.5807075156262684),nrow = 1,ncol = 3)
colnames(centroid3_Niche)<- c("PC1","PC2","PC3")

bio_1<-raster::extract(pca_mundo$rasters,vector)
rgl::plot3d(bio_1$PC1,bio_1$PC2, bio_1$PC3,alpha=0.9,col="red", add=TRUE)
rgl::plot3d(rgl::ellipse3d(cov_max_1_Niche,
                           centre = centroid1_Niche),alpha=0.4,col="black",add=TRUE)

bio_1_palea<-raster::extract(pca_mundo$rasters,vector_palea)
rgl::plot3d(bio_1_palea$PC1,bio_1_palea$PC2, bio_1_palea$PC3,alpha=0.9,col="red")

rgl::plot3d(rgl::ellipse3d(cov_max_2_Niche,
                           centre = centroid2_Niche),alpha=0.4,col="yellow",add=TRUE)
rgl::plot3d(rgl::ellipse3d(cov_max_3_Niche,
                           centre = centroid3_Niche),alpha=0.4,col="red",add=TRUE)








############################################################################################
#### guardamos la información necesaria de los nichos que se tienen#########################
############################################################################################

write_csv(data.frame(overlap@ellipsoids$Niche_1@level), "level_a.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_1@centroid), "centroid_a.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_1@covariance_matrix), "covariancematrix_a.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_1@niche_volume), "nichevolume_a.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_1@semi_axes_length), "semi_axes_lenght_a.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_1@axes_coordinates), "axes_coord_a.csv")

write_csv(data.frame(overlap@ellipsoids$Niche_2@level), "level_b.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_2@centroid), "centroid_b.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_2@covariance_matrix), "covariancematrix_b.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_2@niche_volume), "nichevolume_b.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_2@semi_axes_length), "semi_axes_lenght_b.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_2@axes_coordinates), "axes_coord_b.csv")

write_csv(data.frame(overlap@ellipsoids$Niche_3@level), "level_c.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_3@centroid), "centroid_c.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_3@covariance_matrix), "covariancematrix_c.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_3@niche_volume), "nichevolume_c.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_3@semi_axes_length), "semi_axes_lenght_c.csv")
write_csv(data.frame(overlap@ellipsoids$Niche_3@axes_coordinates), "axes_coord_c.csv")

write_csv(data.frame(overlap@full_overlap), "full_overlap.csv")


#####################################################################################
######################### PROJECT ###################################################
#####################################################################################
prediction_rep_1 <- ellipsenm::predict(object = nicho1, projection_variables = vars,prediction = "both", truncate = FALSE)
prediction_rep_2<- ellipsenm::predict(object = nicho2, projection_variables = vars,prediction = "both", truncate = FALSE)
prediction_rep_3<- ellipsenm::predict(object = nicho3, projection_variables = vars,prediction = "both", truncate = FALSE)



write_csv(data.frame(prediction_rep_1@prevalence), "prevalence_predicted_introduced.csv")
writeRaster(prediction_rep_1@prediction_maha,filename = "mahalanobis_distance_introduced.tif", format = "GTiff", overwrite=TRUE)
writeRaster(prediction_rep_1@prediction_suit,filename = "suitability_index_introduced.tif", format = "GTiff", overwrite=TRUE )

write_csv(data.frame(prediction_rep_2@prevalence), "prevalence_predicted_paleartico.csv")
writeRaster(prediction_rep_2@prediction_maha,filename = "mahalanobis_distance_paleartico.tif", format = "GTiff", overwrite=TRUE)
writeRaster(prediction_rep_2@prediction_suit,filename = "suitability_index_paleartico.tif", format = "GTiff", overwrite=TRUE )

write_csv(data.frame(prediction_rep_3@prevalence), "prevalence_predicted_total.csv")
writeRaster(prediction_rep_3@prediction_maha,filename = "mahalanobis_distance_total.tif", format = "GTiff", overwrite=TRUE)
writeRaster(prediction_rep_3@prediction_suit,filename = "suitability_index_total.tif", format = "GTiff", overwrite=TRUE )




x=readWKT(paste("POLYGON((040,1050,060,4060,40100,5090,60100,60", "60,10060,9050,10040,6040,600,5010,400,4040,040))"))

