library ("easypackages")
libraries (c("dismo", "maps", "maptools", "rgdal", "rgeos", "sp", "virtualspecies", "corrplot", 
             "raster", "usdm", "gtools", "tidyverse", "rJava", "ENMeval", "rgbif",
             "ENMTools", "geodata", "devtools", "RStoolbox", "glue", "rmaxent", "ENMeval",
             "sf", "xlsx", "rgl"))

#devtools::install_github("marlonecobos/ellipsenm")
library(ellipsenm)
set.seed(109)
##################################################################################################
################# Data para los analisis de minimun volume ellipsoid##############################
##################################################################################################
# leemos nuevamente las tablas de ocurrencias de cada una de las variables
occurrences1<-read_csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/nueva_calcular/Monosteira_unicostata_registros_puntuales_data.csv")
occurrences2<-read_csv("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/puntos_externos.csv")


##### variables climaticas necesarias para correr el analisis corresponden a las 19 variables de worldclim
rasters<- worldclim_global("bio", res=5, path="C:/Users/amaru/OneDrive/Escritorio/worldclim")


#### realizamos el pca de las 19 variables en todo el mundo
pca_mundo<-raster.pca(rasters, 3)

###### do not run
####### si se requiere crear uno por uno sin medir el overlap de los datos tomados
ellips <- ellipsoid_fit(data = occurrences1, longitude = "long",
                        latitude = "lat", method = "mve1",
                        level = 99, raster_layers = vars)
###### finish do not run

# convertimos el stack de los rasters de cada una de las variables
vars <- stack(pca_mundo$rasters)

# creo los overlap_object que van a ser analizados colocando como son las caracteristicas de cada uno de las ellipses a analizar
niche1 <- overlap_object(occurrences1, species =  "Species", longitude = "long", 
                         latitude = "lat", method = "mve1", level = 99, 
                         variables = vars)
niche2 <- overlap_object(occurrences2, species =  "species", longitude = "longitude", 
                         latitude = "latitude", method = "mve1", level = 99, 
                         variables = vars)

## calculo y modelo los dos nichos para ver su overlap y tener una mejor aproximacion de la data
overlap <- ellipsoid_overlap(niche1, niche2,overlap_type = "full")


elipsoides<-overlap@ellipsoids ##### cortamos para observar los datos generados en el analisis
### elipsoide del nicho 1 de las ocurrencias 1
nicho1<-elipsoides$Niche_1 ##### el elipsoide del nicho que corresponde al primero
#elipsoide del nicho 1 de las ocurrencias
nicho2<-elipsoides$Niche_2 #### el elipsoide del nicho 2 que corresponde al segundo

###############################################################################################
############## REALIZAMOS LAS GRAFICAS DE CADA UNA DE LAS VARIABLES ###########################
###############################################################################################
# se necesitan la matriz de ovarianza y el centroide de los datos
## accede mos almacenados algunos datos almacenados en la informacin
cov_max_1<-slot(nicho1, "covariance_matrix")
centroid1<-slot(nicho1, "centroid")
cov_max_2<-slot(nicho2, "covariance_matrix")
centroid2<-slot(nicho2, "centroid")
##### realizamos las graficas y almacenamos algo de informacion relevante para realizar la comparacion
rgl::plot3d(rgl::ellipse3d(cov_max_1,
                           centre = centroid1,
                           level = 0.99999),alpha=0.7,col="red")
rgl::plot3d(rgl::ellipse3d(cov_max_2,
                           centre =centroid2,
                           level = 0.99999),alpha=0.7,col="blue", add=TRUE)

############################################################################################
#### guardamos la información necesaria de los nichos que se tienen#########################
############################################################################################
setwd("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/MVE_results")
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

write_csv(data.frame(overlap@full_overlap), "full_overlap.csv")


#####################################################################################
######################### PROJECT ###################################################
#####################################################################################
prediction_rep <- ellipsenm::predict(object = nicho1, projection_variables = vars,prediction = "both", truncate = FALSE)



write_csv(data.frame(prediction_rep@prevalence), "prevalence_predicted.csv")
writeRaster(prediction_rep@prediction_maha,filename = "mahalanobis_distance.tif", format = "GTiff", overwrite=TRUE)
writeRaster(prediction_rep@prediction_suit,filename = "suitability_index.tif", format = "GTiff", overwrite=TRUE )
