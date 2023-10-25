##### codigo para hacer las proyecciones
library("mop")


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

#### realizo el PcA de mis datos para poder hacer el analisis de maxent
pca1<-raster.pca(raster.crop, 4) #### realizo el PCa y obtengo las primero 4 componentes principales para hacer als predicciones


#################################################################################################
#### proyecto el PCA realizado para los cortes actuales y para las capas del futuro #############
#################################################################################################
#### proyectamos para el mundo con las configuraciones del presente
world_PCA_actual<-raster::predict(rasters, pca1$pca.object)
world_PCA_actual<-world_PCA_actual[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_actual)
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_actual.tif", overwrite=TRUE )


#### proyecto para el futuro con el pca Corrido para el pasado
### debo setear los nombres de las capas porque o si no te sale error y no te permite correr las siguientes partes
namesa<-c("wc2.1_5m_bio_1","wc2.1_5m_bio_2","wc2.1_5m_bio_3","wc2.1_5m_bio_4","wc2.1_5m_bio_5",
          "wc2.1_5m_bio_6","wc2.1_5m_bio_7","wc2.1_5m_bio_8","wc2.1_5m_bio_9",
          "wc2.1_5m_bio_10","wc2.1_5m_bio_11","wc2.1_5m_bio_12","wc2.1_5m_bio_13",
          "wc2.1_5m_bio_14","wc2.1_5m_bio_15","wc2.1_5m_bio_16",
          "wc2.1_5m_bio_17","wc2.1_5m_bio_18","wc2.1_5m_bio_19")

names(rastersa) <-namesa
world_PCA_futuro_BCC_CSM2_MR_126 <- raster::predict(rastersa, pca1$pca.object)
world_PCA_futuro_BCC_CSM2_MR_126<-world_PCA_futuro_BCC_CSM2_MR_126[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_BCC_CSM2_MR_126)
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_futuro_BCC_CSM2_MR_126.tif", overwrite=TRUE )


names(rastersb) <-namesa
world_PCA_futuro_CMCC_ESM2_126 <- raster::predict(rastersb, pca1$pca.object)
world_PCA_futuro_CMCC_ESM2_126<-world_PCA_futuro_CMCC_ESM2_126[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_CMCC_ESM2_126)
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_futuro_CMCC_ESM2_126.tif", overwrite=TRUE )

names(rastersc) <-namesa
world_PCA_futuro_MIROC6_126 <- raster::predict(rastersc, pca1$pca.object)
world_PCA_futuro_MIROC6_126 <-world_PCA_futuro_MIROC6_126 [[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_MIROC6_126)
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_futuro_MIROC6_126.tif", overwrite=TRUE )
#
names(rastersd) <-namesa
world_PCA_futuro_MPI_ESM1_2_LR_126 <- raster::predict(rastersd, pca1$pca.object)
world_PCA_futuro_MPI_ESM1_2_LR_126<-world_PCA_futuro_MPI_ESM1_2_LR_126[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_MPI_ESM1_2_LR_126)
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_futuro_MPI_ESM1_2_LR_126.tif", overwrite=TRUE )


names(rasterse) <-namesa
world_PCA_futuro_GISS_E2_1_G_126 <- raster::predict(rasterse, pca1$pca.object)
world_PCA_futuro_GISS_E2_1_G_126<-world_PCA_futuro_GISS_E2_1_G_126[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_GISS_E2_1_G_126)
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_futuro_GISS_E2_1_G_126.tif", overwrite=TRUE )


names(rastersa2) <-namesa
world_PCA_futuro_BCC_CSM2_MR_585 <- raster::predict(rastersa2, pca1$pca.object)
world_PCA_futuro_BCC_CSM2_MR_585<-world_PCA_futuro_BCC_CSM2_MR_585[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_BCC_CSM2_MR_585)
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_futuro_BCC_CSM2_MR_585.tif", overwrite=TRUE )


names(rastersb2) <-namesa
world_PCA_futuro_CMCC_ESM2_585 <- raster::predict(rastersb2, pca1$pca.object)
world_PCA_futuro_CMCC_ESM2_585<-world_PCA_futuro_CMCC_ESM2_585[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_CMCC_ESM2_585)
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_futuro_CMCC_ESM2_585.tif", overwrite=TRUE )


names(rastersc2) <-namesa
world_PCA_futuro_MIROC6_585 <- raster::predict(rastersc2, pca1$pca.object)
world_PCA_futuro_MIROC6_585<-world_PCA_futuro_MIROC6_585[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_MIROC6_585 )
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_futuro_MIROC6_585.tif", overwrite=TRUE )


names(rastersd2) <-namesa
world_PCA_futuro_MPI_ESM1_2_LR_585 <- raster::predict(rastersd2, pca1$pca.object)
world_PCA_futuro_MPI_ESM1_2_LR_585<-world_PCA_futuro_MPI_ESM1_2_LR_585[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_MPI_ESM1_2_LR_585)
raster::writeRaster(mop1$mop_basic,filename = "mop_world_PCA_futuro_MPI_ESM1_2_LR_585.tif", overwrite=TRUE )


names(rasterse2) <-namesa
world_PCA_futuro_GISS_E2_1_G_585 <- raster::predict(rasterse2, pca1$pca.object)
world_PCA_futuro_GISS_E2_1_G_585<-world_PCA_futuro_GISS_E2_1_G_585[[c("PC1", "PC2", "PC3","PC4")]]
mop1<-mop::mop(pca1$rasters,world_PCA_futuro_GISS_E2_1_G_585)
raster::writeRaster(mop1$mop_basic,filename = "mop_wworld_PCA_futuro_GISS_E2_1_G_585 .tif", overwrite=TRUE )


##########################################################################################################
####################### PROYECCION A OTRAS CAPAS AMBIENTALES #############################################
##########################################################################################################
#### ver como automatiza el paso para que se seleccione uno por uno cada una de las especies proyectadas
maximo="C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Modelos_Maxent/Maxent_introduced/Modelos/L05"
setwd(maximo)#### toda las capas de proyecciones se van a guardar en la misma carpeta del modelo estudiado
#########################################################################################################
####### leemos el modelo maxent realizado y obtenido con los mejores resultados en el analisis###########
#########################################################################################################
modelo_maxent=import_maxent(maximo)


modelo_presente<-dismo::predict(object=modelo_maxent, x=pca1$rasters )
writeRaster(raster(modelo_presente), filename = "modelo_presente_calibration.tif", format = "GTiff", overwrite=TRUE)

#########################################################################################################
########## predecimos para diferentes partes del mundo y para el futuro ##################################
##########################################################################################################
#### predecimos para el presente
modelo_presente<-dismo::predict(object=modelo_maxent, x=world_PCA_actual )
writeRaster(raster(modelo_presente), filename = "modelo_presente.tif", format = "GTiff", overwrite=TRUE)

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

