setwd("D:/0001_JAC_monosteira/Modelos_Maxent/Maxent_complete/Modelos/LQ05/Resultados_promedio")

path_126<-"D:/0001_JAC_monosteira/Modelos_Maxent/Maxent_complete/Modelos/LQ05/126"
path_585<-"D:/0001_JAC_monosteira/Modelos_Maxent/Maxent_complete/Modelos/LQ05/585"

files_126<-list.files(path_126, full.names = TRUE)
files_585<-list.files(path_585, full.names = TRUE)

stack_126<-stack(files_126)
suma<-stack_126$modelo_futuro_BCC_CSM2_MR_126+
  stack_126$modelo_futuro_CMCC_ESM2_126+
  stack_126$modelo_futuro_GISS_E2_1_G_126+
  stack_126$modelo_futuro_MIROC6_126+
  stack_126$modelo_futuro_MPI_ESM1_2_LR_126
stack_126_prom<-suma/5

binario_stack_126<-stack_126_prom>=0.395
binario_stack_126[binario_stack_126==0] <- NA
polygono_cortar<-as.polygons(rast(binario_stack_126))
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "binario_stack_126_promedio.shp")

stack_585<-stack(files_585)
suma2<-stack_585$modelo_futuro_CMCC_ESM2_585+
  stack_585$modelo_futuro_CSM2_MR_585+
  stack_585$modelo_futuro_GISS_E2_1_G_585+
  stack_585$modelo_futuro_MIROC6_585+
  stack_585$modelo_futuro_MPI_ESM1_2_LR_585
stack_585_prom<-suma2/5

binario_stack_585<-stack_585_prom>=0.395
binario_stack_585[binario_stack_585==0] <- NA
polygono_cortar<-as.polygons(rast(binario_stack_585))
s <- sf::st_as_sf(polygono_cortar)
st_write(s, "binario_stack_585_promedio.shp")



