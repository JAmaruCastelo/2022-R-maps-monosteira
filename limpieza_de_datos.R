######  limpieza de datos generales 

#### eliminamos duplicados de la tabla
library ("ntbox")

paleartico<-read.csv ("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/Paleartico_records.csv")
introduced<-read.csv ("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/introduced_records.csv")
total<-read.csv ("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales/Total_records.csv")


paleartico_clean <- clean_dup(paleartico,longitude = "Longitud",latitude = "Latitud")
introduced_clean <- clean_dup(introduced,longitude = "Longitud",latitude = "Latitud")
total_clean <- clean_dup(total,longitude = "Longitud",latitude = "Latitud")


#### guardamos los archivos limpios
setwd("C:/Users/amaru/OneDrive/Escritorio/CODES/00017_JAC_monosteira/Bases_de_datos_registros_puntuales")
write.csv(paleartico_clean, "paleartico_clean.csv")
write.csv(introduced_clean, "introduced_clean.csv")
write.csv(total_clean , "total_clean.csv")
