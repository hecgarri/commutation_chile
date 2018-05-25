rm(list= ls())

if (!require(cem)) install.packages("cem"); require(cem)
if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(survey)) install.packages("survey"); require(survey)
if (!require(stringi)) install.packages("stringi"); require(stringi)
if (!require(foreign)) install.packages("foreign"); require(foreign)

path = file.path("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble",
                 "Bases de datos/Encuesta Suplementaria de Ingresos (ESI)",
                  "ESI_2016_PERSONAS _USUARIOSEXTERNOS.sav")
path2 = file.path("/home/hector/GoogleDriveUBB",
                  "OLR Ñuble - Observatorio laboral de Ñuble",
                  "Bases de datos/Nueva Encuesta Nacional de Empleo (ENE)",
                  "ENE 2016 11 OND.sav")

esi <- read.spss(path,to.data.frame = TRUE, use.value.labels = FALSE)

ene <- read.spss(path2,to.data.frame = TRUE, use.value.labels = FALSE)

names(esi) = tolower(names(esi))
names(ene) = tolower(names(ene))

datos = left_join(esi,ene, by = c("id_identificacion","hogar","nro_linea"))

datos = datos[,-grep(".y",colnames(datos))]

colnames(datos) = gsub(".x","",colnames(datos))


#############################################
# Identificación de migrantes internacionales
#############################################

datos = datos %>% mutate(migrantes = ifelse(nacionalidad!="00",1,0)) 

########################################
# Identificación de conmutantes
########################################

datos = datos %>% mutate(aux = stri_length(b18_codigo),
                     region_e = b18_codigo,
                     region_e = ifelse(aux==5, stri_sub(region_e,1,2), region_e),
                     region_e =ifelse(aux==4, stri_sub(region_e,1,1), region_e)) %>% 
              mutate(conmutante = ifelse(region_e!=region,1,0))

########################################
# Sólo ocupados
########################################

tip_ocupados = c(1,2,3)

datos = datos %>% mutate(ocup = ifelse(cae_general %in% tip_ocupados,1,0))

# Total de migrantes 

total_migrantes = svytotal(~migrantes,ddatosgn = ddatosgn)

total_conmutantes = svytotal(~conmutante,ddatosgn = ddatosgn,na.rm = TRUE)

# Vive en Biobío,pero trabaja en otra region 
datos = datos %>% mutate(conmutante_biobio_a = ifelse(conmutante==1 & region==8,1,0))
# Trabaja en Biobío pero vive en otra región
datos = datos %>% mutate(conmutante_biobio_b = ifelse(conmutante==1 & region_e==8,1,0))

# Vive en Antofagasta,pero trabaja en otra region 
datos = datos %>% mutate(conmutante_anto_a = ifelse(conmutante==1 & region==2,1,0))
# Trabaja en Antofagasta pero vive en otra región
datos = datos %>% mutate(conmutante_anto_b = ifelse(conmutante==1 & region_e==2,1,0))

datos = datos %>% mutate(ecivil = ifelse(est_conyugal==1 | est_conyugal==2,1,
                              ifelse(est_conyugal==3 | est_conyugal==4,
                                     est_conyugal==5 | est_conyugal==6,0)))
# total de conmutantes de tipo a en Biobío 

# Características del diseño muestral 
ddatosgn = svyddatosgn(id = ~id_directorio, strata = ~estrato, weights  = 
                     ~fact,nest = TRUE, data = datos)
options(survey.lonely.psu="certainty")

svytotal(~conmutante_biobio_a, ddatosgn = ddatosgn,na.rm = TRUE)
svytotal(~conmutante_anto_b, ddatosgn = ddatosgn,na.rm = TRUE)

vars = c("edad","sexo","ecivil")


