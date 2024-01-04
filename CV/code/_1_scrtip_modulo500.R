rm(list=ls())

#-------------------------------------------------------------
# Carpetas de informacion
#-------------------------------------------------------------
ruta    <- "C:/Users/et396/Dropbox"
enaho   <- "/BASES/ENAHO"
codigo  <- "/Scripts"
out     <- "/Salida_indicadores"

# Paquetes para instalar y poder usarlos
paquetes_set <- c("readstata13", "dplyr", "tidyverse","sjlabelled",
                  "survey", "stargazer", "caret", "foreign","readr")
# install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

# Carga de funciones de codigos de limpieza de base
source(paste0(ruta,enaho,codigo,"/","Funciones_MTPE.R"))

#------------------------------------------------
# Base de datos para EMPLEO
#------------------------------------------------
data <- read.dta13(paste0(ruta,"/",enaho,"/","2021","/","enaho01a-2021-500.dta"))
data %>%  names()

base <- funcion_r500(    data)           # sociolaborales
base <- funcion_rsocio(  base)           # sociodemograficos
base <- funcion_rDpto(   base)           # Departamentos

# Filtro solo informacion por jefe de hogar
df_new <- base %>% filter(p203=='jefe/jefa')

df_new %>%  dim()
df_new %>%  tail()

# Generacion de variables de codigo de personas y hogar
df_empleo <- df_new %>% 
  mutate(rcod_persona = paste0(conglome,vivienda, hogar,codperso),
         rcod_hogar   = paste0(conglome,vivienda, hogar),
         lnr6=ifelse(r6>0,log(r6),NA)) 

df_empleo <- df_empleo %>%
  select(starts_with("r"), lnr6) # seleccionando las variables que comienzan con R

df_empleo <- df_empleo %>%
  select(!starts_with("r559_")) # eleiminando las variables que comienzas con r559

# Etiquetas
df_empleo$rcod_persona     <- set_label(df_empleo$rcod_persona    , c("Codigo persona") )
df_empleo$lnr6             <- set_label(df_empleo$lnr6            , c("Log. Ingreso") )

# Filtro de edad 
df_empleo[,c("r6","lnr6","rhoras","redad","rmu","rneduca")] %>%  summary()
df_empleo <- df_empleo %>% filter(redad>18 & redad< 70) # solo edad 19-69
df_empleo <- df_empleo %>% drop_na(rmu, rneduca) # no missing en dichas variables

# df_empleo %>%  str()
df_empleo %>%  dim()
df_empleo %>%  names()
df_empleo %>%  head()
df_empleo %>%  summary()

rm(data, base, df_new)
# Guardar informacion de base de datos
# ------------------------------------------------------
# write.csv(df_empleo, file = paste0(ruta, enaho, out,"/","mod_500_2021.csv"))
saveRDS(df_empleo,  file = paste0(ruta, enaho, out,"/","mod_500_2021.rds"))