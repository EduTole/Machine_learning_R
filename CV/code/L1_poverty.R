#-----------------------------
# Autor: Edinson Tolentino
# Proyecto : poverty
#-----------------------------
rm(list=ls())

#-------------------------------------------------------------
# Carpetas de informacion
#-------------------------------------------------------------
ruta   <- "C:/Users/et396/Dropbox"
base   <- "/BASES/ENAHO"
codigo <- "/Scripts"
out    <- "/Docencia/Impacto/ML/L1/Data"

# Importar librerias

paquetes <- c("readstata13", "dplyr", "tidyverse", 
              "survey", "stargazer","caret","sjlabelled")
#install.packages(paquetes)
lapply(paquetes, library, character.only=TRUE)

#------------------------------------------------
# Base de datos para SUMARY (poverty)
#------------------------------------------------
# Carga de funciones de codigos de limpieza de base
source(paste0(ruta,base,codigo,"/","Funciones_MTPE.R"))

data <- read.dta13(paste0(ruta,"/",base,"/","2021","/","sumaria-2021.dta"))
data %>%  names()

# Mesuare poverty 
base_pobreza <- funcion_rpoverty(data) # Pobreza
base_pobreza <- funcion_rDpto(base_pobreza)  # Departamento

# Filtro de las variables creadas y nueva base a crear
base_pobreza <- base_pobreza %>% 
  mutate(rcod_hogar=paste0(conglome,vivienda, hogar),
         lnrgasto=log(rgasto)) %>% 
  dplyr::select(rcod_hogar,rpoverty, rpoverty_c, rpoverty_f, 
                rgasto,lnrgasto,rDpto, rpeople, factor07)

Hmisc::describe(base_pobreza)

rm(data)
# Import dataset empleo
#------------------------------------------------
data <- read.dta13(paste0(ruta,"/",base,"/","2021","/","enaho01a-2021-500.dta"))
data %>%  names()

# Filtro de MTPE
base_empleo <- funcion_rfiltro(data)

# Filtro de MTPE
base_empleo <- base_empleo %>% 
  filter(rfiltro==1)

# Filtro de ingresos, departamento, edad, mujer y nivel educativo
base_empleo <- funcion_r500(base_empleo)
base_empleo <- funcion_rDpto(base_empleo)
base_empleo <- funcion_redad(base_empleo)
base_empleo <- funcion_rmujer(base_empleo)
base_empleo <- funcion_rneduca(base_empleo)
base_empleo <- funcion_rpareja(base_empleo)

base_empleo <- funcion_rjefe(base_empleo)
base_empleo %>%  dim()

base_empleo <- base_empleo %>% 
  filter(rjefe=="jefe")

# filtro de solo PEA
Hmisc::describe(base_empleo$ocu500)
base_empleo <- base_empleo %>% 
  filter(ocu500<=2 & ocu500>=1)
base_empleo %>%  dim()

# filtro de variables 
base_empleo <- base_empleo %>% 
  mutate(rcod_persona=paste0(conglome,vivienda, hogar,codperso),
         rcod_hogar=paste0(conglome,vivienda, hogar),
         lnr6=ifelse(r6>0,log(r6),NA)) %>% 
  dplyr::select(rcod_persona,rcod_hogar,r6,lnr6, redad, rmujer, rneduca,
                rmu, rocupado, rinfo, rcivil, rcivil_rpareja, rcivil_rsoltero, 
                rinfo, rneduca_respecial, rneduca_rinicial, rneduca_rprimaria, 
                rneduca_rsecundaria, rneduca_rsuperior, rneduca_rtecnica)

base_empleo %>%  names()
base_empleo %>%  head()
base_empleo %>%  dim()
base_empleo %>%  summary()

rm(data)
# Import dataset vivienda
#------------------------------------------------
data <- read.dta13(paste0(ruta,"/",base,"/","2021","/","enaho01-2021-100.dta"))
data %>%  dim()

base_vivienda <- funcion_rmod100(data)
base_vivienda %>%  names()

base_vivienda <- base_vivienda %>%
  mutate(rcod_hogar=paste0(conglome,vivienda, hogar)) %>% 
  dplyr::select(rcod_hogar, rnbi1_adecuada,rnbi1_inadecuada, 
                rnbi2_sinhacinamiento,rnbi2_conhacinamiento, 
                rnbi3_consshh, rnbi4_conschool, rnbi4_sinschool,
                relectricidad)

base_vivienda %>%  dim()
base_vivienda %>%  names()
base_vivienda %>%  summary()
Hmisc::describe(base_vivienda$relectricidad)

# Informacion de union de bases de datos
# -------------------------------------------------------------------------
base_final_2021 <- merge(base_pobreza   , base_empleo , by=c("rcod_hogar"))       # union de datasets
base_final_2021 <- merge(base_final_2021,base_vivienda, by=c("rcod_hogar"))   # union de vivienda
base_final_2021 %>%  dim()
base_final_2021 %>%  names()
base_final_2021 %>%  summary()

# drop NA
base_final_2021 <- base_final_2021 %>% drop_na(lnr6, rneduca)
base_final_2021 %>%  dim()
base_final_2021 %>%  names()
base_final_2021 %>%  summary()

# jefe de hogar 18 - 66
Hmisc::describe(base_final_2021$redad)
Hmisc::describe(base_final_2021$rgasto)
Hmisc::describe(base_final_2021$r6)

# filtro de edad
base_final_2021 <- base_final_2021 %>% 
  filter(redad>17 & redad<67)

# filtro de gasto
base_final_2021 <- base_final_2021 %>% 
  filter(rgasto>0 & redad<24000)

# filtro de ingresos
base_final_2021 <- base_final_2021 %>% 
  filter(r6>0 & r6<10000)

base_final_2021 %>%  dim()
base_final_2021 %>%  str()

# Etiqueta de las variables
# base_final_2021 <- base_final_2021 %>% 
#   set_label(rcod_hogar, label= "Codigo hogar")

base_final_2021$rcod_hogar   <- set_label(base_final_2021$rcod_hogar    , c("Codigo hogar") )
base_final_2021$rpoverty     <- set_label(base_final_2021$rpoverty      , c("Poverty ==1") )
base_final_2021$rpoverty_c   <- set_label(base_final_2021$rpoverty_c    , c("Poverty chr    ==1") )
base_final_2021$rpoverty_f   <- set_label(base_final_2021$rpoverty_f    , c("Poverty factor ==1") )
base_final_2021$rgasto       <- set_label(base_final_2021$rgasto        , c("Gasto mensual (soles)") )
base_final_2021$lnrgasto     <- set_label(base_final_2021$lnrgasto      , c("Log Gasto mensual (soles)") )
base_final_2021$rDpto        <- set_label(base_final_2021$rDpto         , c("Departamento") )

base_final_2021 %>%  str()

# Guardar informacion 
# -------------------------------------------------------------------------
write.csv(base_final_2021, file = paste0(ruta,"/",out,"/","base_pobreza.csv"))
saveRDS(base_final_2021  , file = paste0(ruta,"/",out,"/","base_pobreza.rds"))


