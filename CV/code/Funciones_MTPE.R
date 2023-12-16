#### Filtro de MTPE

# 1. filtros
funcion_rfiltro <- function(data){
  #data <- data %>% 
  #  mutate(rfiltro= ifelse((p204=="si" & p205=="no") | (p204=="no" & p206=="si") & !is.na(p501) ,1,0))
  data$rfiltro <- ifelse((data$p204=="si" & data$p205=="no") | ( data$p204=="no" & data$p206=="si") & !is.na(data$p501) ,1,0)
  return(data)
}

# 2. Variable sde ingresos
#funcion_ingresos <- function(data){
#  ingresos <- data %>% 
#    dplyr::select(i524a1, d529t, i530a, d536, i538a1, d540t, i541a, d543 ,d544t)
#  return(data)
#}

# 2. Filtro de Empleo (Modulo 500)
funcion_r500 <- function(data){
  #library(caret) # using function 
  #library(mltools)
  
  # Categoria de niveles ocupacionales
  data$rcategoria <- factor(ifelse(data$ocu500==1,"Ocupado", 
                                   ifelse(data$ocu500==2,"Desempleo",
                                          ifelse(data$ocu500==3,"Oculto",
                                                 ifelse(data$ocu500==4,"Inactico",NA))))) 
  
  base_hot  <- cbind(data["rcategoria"],data["ocupinf"])  # solo filtramos la variable (columna de interes)
  dummy     <- dummyVars("~ .", data=base_hot)            # filtramos las variables como nombre
  
  final_datahot <- data.frame(predict(dummy, newdata=base_hot)) # aplicamos la funcion de code hot
  # extraccion de variables
  data$rmu        <- final_datahot$rcategoria.Desempleo     # quedamos con la var. discreta desempleo
  data$rocupado   <- final_datahot$rcategoria.Ocupado       # quedamos con la var. discreta ocupado
  data$roculto    <- final_datahot$rcategoria.Oculto        # quedamos con la var. discreta desempleo oculto
  data$rinactivo  <- final_datahot$rcategoria.Inactico      # quedamos con la var. discreta Inactivo
  data$rinfo      <- replace_na(final_datahot$ocupinf.empleo.informal,0)  # quedamos con la var. discreta informalidad
  
  # Ingresos laborales
  ingresos <- data %>% 
    dplyr::select(i524a1, d529t, i530a, d536, i538a1, d540t, i541a, d543 ,d544t)
  data$r6 <- ifelse(data$ocu500==1, rowSums(ingresos, na.rm=T)/12,0)
  
  return(data)
  
}

# 8. funcion de ingresos 
funcion_r6 <- function(data){
  ingresos <- data %>% 
    dplyr::select(i524a1, d529t, i530a, d536, i538a1, d540t, i541a, d543 ,d544t)
  data$r6 <- ifelse(data$ocu500==1, rowSums(ingresos, na.rm=T)/12,0)
  return(data)
}

# 3. Nivel educativo
funcion_rneduca <- function(df){
  
  base_hot <- cbind(df["p301a"]) 
  dummy <- dummyVars("~ .", data=base_hot, sep="_")
  final_datahot <- data.frame(predict(dummy, newdata=base_hot))
  
  final_datahot <- final_datahot %>%  mutate(rneduca =factor(ifelse(p301a_primaria.incompleta==1 | p301a_primaria.completa==1 ,"rprimaria",
                                                             ifelse(p301a_secundaria.incompleta==1 | p301a_secundaria.completa==1,"rsecundaria",
                                                                    ifelse(p301a_superior.no.universitaria.incompleta==1 | p301a_superior.no.universitaria.completa==1,"rtecnica",
                                                                           ifelse(p301a_superior.universitaria.incompleta==1 | p301a_superior.universitaria.completa==1 | p301a_maestria.doctorado==1,"rsuperior",
                                                                                  ifelse(p301a_inicial==1 | p301a_sin.nivel==1, "rinicial", "respecial")))))))
  df$rneduca <- final_datahot$rneduca
  
  # Nivel educativo como dummys
  # -----------------------------------------------------
  base_hot <- cbind(df["rneduca"]) 
  dummy <- dummyVars("~ .", data=base_hot, sep="_")
  final_datahot <- data.frame(predict(dummy, newdata=base_hot))
  
  df$rneduca_respecial    <- final_datahot$rneduca_respecial
  df$rneduca_rinicial     <- final_datahot$rneduca_rinicial
  df$rneduca_rprimaria    <- final_datahot$rneduca_rprimaria
  df$rneduca_rsecundaria  <- final_datahot$rneduca_rsecundaria
  df$rneduca_rsuperior    <- final_datahot$rneduca_rsuperior
  df$rneduca_rtecnica     <- final_datahot$rneduca_rtecnica
  df$rneduca              <- df$rneduca
  
  
 # df1 <- df %>% 
  #   mutate(rneduca = factor(ifelse(p301a=="sin nivel" | p301a=="inicial",1,
  #                               ifelse(p301a=="primaria incompleta" | p301a=="primaria completa",2,
  #                                      ifelse(p301a=="secundaria incompleta" | p301a=="secundaria completa",3,
  #                                             ifelse(p301a=="superior no universitaria incompleta" | p301a=="superior no universitaria completa",4,5))))))
  
  #df$rneduca <- factor(ifelse(df$p301a=="primaria incompleta" | df$p301a=="primaria completa","Primaria",
  #                                   ifelse(df$p301a=="secundaria incompleta" | df$p301a=="secundaria completa","Secundaria",
  #                                          ifelse(df$p301a=="superior no universitaria incompleta" | df$p301a=="superior no universitaria completa" | df$p301a=="superior universitaria incompleta" | df$p301a=="superior universitaria completa" | df$p301a=="maestria/doctorado","Postgrado","No-Nivel"))))
  return(df)
}

funcion_rpareja <- function(df){
  # Funcion para crear la variables dummy de rpareja
  base_hot      <- cbind(df["p209"]) 
  dummy         <- dummyVars("~ .", data=base_hot, sep="_")
  final_datahot <- data.frame(predict(dummy, newdata=base_hot))
  
  final_datahot <- final_datahot %>%  
    mutate(rcivil =factor(ifelse(p209_conviviente==1 | p209_casado.a.==1,
                                 "rpareja","rsoltero")))
  df$rcivil     <- final_datahot$rcivil
  
  base_hot      <- cbind(df["rcivil"]) 
  dummy         <- dummyVars("~ .", data=base_hot, sep="_")
  final_datahot <- data.frame(predict(dummy, newdata=base_hot))  
  
  df$rcivil           <- df$rcivil
  df$rcivil_rpareja   <- final_datahot$rcivil_rpareja
  df$rcivil_rsoltero  <- final_datahot$rcivil_rsoltero
  
  return(df)
}
  


# 5. funcion de departamentos 
funcion_rDpto <- function(data){
  
  data$rregion <- as.numeric(substr(data$ubigeo,1,2))
  regiones <- data.frame(id=c(1:25),
                         dep=c("Amazonas","Ancash", "Apurimac", "Arequipa", 
                               "Ayacucho", "Cajamarca", "Callao", "Cusco", 
                               "Huancavelica", "Huanuco", "Ica","Junin", 
                               "La Libertad","Lambayeque", "Lima", "Loreto", 
                               "Madre De Dios", "Moquegua", "Pasco", "Piura", 
                               "Puno" , "San Martin", "Tacna" , "Tumbes" , 
                               "Ucayali"))
  data$rDpto <- regiones$dep[match(data$rregion, regiones$id)]
  
  return(data)
}

# 6. funcion de edad 
funcion_redad <- function(data){
  data$redad <- data$p208a
  return(data)
}

# 7. funcion de rmujer 
funcion_rmujer <- function(data){
  
  #data$sexo <- factor(data$p207)
  data$rmujer <- factor(ifelse(data$p207=="mujer", "female", "male"))
  return(data)
  
}

# Años de educacion 
funcion_reduca <- function(data){
  # Convertimos en factor la variable p301a, p301b y p301c
  # luego podemos construir nuestra secuencia de informacion
  data$p301a <- as.numeric(factor(data$p301a))
  data$p301b <- as.numeric(factor(data$p301b))
  data$p301c <- as.numeric(factor(data$p301c))
  
  data <- data %>% 
    mutate(reduca = ifelse(p301a==1,0,
                           ifelse(p301a==2,is.na(p301b)+0,
                                  ifelse(p301a==3,is.na(p301b)+0,
                                         ifelse(p301a==4,is.na(p301b)+0,
                                                ifelse(p301a==3 & p301b==0,is.na(p301c)+0,
                                                       ifelse(p301a==4 & p301b==0,is.na(p301c)+0,
                                                              ifelse(p301a==5,is.na(p301b)+6,
                                                                     ifelse(p301a==6,is.na(p301b)+6,
                                                                            ifelse(p301a==7,is.na(p301b)+11,
                                                                                   ifelse(p301a==8,is.na(p301b)+11,
                                                                                          ifelse(p301a==9,is.na(p301b)+11,
                                                                                                 ifelse(p301a==10,is.na(p301b)+11,is.na(p301b)+16
                                                                                                        )))))))))))))
}

# Funcion de categoria de  mercado laboraal;
# - rmu       : desempleo
# - rocupado  : empleo
# - roculto   : desempleo oculto
# - rnopea    : inactivo 
funcion_rcategoria <- function(data){
  #library(caret) # using function 
  #library(mltools)
  data$rcategoria <- factor(ifelse(data$ocu500==1,"Ocupado", 
                                   ifelse(data$ocu500==2,"Desempleo",
                                          ifelse(data$ocu500==3,"Oculto",
                                                 ifelse(data$ocu500==4,"Inactico",NA))))) 
  
  base_hot  <- cbind(data["rcategoria"],data["ocupinf"])  # solo filtramos la variable (columna de interes)
  dummy     <- dummyVars("~ .", data=base_hot)            # filtramos las variables como nombre
  
  final_datahot <- data.frame(predict(dummy, newdata=base_hot)) # aplicamos la funcion de code hot
  # extraccion de variables
  data$rmu      <- final_datahot$rcategoria.Desempleo     # quedamos con la var. discreta desempleo
  data$rocupado <- final_datahot$rcategoria.Ocupado       # quedamos con la var. discreta ocupado
  data$roculto  <- final_datahot$rcategoria.Oculto        # quedamos con la var. discreta desempleo oculto
  data$rnopea   <- final_datahot$rcategoria.Inactico      # quedamos con la var. discreta Inactivo
  data$rinfo    <- final_datahot$ocupinf.empleo.informal  # quedamos con la var. discreta informalidad
  
  return(data)
  
}

# Funcion de empleo informalidad laboral
funcion_rinfo <- function(data){
  #library(caret)       # esta forma de filtrar usa la linreria caret
  #library(mltools)
  data$rcategoria <- factor(ifelse(data$ocu500==1,"Ocupado", ifelse(data$ocu500==2,"Desempleo",ifelse(data$ocu500==3,"Oculto",ifelse(data$ocu500==4,"Nopea",NA))))) 
  
  base_hot <- cbind(data["rcategoria"],data["ocupinf"])         # solo filtramos la variable (columna de interes)
  dummy <- dummyVars("~ .", data=base_hot)                      # filtramos las variables como nombre
  
  final_datahot <- data.frame(predict(dummy, newdata=base_hot)) # aplicamos la funcion de code hot
  data$rinfo <- final_datahot$ocupinf.empleo.informal           # quedamos con la var. discreta informalidad
  
  return(data)
  
}

# Indicadores de poverty
#-----------------------------------
# 8. funcion de pobreza 
funcion_rpoverty <- function(data){
  
  data$rpoverty_f <- factor(ifelse(data$pobreza   =="pobre extremo" | data$pobreza == "pobre no extremo" ,
                                   "Pobre","NoPobre"))
  data$rpoverty_c <- ifelse(data$pobreza   =="pobre extremo" | data$pobreza == "pobre no extremo" ,
                            "Pobre","NoPobre")
  data$rpoverty   <- ifelse(data$pobreza =="no pobre", 0, 1)
  
  data$rgasto <- (data$gashog2d)/12    # Variable de Gasto
  data$rpeople <- data$mieperho        # Miembros del hogar
  
  return(data)
  
}


# 9. Mesuare gasto
# 6. funcion de edad 
# funcion_rgasto <- function(data){
#   data$rgasto <- (data$gashog2d)/12
#   return(data)
# }


# 10. Miembros del hogar 
# funcion_rpeople <- function(data){
#   data$rpeople <- data$mieperho
#   return(data)
# }

# 11 Indicadores percepcion de hogar
funcion_rhogar <- function(data){
  
  data$rhogar   <- ifelse(data$p33_2=="mejoró","Mejor",
                               ifelse(data$p33_2=="esta igual","Igual","Peor"))
  data$rhogar   <- factor(data$rhogar, levels=c("Mejor", "Igual","Peor"))
  data$rvida    <- factor(data$p37) 
  return(data)
}


funcion_rjefe <- function(data){
  data$rjefe <- factor(ifelse(data$p203=="jefe/jefa","jefe","otro"))
  return(data)
}

# Funcions del modulo 100
#----------------------------------------------------

funcion_rmod100 <- function(data){
  library(caret) # using function 
  #library(mltools)
  #data$rcategoria <- factor(ifelse(data$ocu500==1,"Ocupado", ifelse(data$ocu500==2,"Desempleo",ifelse(data$ocu500==3,"Oculto",ifelse(data$ocu500==4,"Nopea",NA))))) 
  
  # define one-hot encoding function
  base_hot <- cbind(data["nbi1"],data["nbi2"],data["nbi3"],data["nbi4"],
                    data["p110"], data["p1121"])        # solo filtramos la variable (columna de interes)
  dummy <- dummyVars("~ .", data=base_hot, sep = "_")   # filtramos las variables como nombre
  
  final_datahot <- data.frame(predict(dummy, newdata=base_hot)) # aplicamos la funcion de code hot
  #data$rinfo <- final_datahot$ocupinf.empleo.informal # quedamos con la var. discreta informalidad
  
  data$rnbi1_adecuada           <- replace_na(final_datahot$nbi1_vivienda.adecuada,0)
  data$rnbi1_inadecuada         <- replace_na(final_datahot$nbi1_vivienda.inadecuada,0)
  data$rnbi2_sinhacinamiento    <- replace_na(final_datahot$nbi2_vivienda.sin.hacinamiento,0)
  data$rnbi2_conhacinamiento    <- replace_na(final_datahot$nbi2_vivienda.con.hacinamiento,0)
  data$rnbi3_consshh            <- replace_na(final_datahot$nbi3_hogares.con.vivienda.con.servicios.higiénicos,0)
  data$rnbi3_sinsshh            <- replace_na(final_datahot$nbi3_hogares.sin.vivienda.sin.servicios.higiénicos,0)
  data$rnbi4_conschool          <- replace_na(final_datahot$nbi4_hogares.con.niños.que.asisten.a.la.escuela,0)
  data$rnbi4_sinschool          <- replace_na(final_datahot$nbi4_hogares.con.niños.que.no.asisten.a.la.escuela,0)
  data$relectricidad            <- replace_na(final_datahot$p1121_electricidad,0)
  
  return(data)
  
}

funcion_r200 <- function(data){
  
  #Hmisc::describe(data$p203b)
  
  # numero de hijos 
  # data$rid_hogar  = paste0(data$conglome, data$vivienda, data$hogar)
  data            <- data %>% mutate(rid_hogar = paste0(conglome, vivienda, hogar))
  data$aux1       <- ifelse(data$p203b==3,1,0)
  data$aux1       <- replace_na(data$aux1,0)
  data            <- data %>% group_by(rid_hogar) %>% mutate(rnh = cumsum(aux1))

  # numero hijos 0 a 6
  data$aux2       <- ifelse(data$p203b==3 & data$p208a<=6,1,0)
  data$aux2       <- replace_na(data$aux2,0)
  data            <- data %>% group_by(rid_hogar) %>% mutate(rnh6 = cumsum(aux2))
  
  # numero hijos 6 a 12
  data$aux3       <- ifelse(data$p203b==3 & (data$p208a>6 & data$p208a<=12) ,1,0)
  data$aux3       <- replace_na(data$aux3,0)
  data            <- data %>% group_by(rid_hogar) %>% mutate(rnh12 = cumsum(aux3))
  
  return(data)
}

