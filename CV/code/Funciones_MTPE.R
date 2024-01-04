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

# Variables sociodemografica
funcion_rsocio <- function(data){
  
  
  # Informacion de edad y condicion de genero (mujer)
  #------------------------------------------------------
  data <- data %>% 
    mutate(rfiltro = ifelse((p204=="si" & p205=="no") | 
                              ( p204=="no" & p206=="si") & 
                              !is.na(p501) ,1,0),
           # edad de las personas
           redad  = p208a,
           # mujer 
           rmujer = factor(ifelse(p207=="mujer",1,0))
    ) 

  
  
  # # tipo de trabajador
  #-----------------------------------------------
  data <- data %>%
    mutate(r8 = ifelse(ocu500==1 & as.numeric(p507)==1, 1,
                       ifelse(ocu500==1 & as.numeric(p507)==3 & as.numeric(p510)>=3 & as.numeric(p510)<=7, 2,
                              ifelse(ocu500==1 & as.numeric(p507)==3 & as.numeric(p510)>=1 & as.numeric(p510)<=2,3,
                                     ifelse(ocu500==1 & as.numeric(p507)==4 & as.numeric(p510)>=3 & as.numeric(p510)<=7,4,
                                            ifelse(ocu500==1 & as.numeric(p507)==4 & as.numeric(p510)>=1 & as.numeric(p510)<=2,5,
                                                   ifelse(ocu500==1 & as.numeric(p507)==2,6,
                                                          ifelse(ocu500==1 & (as.numeric(p507)==5 | as.numeric(p507)==7),7,
                                                                 ifelse(ocu500==1 & as.numeric(p507)==6,8,9)))))))))

  data <- data %>%
    mutate(r8r = ifelse(r8==1,"Empleador",
                        ifelse(r8==2 | r8==4,"Asalariado privado",
                               ifelse(r8==3 | r8==5,"Asalariado publico",
                                      ifelse(r8==6,"Independiente",
                                             ifelse(r8==7,"TFNR",
                                                    ifelse(r8==8,"Trabajadora hogar","No Ocupado")))))))
  data <- data %>%
    mutate(r8r = factor(r8r, levels = c("Empleador",
                                         "Asalariado privado",
                                         "Asalariado publico",
                                         "Independiente",
                                         "TFNR",
                                         "Trabajadora hogar",
                                         "No Ocupado"
                                         )))
  
  # Departamentos
  #------------------------------------------------
  # data$rregion <- as.numeric(substr(data$ubigeo,1,2))
  # regiones     <- data.frame(id=c(1:25),
  #                            dep=c("Amazonas","Ancash", "Apurimac", "Arequipa",
  #                              "Ayacucho", "Cajamarca", "Callao", "Cusco",
  #                              "Huancavelica", "Huanuco", "Ica","Junin",
  #                              "La Libertad","Lambayeque", "Lima", "Loreto",
  #                              "Madre De Dios", "Moquegua", "Pasco", "Piura",
  #                              "Puno" , "San Martin", "Tacna" , "Tumbes" ,
  #                              "Ucayali"))
  # data  <- merge(data, regiones, 
  #                by.x = c("rregion"),
  #                by.y = c("id"),
  #                all.x = TRUE)
  # data$rDpto <- data$dep
  
  # data$rDpto <- regiones$dep[match(data$rregion, regiones$id)]  
  
  # Sectores economicos
  # data < data %>% 
  #   mutate(r5r4mtpe = ifelse(ocu500==1 & (p506r4>=100 & p506r4<=399), "Agricultura, ganadería, silvicultura y pesca", 
  #                            ifelse(ocu500==1 & (p506r4>=500 & p506r4<=999),"Minería",
  #                                   ifelse(ocu500==1 & (p506r4==1010 | p506r4==1020 | p506r4==1030 | p506r4==1040 
  #                                                       | p506r4==1050 | p506r4==1061 | p506r4==1062 | 
  #                                                         (p506r4>=1071 & p506r4<=1075) | p506r4==1079 | 
  #                                                         p506r4==1080 | (p506r4>=1101 & p506r4<=1104) | 
  #                                                         p506r4==1200 | (p506r4>=1311 & p506r4<=1313) | 
  #                                                         (p506r4>=1391 & p506r4<=1394) | p506r4==1399 | 
  #                                                         p506r4==1410 | p506r4==1420 | p506r4==1430 | 
  #                                                         p506r4==1512 | p506r4==1520 | p506r4==1629  | 
  #                                                         p506r4==1709 | p506r4==1811 | p506r4==1812 | 
  #                                                         p506r4==2219 | p506r4==2220 | p506r4==2299 | 
  #                                                         p506r4==2599 | p506r4==2640 | p506r4==2651 | 
  #                                                         p506r4==2652 | p506r4==2670 | p506r4==2731 | 
  #                                                         p506r4==2733 | p506r4==2817 | p506r4==2930 | 
  #                                                         p506r4==3092 | p506r4==3100 | p506r4==3211 | 
  #                                                         p506r4==3212 | p506r4==3220 | p506r4==3230 | 
  #                                                         p506r4==3240 | p506r4==3250 | p506r4==3290 | 
  #                                                         p506r4==3319),"Industria de bienes de consumo",
  #                                          ifelse(ocu500==1 & (p506r4==1511 | p506r4==1610 | (p506r4>=1621 & p506r4<=1623) | 
  #                                                                p506r4==1701 | p506r4==1702 | p506r4==1820 | p506r4==1910 | 
  #                                                                p506r4==1920 | (p506r4>=2011 & p506r4<=2013) | 
  #                                                                (p506r4>=2021 & p506r4<=2023) | p506r4==2029 | 
  #                                                                p506r4==2030 | p506r4==2100 | 
  #                                                                p506r4==2211 | p506r4==2310 | 
  #                                                                (p506r4>=2391 & p506r4<=2396) | 
  #                                                                p506r4==2399 | p506r4==2420 | 
  #                                                                (p506r4>=2431 & p506r4<=2432) | p506r4==2591 | 
  #                                                                p506r4==2592 | p506r4==2610 | p506r4==2680 | 
  #                                                                p506r4==2812),"Industria de bienes intermedios",
  #                                                 ifelse(ocu500==1 & (p506r4==2410 | (p506r4>=2511 & p506r4<=2513) | 
  #                                                                       p506r4==2520 | p506r4==2593 | p506r4==2620 | 
  #                                                                       p506r4==2630 | p506r4==2660 | 
  #                                                                       p506r4==2710 | p506r4==2720 | 
  #                                                                       p506r4==2732 | p506r4==2740 | 
  #                                                                       p506r4==2750 | p506r4==2790 | 
  #                                                                       p506r4==2811 | (p506r4>=2813 & p506r4<=2816) | 
  #                                                                       p506r4==2818 | p506r4==2819 | (p506r4>=2821 & p506r4<=2826) | 
  #                                                                       p506r4==2829 | p506r4==2910 | p506r4==2920 | p506r4==2999 | 
  #                                                                       p506r4==3011 | p506r4==3012 | p506r4==3020 | p506r4==3030 | 
  #                                                                       p506r4==3040 | p506r4==3091 | p506r4==3099 | 
  #                                                                       (p506r4>=3311 & p506r4<=3315) | p506r4==3320),"Industria de bienes de capital",
  #                                                        ifelse(ocu500==1 & (p506r4>=3500 & p506r4<=3999),"Electricidad, gas, agua y saneamiento",
  #                                                               ifelse(ocu500==1 & (p506r4>=4100 & p506r4<=4399),"Construcción",
  #                                                                      ifelse(ocu500==1 & (p506r4>=4600 & p506r4<=4699),"Comercio al por mayor",
  #                                                                             ifelse(ocu500==1 & ((p506r4>=4500 & p506r4<=4599) | 
  #                                                                                                   (p506r4>=4700 & p506r4<=4799)),"Comercio al por menor",
  #                                                                                    ifelse(ocu500==1 & (p506r4>=5500 & p506r4<=5699),"Restaurantes y hoteles",
  #                                                                                           ifelse(ocu500==1 & ((p506r4>=4900 & p506r4<=5399) | 
  #                                                                                                                 (p506r4>=5800 & p506r4<=6399)),"Transporte, almacenamiento y comunicaciones",
  #                                                                                                  ifelse(ocu500==1 & (p506r4>=6400 & p506r4<=8299),"Establecimientos financieros, seguros, bienes inmuebles y servicios prestados a empresas",
  #                                                                                                         ifelse(ocu500==1 & ((p506r4>=8400 & p506r4<=9499) | 
  #                                                                                                                               (p506r4>=9900 & p506r4<=9999)),"Servicios comunitarios, sociales y recreativos",
  #                                                                                                                ifelse(ocu500==1& (p506r4>=9500 & p506r4<=9699),"Servicios personales",
  #                                                                                                                       ifelse(ocu500==1 & (p506r4>=9700 & p506r4<=9799),"Hogares","No Pea"))))))))))))))))
  # 
  # data <- data %>%
  #   mutate(r5r4mtpe = factor(r5r4mtpe, levels = c(
  #     "Agricultura, ganadería, silvicultura y pesca",
  #     "Minería",
  #     "Industria de bienes de consumo",
  #     "Industria de bienes intermedios",
  #     "Industria de bienes de capital", 
  #     "Electricidad, gas, agua y saneamiento", 
  #     "Construcción",
  #     "Comercio al por mayor",
  #     "Comercio al por menor",
  #     "Restaurantes y hoteles",
  #     "Transporte, almacenamiento y comunicaciones",
  #     "Establecimientos financieros, seguros, bienes inmuebles y servicios prestados a empresas",
  #     "Servicios comunitarios, sociales y recreativos",
  #     "Servicios personales",
  #     "Hogares",
  #     "No Pea")))
  # 
  # 
  # data <- data %>% 
  #   mutate(r5r4mtpe2= ifelse(ocu500==1 & r5r4mtpe==1, "Agricultura, ganadería, silvicultura y pesca",
  #                            ifelse(ocu500==1 & r5r4mtpe==2, "Minería",
  #                                   ifelse(ocu500==1 & r5r4mtpe==3, "Industria de bienes de consumo",
  #                                          ifelse(ocu500==1 
  #                                                 & r5r4mtpe>=4 
  #                                                 & r5r4mtpe<=5, "Industria de bienes intermedios y de capital",
  #                                                 ifelse(ocu500==1 & r5r4mtpe==7, "Construcción",
  #                                                        ifelse(ocu500==1 
  #                                                               & r5r4mtpe>=8 
  #                                                               & r5r4mtpe<=9, "Comercio",
  #                                                               ifelse(ocu500==1 
  #                                                                      & r5r4mtpe==6 
  #                                                                      | (r5r4mtpe>=11 
  #                                                                         & r5r4mtpe<=13), "Servicios no personales",
  #                                                                      ifelse(ocu500==1 
  #                                                                             & r5r4mtpe==10 
  #                                                                             | r5r4mtpe==14, "Servicios personales",
  #                                                                             ifelse(ocu500==1 
  #                                                                                    & r5r4mtpe==15, "Hogares","No Pea"))))))))))
  # 
  # data <- data %>%
  #   mutate(r5r4mtpe2 = factor(r5r4mtpe2, levels = c(
  #     "Agricultura, ganadería, silvicultura y pesca",
  #     "Minería",
  #     "Industria de bienes de consumo",
  #     "Industria de bienes intermedios y de capital",
  #     "Construcción",
  #     "Comercio",
  #     "Servicios no personales",
  #     "Servicios personales",
  #     "Hogares",
  #     "No Pea")))
  
  
  
  
}

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
  
  # Horas
  data <- data %>%
    mutate(rhoras = ifelse(p519 == "si" & ocu500 == 1, 
                           rowSums(select(., i513t, i518)), NA),
           rhoras = ifelse(p519 == "no" & ocu500 == 1, i520, rhoras))
  
  
  # Funcion para crear la variables dummy de rpareja
  #---------------------------------------------------------
  base_hot      <- cbind(data["p209"]) 
  dummy         <- dummyVars("~ .", data=base_hot, sep="_")
  final_datahot <- data.frame(predict(dummy, newdata=base_hot))
  
  final_datahot <- final_datahot %>%  
    mutate(rcivil =factor(ifelse(p209_conviviente==1 | p209_casado.a.==1,
                                 "rpareja","rsoltero")))
  data$rcivil     <- final_datahot$rcivil
  
  base_hot      <- cbind(data["rcivil"]) 
  dummy         <- dummyVars("~ .", data=base_hot, sep="_")
  final_datahot <- data.frame(predict(dummy, newdata=base_hot))  
  
  data$rcivil           <- data$rcivil
  data$rcivil_rpareja   <- final_datahot$rcivil_rpareja
  data$rcivil_rsoltero  <- final_datahot$rcivil_rsoltero
  
  # Nivel educativo
  #-----------------------------------------
  base_hot <- cbind(data["p301a"]) 
  dummy <- dummyVars("~ .", data=base_hot, sep="_")
  final_datahot <- data.frame(predict(dummy, newdata=base_hot))
  
  final_datahot <- final_datahot %>% 
    mutate(rneduca =factor(ifelse(p301a_primaria.incompleta==1 | 
                                    p301a_primaria.completa==1 ,"rprimaria",
                                  ifelse(p301a_secundaria.incompleta==1 | 
                                           p301a_secundaria.completa==1,"rsecundaria",
                                         ifelse(p301a_superior.no.universitaria.incompleta==1 | 
                                                  p301a_superior.no.universitaria.completa==1,"rtecnica",
                                                ifelse(p301a_superior.universitaria.incompleta==1 | 
                                                         p301a_superior.universitaria.completa==1 | 
                                                         p301a_maestria.doctorado==1,"rsuperior",
                                                       ifelse(p301a_inicial==1 | 
                                                                p301a_sin.nivel==1, "rinicial", 
                                                              "respecial")))))))
  data$rneduca <- final_datahot$rneduca
  
  # Nivel educativo como dummys
  # -----------------------------------------------------
  base_hot <- cbind(data["rneduca"]) 
  dummy <- dummyVars("~ .", data=base_hot, sep="_")
  final_datahot <- data.frame(predict(dummy, newdata=base_hot))
  
  data$rneduca_respecial    <- final_datahot$rneduca_respecial
  data$rneduca_rinicial     <- final_datahot$rneduca_rinicial
  data$rneduca_rprimaria    <- final_datahot$rneduca_rprimaria
  data$rneduca_rsecundaria  <- final_datahot$rneduca_rsecundaria
  data$rneduca_rsuperior    <- final_datahot$rneduca_rsuperior
  data$rneduca_rtecnica     <- final_datahot$rneduca_rtecnica
  data$rneduca              <- data$rneduca

  
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
  
  final_datahot <- final_datahot %>% 
    mutate(rneduca =factor(ifelse(p301a_primaria.incompleta==1 | 
                                    p301a_primaria.completa==1 ,"rprimaria",
                                  ifelse(p301a_secundaria.incompleta==1 | 
                                           p301a_secundaria.completa==1,"rsecundaria",
                                         ifelse(p301a_superior.no.universitaria.incompleta==1 | 
                                                  p301a_superior.no.universitaria.completa==1,"rtecnica",
                                                ifelse(p301a_superior.universitaria.incompleta==1 | 
                                                         p301a_superior.universitaria.completa==1 | 
                                                         p301a_maestria.doctorado==1,"rsuperior",
                                                       ifelse(p301a_inicial==1 | 
                                                                p301a_sin.nivel==1, "rinicial", 
                                                              "respecial")))))))
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

