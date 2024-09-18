#configuraciones iniciales

rm(list = ls())
gc()
options(scipen = 999)

packages <- c("tidyverse","stringi","readxl","stringr", "openxlsx") 
# librerias instaladas o no, en caso de que no esten instaladas, las instala.
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])}
invisible(lapply(packages, library, character.only = TRUE))# Carga todas las librerias
#####LLAMAR LIBRERIAS########################################################################

Sys.setlocale("LC_TIME", "es_EC.utf8")

library(readxl)
library(tidyverse)
library(stringi)
library(stringr)
library(openxlsx)
library(dplyr)

####################LEER LIBRO########################################
#################### DEPURACION VICTIMAS #############################
Sys.setlocale("LC_TIME", "es_EC.utf8")

data_denuncias_ant <- read_excel(path = "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/07. Jul/Boletín/DATA DENUNCIAS.xlsx",
                               sheet = "DATA")

data_denuncias_act <- read_excel(path = "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/08. Ago/Depuración/DENUNCIAS.xlsx",
                               sheet = "Hoja2")

data_denuncias_act <- data_denuncias_act %>%
  filter(!(year(FECHA_INGRESO_SISTEMA) == year(Sys.Date()) & month(FECHA_INGRESO_SISTEMA) == month(Sys.Date())))

# base_Nom_act <- as.data.frame(names(data_denuncias_act))
# 
# base_Nom_ant <- as.data.frame(names(data_denuncias_ant))

data_denuncias_ant_ord <- data_denuncias_ant %>% arrange(CODIGO_CASO) 
##############################CONTABILIZAR DUPLICADOS##########################


duplicados <- data_denuncias_act %>%
  filter(duplicated(CODIGO_CASO) | duplicated(CODIGO_CASO, fromLast = TRUE))


######################Depuracion de bases de datos##################

data_denuncias_ant1 <- data_denuncias_ant %>% 
  distinct(CODIGO_CASO, .keep_all = TRUE)

data_denuncias_act1 <- data_denuncias_act %>% 
  distinct(CODIGO_CASO, .keep_all = TRUE)

########HOMOGENEIZAR FECHAS##########################################

data_denuncias_ant1 <- data_denuncias_ant1 %>% 

mutate(FECHA_INGRESO_SISTEMA = as.Date(FECHA_INGRESO_SISTEMA,format ="%d%m%Y"), 
       FEC_INGRESO_DENUNCIA_DIST = as.Date(FEC_INGRESO_DENUNCIA_DIST, format ="%d%m%Y"), 
       FEC_INFRACCION = as.Date(FEC_INFRACCION, format ="%d%m%Y"), 
       FEC_DENUNCIA_FISCALIA = as.Date(FEC_DENUNCIA_FISCALIA, format ="%d%m%Y"))

data_denuncias_act1 <- data_denuncias_act1 %>% 

mutate(FECHA_INGRESO_SISTEMA = as.Date(FECHA_INGRESO_SISTEMA, format ="%d%m%Y"), 
       FEC_INGRESO_DENUNCIA_DIST = as.Date(FEC_INGRESO_DENUNCIA_DIST, format ="%d%m%Y"), 
       FEC_INFRACCION = as.Date(FEC_INFRACCION, format ="%d%m%Y"), 
       FEC_DENUNCIA_FISCALIA = as.Date(FEC_DENUNCIA_FISCALIA, format ="%d%m%Y"))

###############OBTENER VALORES NUEVOS####################################

denuncias_uni <- full_join(data_denuncias_ant1, data_denuncias_act1, by = "CODIGO_CASO") 

denuncias_nuev <- denuncias_uni %>% filter(is.na(TXT_NOM_ZONA.x)) %>% 
  select(CODIGO_CASO, 42:80)

#base_Nom <- as.data.frame(names(denuncias_uni))

names(denuncias_nuev) <- gsub("\\.y","",names(denuncias_nuev))

#####################CARGAR BASE AMIE###############################

base_AMIE <- read_excel(path = "C:/Users/juan.minchalo/Desktop/AMIE PARA ANALISIS/AMIE_DISTRIT/1MINEDUC_RegistrosAdministrativos_2023-2024_FORMAT_DEP.xlsx",
                                 sheet = "Hoja2")

base_AMIE$Zona <- sub("\\Zona ","",base_AMIE$Zona)

base_AMIE <- base_AMIE %>%  select(COD_AMIE, Zona, Provincia, Cantón, Parroquia, Cod_Distrito, Nom_Distrito, Nombre_Institución,
                     Sostenimiento)

#########################CRUZAR BASE AMIE CON DENUNCIAS Y COMPLETAR DATOS FALTANTES##############

denuncias_n_1 <- left_join(denuncias_nuev, base_AMIE, by="COD_AMIE")

denuncias_n_1$TXT_NOM_ZONA <- ifelse(is.na(denuncias_n_1$TXT_NOM_ZONA), 
                                     denuncias_n_1$Zona, denuncias_n_1$TXT_NOM_ZONA)

denuncias_n_1$PROVINCIA <- ifelse(is.na(denuncias_n_1$PROVINCIA), 
                                  denuncias_n_1$Provincia, denuncias_n_1$PROVINCIA)

denuncias_n_1$CANTON <- ifelse(is.na(denuncias_n_1$CANTON), 
                               denuncias_n_1$Cantón, denuncias_n_1$CANTON)

denuncias_n_1$PARROQUIA <- ifelse(is.na(denuncias_n_1$PARROQUIA), 
                               denuncias_n_1$Parroquia, denuncias_n_1$PARROQUIA)

denuncias_n_1$COD_AD_DISTRITO <- ifelse(is.na(denuncias_n_1$COD_AD_DISTRITO), 
                                  denuncias_n_1$Cod_Distrito, denuncias_n_1$COD_AD_DISTRITO)

denuncias_n_1$DISTRITO <- ifelse(is.na(denuncias_n_1$DISTRITO), 
                                        denuncias_n_1$Nom_Distrito, denuncias_n_1$DISTRITO)

denuncias_n_2 <- denuncias_n_1 %>%  select(-Zona, -Provincia, -Cantón, -Parroquia, -Cod_Distrito, -Nom_Distrito, -Nombre_Institución,
                                           -Sostenimiento)

denuncia_nuev_orden <- denuncias_n_2 %>% arrange(CODIGO_CASO)

###############################INCLUIR GRUPO DE EDADES################################################

denuncias_n_3 <- denuncias_n_2 %>% mutate (GRUPO_INFRACTOR = case_when(
  RELACION_INFRACTOR == "Docente" ~                         "Infractor DENTRO del sistema educativo",
  RELACION_INFRACTOR == "Estudiantes del establecimiento" ~ "Infractor DENTRO del sistema educativo",
  RELACION_INFRACTOR == "Autoridad de la IE" ~              "Infractor DENTRO del sistema educativo",
  RELACION_INFRACTOR == "Personal administrativo de la IE" ~ "Infractor DENTRO del sistema educativo",
  RELACION_INFRACTOR == "Compañero de aula" ~                "Infractor DENTRO del sistema educativo",
  RELACION_INFRACTOR == "Conserjes/Personal de limpieza" ~ "Infractor DENTRO del sistema educativo",
  RELACION_INFRACTOR == "Pariente" ~                         "Infractor FUERA del sistema educativo",
  RELACION_INFRACTOR == "Conocido no pariente" ~             "Infractor FUERA del sistema educativo",
  RELACION_INFRACTOR == "Desconocido" ~                      "Infractor FUERA del sistema educativo",
  RELACION_INFRACTOR == "Choferes de transporte escolar" ~   "Infractor FUERA del sistema educativo",
  RELACION_INFRACTOR == "Enamorado/Novio" ~                 "Infractor FUERA del sistema educativo",
  RELACION_INFRACTOR == "Otro (Especifique)" ~               "Infractor FUERA del sistema educativo",
))  

denuncias_n_3 <- denuncias_n_3 %>% select(1:22, GRUPO_INFRACTOR, 23:40)

##############################UNION DE BASE ANTIGUA CON BASE NUEVA######################################

# nom_den_n_3 <- as.data.frame(names(denuncias_n_3))
# 
# nom_data_denun <- as.data.frame(names(data_denuncias_ant1))

####ACTUALIZAR COLUMNA STS DE ACUERDO A LA NUEVA BASE##################

denuncias_final <- rbind(data_denuncias_ant1, denuncias_n_3)

denuncias_final_2 <- left_join(denuncias_final, data_denuncias_act1, by="CODIGO_CASO")

denuncias_final_3 <- denuncias_final_2 %>% select(1:28,68:72,34:40, 80)

remove_suffix <- function(names) {
  gsub("\\.x$|\\.y$", "", names)
}

denuncias_final_3 <- denuncias_final_3 %>% rename_with(remove_suffix, everything())

write.xlsx(denuncias_final_3, "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/08. Ago/Depuración/DEPURADAS/Dep_DENUNCIAS.xlsx")

####################LEER LIBRO########################################
#################### DEPURACION VICTIMAS #############################
data_victima_ant <- read_excel(path = "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/07. Jul/Boletín/DATA VICTIMAS.xlsx",
                           sheet = "DATA VICTIMAS")

data_victima_act <- read_excel(path = "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/08. Ago/Depuración/VICTIMAS.xlsx",
                               sheet = "Hoja2")

# base_Nom_ant <- as.data.frame(names(data_victima_ant))
# 
# base_Nom_act <- as.data.frame(names(data_victima_act))



data_victima_ant_ord <- data_victima_ant %>%  arrange(CODIGO_VICTIMA)
######################Depuracion de bases de datos##################

data_victima_ant1 <- data_victima_ant %>% 
  distinct(CODIGO_VICTIMA, .keep_all = TRUE)

data_victima_act1 <- data_victima_act %>% 
  distinct(CODIGO_VICTIMA, .keep_all = TRUE)

########HOMOGENEIZAR FECHAS##########################################

data_victima_ant1 <- data_victima_ant1 %>% 
  mutate(FEC_INGRESO_DENUNCIA_DIST = as.Date(FEC_INGRESO_DENUNCIA_DIST,
                                             format ="%d%m%Y"))

data_victima_act1 <- data_victima_act1 %>% 
  mutate(FEC_INGRESO_DENUNCIA_DIST = as.Date(FEC_INGRESO_DENUNCIA_DIST,
                                             format ="%d%m%Y"))

########OBTENER VALORES NUEVOS##############################################

victima_uni <- full_join(data_victima_ant1, data_victima_act1, by = "CODIGO_VICTIMA")

victima_nuev <- victima_uni %>% filter(is.na(CODIGO_CASO.x)) %>% 
  select(CODIGO_CASO.y, CODIGO_VICTIMA, 29:50)

names(victima_nuev) <- gsub("\\.y","",names(victima_nuev))

victima_nuev_ord <- victima_nuev %>%  arrange(CODIGO_VICTIMA)

#####################SELECIONAR VALORES DE DENUNCIAS###############################

base_reemp_vic <- denuncias_final %>%
  select(CODIGO_CASO, FECHA_INGRESO_SISTEMA, 
         Zona = TXT_NOM_ZONA  ,COD_CANTON, CANTON, COD_PARROQUIA, PARROQUIA, Cod_Distrito = COD_AD_DISTRITO,
         Nom_Distrito = DISTRITO)


#################OBTENER VARIABLES FALTANTES PARA LA BASE###################

victimas_n_1 <- left_join(victima_nuev, base_reemp_vic, by="CODIGO_CASO")

victimas_n_1$TXT_NOM_ZONA <- ifelse(is.na(victimas_n_1$TXT_NOM_ZONA), 
                                     victimas_n_1$Zona, victimas_n_1$TXT_NOM_ZONA)

victimas_n_1$COD_AD_DISTRITO <- ifelse(is.na(victimas_n_1$COD_AD_DISTRITO), 
                                        victimas_n_1$Cod_Distrito, victimas_n_1$COD_AD_DISTRITO)

victimas_n_1$DISTRITO <- ifelse(is.na(victimas_n_1$DISTRITO), 
                                 victimas_n_1$Nom_Distrito, victimas_n_1$DISTRITO)


victimas_n_2 <- victimas_n_1 %>% select(CODIGO_CASO, CODIGO_VICTIMA, FECHA_INGRESO_SISTEMA, TXT_NOM_VICTIMA,
                        NUM_CEDULA, NUM_EDAD, TXT_SEXO, STS_DISCAPACIDAD, TXT_NOM_DISCAPACIDAD,
                        GRADO, TXT_NOMBRE_REPRESENTANTE, TXT_CELULAR_REPRESENTANTE,
                        TXT_EMAIL_REPRESENTANTE, EXISTE_PLAN_ACOMPANAMIENTO,
                        ESTADO_CASO, FEC_INGRESO_DENUNCIA_DIST, DELITO_EDUCATIVO,
                        RELACION_INFRACTOR, TXT_NOM_ZONA, COD_PROVINCIA, PROVINCIA, 
                        COD_CANTON, CANTON, COD_PARROQUIA, PARROQUIA, COD_AD_DISTRITO,
                        DISTRITO, EXISTE_EMBARAZO)

victimas_nuev_orden <- victimas_n_2 %>% arrange(CODIGO_CASO, TXT_CELULAR_REPRESENTANTE)


##########################BASE PARA FISCALIA ##############################################################

casos_fiscalia <- full_join(denuncia_nuev_orden, victimas_nuev_orden, by = "CODIGO_CASO")

casos_fiscalia_2 <- full_join(denuncias_final_3, victimas_n_2, by = "CODIGO_CASO")

casos_fiscalia_2 <- casos_fiscalia %>%  
  filter(CODIGO_CASO > 32142)

casos_fiscalia_3 <- casos_fiscalia_2 %>%
  mutate(Grupo_infractor = case_when(RELACION_INFRACTOR.x == "Docente" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Estudiantes del establecimiento" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Autoridad de la IE" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Personal administrativo de la IE" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Conserjes/Personal de limpieza" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Compañero de aula" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Pariente" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Conocido no pariente" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Choferes de transporte escolar" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Desconocido" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Enamorado/Novio" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Otro (Especifique)" ~ "Infractor FUERA del sistema educativo",
                                     ))

base_fiscalia <- casos_fiscalia_3 %>%  select(CODIGO_CASO, CODIGO_VICTIMA, TXT_NOM_VICTIMA, NUM_CEDULA,
                                                 NUM_EDAD, TXT_SEXO, STS_DISCAPACIDAD, GRADO,
                                                 TXT_NOMBRE_REPRESENTANTE, TXT_CELULAR_REPRESENTANTE,
                                              TXT_EMAIL_REPRESENTANTE,
                                                 TXT_NOM_ZONA.y, PROVINCIA.y, CANTON.y, PARROQUIA.y, COD_AD_DISTRITO.y,
                                                 DISTRITO.y, COD_AMIE, TXT_NOM_INSTITUCION, SOSTENIMIENTO, 
                                              TXT_NOMBRES_INFRACTOR, CEDULA_INFRACTOR, NUM_EDAD_INFRACTOR,
                                              TXT_SEXO_INFRACTOR, RELACION_INFRACTOR.x, STS_DENUNCIA_FISCALIA,
                                              FEC_DENUNCIA_FISCALIA, TXT_NUM_DENUNCIA_FIS, TXT_NOM_CIUDAD_DENUNCIA,
                                              Grupo_infractor)



write.xlsx(base_fiscalia, "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/08. Ago/Depuración/DEPURADAS/BF_Jul_2024.xlsx")
#########################UNIR LA BASE ANTIGUA CON LA NUEVA###################


victimas_final <- rbind(data_victima_ant1, victimas_n_2)

victimas_final$FECHA_INGRESO_SISTEMA <- as.Date(victimas_final$FECHA_INGRESO_SISTEMA,
                                                format ="%d%m%Y")

victimas_final <- victimas_final %>% arrange(CODIGO_VICTIMA)

victimas_final_1 <- left_join(victimas_final, data_victima_act1, by = "CODIGO_VICTIMA")

victimas_final_2 <- victimas_final_1 %>% select(1:13,40, 41, 16:28)

victimas_final_3 <- victimas_final_2 %>% rename_with(remove_suffix, everything())

write.xlsx(victimas_final_3, "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/08. Ago/Depuración/DEPURADAS/Dep_VICTIMAS.xlsx")

############BASE PARA SOLICITUDES DE INFORMACIÓN###############################

victima_sol_inf <- victimas_final_3 %>%
  mutate(FECHA_ING_SIST = as.Date(FECHA_INGRESO_SISTEMA), 
         FECHA_ING_DEN_DIST = as.Date(FEC_INGRESO_DENUNCIA_DIST))

denun_sol_inf <- denuncias_final_3 %>% 
  filter (STS_ESTADO == 1) %>% 
  mutate(FECHA_ING_SIST = as.Date(FECHA_INGRESO_SISTEMA), 
         FECHA_ING_DEN_DIST = as.Date(FEC_INGRESO_DENUNCIA_DIST), 
         FECHA_INFRAC = as.Date(FEC_INFRACCION), 
         FEC_DEN_FISC = as.Date(FEC_DENUNCIA_FISCALIA)) %>% 
  
  mutate(AÑO_ING_SIST = year(FECHA_ING_SIST), MES_ING_SIST = month(FECHA_ING_SIST, label = TRUE),
         AÑO_DEN_DIST = year(FECHA_ING_DEN_DIST), MES_DEN_DIST = month(FECHA_ING_DEN_DIST, label = TRUE),
         AÑO_INFRAC = year(FEC_INFRACCION), MES_INFRAC = month(FEC_INFRACCION, label = TRUE),
         AÑO_DEN_FISC = year(FEC_DEN_FISC), MES_DEN_FISC = month(FEC_DEN_FISC, label = TRUE))


victima_sol_inf_2 <- victima_sol_inf %>% 
  mutate(FECHA_ING_SIST = as.Date(FECHA_INGRESO_SISTEMA), 
         FECHA_ING_DEN_DIST = as.Date(FEC_INGRESO_DENUNCIA_DIST)) %>% 
  
  mutate(AÑO_ING_SIST = year(FECHA_ING_SIST), MES_ING_SIST = month(FECHA_ING_SIST, label = TRUE),
         AÑO_DEN_DIST = year(FECHA_ING_DEN_DIST), MES_DEN_DIST = month(FECHA_ING_DEN_DIST, label = TRUE))


write.xlsx(denun_sol_inf, "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/08. Ago/Bases_sol_inf/data_den_fechas.xlsx")

write.xlsx(victima_sol_inf_2, "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/08. Ago/Bases_sol_inf/data_vic_fechas.xlsx")

##########################BASE PARA FISCALIA ##############################################################

casos_fiscalia_2 <- full_join(denuncias_final_3, victimas_final_3, by = "CODIGO_CASO")

casos_fiscalia_3 <- casos_fiscalia_2 %>%
  mutate(Grupo_infractor = case_when(RELACION_INFRACTOR.x == "Docente" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Estudiantes del establecimiento" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Autoridad de la IE" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Personal administrativo de la IE" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Conserjes/Personal de limpieza" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Compañero de aula" ~ "Infractor DENTRO del sistema educativo",
                                     RELACION_INFRACTOR.x == "Pariente" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Conocido no pariente" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Choferes de transporte escolar" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Desconocido" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Enamorado/Novio" ~ "Infractor FUERA del sistema educativo",
                                     RELACION_INFRACTOR.x == "Otro (Especifique)" ~ "Infractor FUERA del sistema educativo",
  ))

base_fiscalia <- casos_fiscalia_3 %>%  select(CODIGO_CASO, CODIGO_VICTIMA, TXT_NOM_VICTIMA, NUM_CEDULA,
                                              NUM_EDAD, TXT_SEXO, STS_DISCAPACIDAD, GRADO,
                                              TXT_NOMBRE_REPRESENTANTE, TXT_CELULAR_REPRESENTANTE,
                                              TXT_EMAIL_REPRESENTANTE,
                                              TXT_NOM_ZONA.y, PROVINCIA.y, CANTON.y, PARROQUIA.y, COD_AD_DISTRITO.y,
                                              DISTRITO.y, COD_AMIE, TXT_NOM_INSTITUCION, SOSTENIMIENTO, 
                                              TXT_NOMBRES_INFRACTOR, CEDULA_INFRACTOR, NUM_EDAD_INFRACTOR,
                                              TXT_SEXO_INFRACTOR, RELACION_INFRACTOR.x, STS_DENUNCIA_FISCALIA,
                                              FEC_DENUNCIA_FISCALIA, TXT_NUM_DENUNCIA_FIS, TXT_NOM_CIUDAD_DENUNCIA,
                                              Grupo_infractor)



write.xlsx(base_fiscalia, "C:/Users/juan.minchalo/Desktop/MINEDUC/0. Información Boletín/3. Reportes Jp/2024/08. Ago/Depuración/DEPURADAS/BF_AGO_2024.xlsx")
 