
# 1. Instalo y abro paquetes -------------------------------------------------
# install.packages("pacman")

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer, 
               datos,  knitr, gt, summarytools, ggthemes, hrbrthemes, foreign, DescTools, ineq)


# 2. Importo archivo y lo asigno a environment ----------------------------
base_antropologia <- read.xlsx("encuesta-antropologia-2025.xlsx")
libro_codigos<- read.xlsx("encuesta-antropologia-2025.xlsx") # dejo una base sin limpiar para observar nombres de preguntas

#Explorar
glimpse(base_antropologia) #Una primera mirada de lo que hay en mis datos, la primera fila es extraña, dice "respuesta" o repite el nombre de la variable.
names(base_antropologia) #observo que hay puntos, mayúsculas y minúsculas, etcétera. Está sucia


# 3. Data Wrangling -------------------------------------------------------

#3.1. Limpieza inicial ####
base_antropologia <- janitor::clean_names(base_antropologia) #con esto transformo todo a minúscula, quito tildes, saco signos, borro espacios
names(base_antropologia) # queda mucho mejor

#3.2.Acorto nombre de variables ####
names(base_antropologia)

#posibilidad de renombrar uno por uno las variables de interés. # primero nuevo nombre y luego nombre antiguo
#estructura: base_datos <- base_datos %>% dplyr::rename(nombrenuevo=nombre_antiguo,nombre_nuevo=nombre_antiguo)
names(base_antropologia)

#cambio preguntas de salud mental que son muy largas

base_antropologia <- base_antropologia %>% dplyr::rename(s_me_tris = s_me_01_en_las_ultimas_dos_semanas_con_que_frecuencia_ha_experimentado_los_siguientes_tres_sintomas_tristeza,
s_me_ansi = s_me_01_en_las_ultimas_dos_semanas_con_que_frecuencia_ha_experimentado_los_siguientes_tres_sintomas_ansiedad, 
s_me_estre = s_me_01_en_las_ultimas_dos_semanas_con_que_frecuencia_ha_experimentado_los_siguientes_tres_sintomas_estres)

names(base_antropologia)

#acorto a 10 caracteres para que sea más fácil trabajar
names(base_antropologia) <- substring(names(base_antropologia), 1, 10)


names(base_antropologia)
names(libro_codigos)

#renombro las variables de identificación y las sociodemográficas
base_antropologia <- base_antropologia %>%
  rename(
    mail = escriba_su,
    nombre_encuestado = indique_su,
    encuestador   = cual_es_el,
    edad              = sd_01_que_,
    genero            = sd_02_se_e,
    anio_ingreso      = sd_03_en_q,
    comuna_rm         = sd_04_en_q,
    rm_dis            = sd_05_prov,
    reg               = sd_06_de_q,
    tiempo_u          = sd_07_cuan,
    nacionalidad      = sd_08_indi,
    origen_padre      = sd_09_p_in,
    origen_madre      = sd_09_m_in,
    pueblo_o          = sd_10_pert,
    pueblo_os         = sd_10_o_en,
    ne_p              = sd_11_indi,
    ne_m              = sd_12_indi,
    clase_social      = sd_13_en_l,
    fut_laboral_1     = sd_14_a_un,
    fut_laboral_2     = sd_14_b_un
  )

names(base_antropologia)


# filtrar filas donde el mail aparece al menos dos veces
# Observo duplicados por mail

base_antropologia %>%
  group_by(mail) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(mail)

# hay 8 casos repetidos por mail.

# elimino duplicados por mail: elimino cuándo contestó primero. 
base_antropologia <- base_antropologia %>%
  group_by(mail) %>%
  slice_max(marca_temp, n = 1, with_ties = FALSE) %>%
  ungroup()

#veo casos repetidos por nombre_encuestado
names(base_antropologia)
base_antropologia %>%
  group_by(nombre_encuestado) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(nombre_encuestado)

#jamadues contestó 2 veces. Elimino la priemra ves.

base_antropologia <- base_antropologia %>%
  filter(mail != "jamadues")

#ahora observo cantidad de casos por encuestador
table(base_antropologia$encuestador)

# 1) Crea la tabla de frecuencias
t <- table(base_antropologia$encuestador)

# 2) Reordénala por nombre (alfabéticamente)
t_alfabetico <- t[order(names(t))]

# 3) Muéstrala
t_alfabetico

#observo que Valeria Alejandra ,Verdugo Monardes se repite dos veces. Recodifico y uno. 

base_antropologia <- base_antropologia %>%
  mutate(
    encuestador = recode(
      encuestador,
      "Valeria Alejandra ,Verdugo Monardes" = "Valeria Alejandra Verdugo Monardes"
    )
  )


# normalizo el mail y el nombre_encuestado para ver si hay otros casos repetidos que se me fueron. 

base_antropologia <- base_antropologia %>%
  mutate(
    # Normalizar y formatear mail
    mail = stringi::stri_trans_general(mail, "Latin-ASCII"),
    mail = tolower(mail),
    mail = gsub(" ", "_", mail),
    
    # Normalizar y formatear nombre_encuestado
    nombre_encuestado = stringi::stri_trans_general(nombre_encuestado, "Latin-ASCII"),
    nombre_encuestado = tolower(nombre_encuestado),
    nombre_encuestado = gsub(" ", "_", nombre_encuestado)
  )



base_antropologia <- base_antropologia %>%
  arrange(encuestador)



base_antropologia <- base_antropologia %>%
  # 1) Asegurarte de que marca_temp sea numérico (si viene como factor o chr)
  mutate(marca_temp = as.numeric(as.character(marca_temp))) %>%
  # 2) Ordenar de modo descendente según marca_temp
  arrange(desc(marca_temp)) %>%
  # 3) Quedarte con la primera aparición de cada mail (la de mayor marca_temp)
  distinct(mail, .keep_all = TRUE)


#hago un gráfico con casos por encuestador
# 1. Calcular frecuencias y ordenar
df_plot <- base_antropologia %>%
  count(encuestador) %>%
  arrange(desc(n)) %>%
  mutate(encuestador = factor(encuestador, levels = encuestador))

# 2. Dibujar gráfico
ggplot(df_plot, aes(x = encuestador, y = n)) +
  geom_col(fill = "tomato") +
  theme_minimal() +
  labs(
    x     = "Encuestador/a",
    y     = "Conteo",
    title = "Frecuencia de Encuestadores"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


names(base_antropologia)


### Elimino los casos de los estudiantes que son de psicología

sort(unique(base_antropologia$mail))


base_antropologia <- base_antropologia %>%
  filter(
    !mail %in% c(
      "p.parkertobarr@gmail.com",
      "werrwilliam5@gmail.com"
    )
  )




################# EMPEZAMOS EL PROCESAMIENTO ############################


names(base_antropologia)


# 4.1 Variable Edad####
# Realizada Amilcar

#Rename sd_03
#primero la cambio el nombre a la variable
base_antropologia <- base_antropologia %>% dplyr::rename (edad =sd_03)
names(base_antropologia)
unique(base_antropologia$edad)

#Proceso de recodificación
base_antropologia <- base_antropologia %>% mutate(edad=case_when(edad=="23.0"~"23",
                                                                 edad=="20.0"~"20",  
                                                                 edad=="22.0"~"22",
                                                                 edad=="24.0"~"24",
                                                                 edad=="21.0"~"21",
                                                                 edad=="21 años"~"21", 
                                                                 edad=="23 años"~"23",
                                                                 edad=="18.0"~"18",
                                                                 edad=="41.0"~"41",
                                                                 edad=="28.0"~"28",
                                                                 edad=="19.0"~"19",
                                                                 edad=="27.0"~"27",
                                                                 edad=="20 años"~"20",
                                                                 edad=="30.0"~"30",
                                                                 edad=="25.0"~"25",
                                                                 edad=="26.0"~"26",
                                                                 edad=="22 años"~"22", 
                                                                 edad=="20 años "~"20",
                                                                 edad=="19 años "~"19",
                                                                 edad=="18 años"~"18",
                                                                 edad=="31.0"~"31",
                                                                 edad=="40.0"~"40",
                                                                 TRUE ~ edad))
unique(base_antropologia$edad)

#ahora construyo una nueva variable con rangos
base_antropologia$edad <- as.numeric(base_antropologia$edad)
class(base_antropologia$edad)

base_antropologia <- base_antropologia %>% 
  mutate (edad_r= case_when (edad %in% c(18:20) ~ "18 a 20", 
                             edad %in% c(21:23) ~ "21 a 23", 
                             edad %in% c(24:29) ~ "24 a 29", 
                             edad >= 30 ~ "30 o más"))
#Observo lo realizado
unique(base_antropologia$edad_r)
table(base_antropologia$edad_r)


# 4.2Variable Identidad de Género ####
# Realizada por Amilcar

unique(base_antropologia$sd_02) # NOEL 

#Rename sd_02
base_antropologia <- base_antropologia %>% dplyr::rename (identidad_genero =sd_02)
unique(base_antropologia$identidad_genero)

#Recodifico en 3 grupos
base_antropologia<- base_antropologia %>%
  mutate(identidad_genero_r= case_when(
    identidad_genero %in% c("Hombre cisgénero") ~ "Hombre cisgenero",
    identidad_genero %in% c("Mujer cisgénero") ~ "Mujer cisgenero",
    identidad_genero %in% c("No binarie",                 
                            "Agénero", "Género fluido", "Ninguno", "Hombre trans/transmasculino", "Mujer trans/transfemenina") ~ "Persona de genero diverso"))

#Observo lo realizado
unique(base_antropologia$identidad_genero_r)
table(base_antropologia$identidad_genero_r)




# 4.3 Variable Ingreso a Carrera####
# Realizada por Amilcar
#Observo
unique(base_antropologia$sd_04) 

#Renombro
base_antropologia <- base_antropologia %>% dplyr::rename(año_ingreso_carrera = sd_04)

#Recodifico
base_antropologia <- base_antropologia %>%
  mutate(año_ingreso_carrera_r=case_when(año_ingreso_carrera == 2019 ~ "pre-pandemia",
                                       año_ingreso_carrera == 2020 ~ "Pandemia",
                                       año_ingreso_carrera == 2021 ~ "Pandemia",
                                       año_ingreso_carrera == 2022 ~ "post-pandemia",
                                       año_ingreso_carrera == 2023 ~ "post-pandemia",
                                       año_ingreso_carrera == 2024 ~ "post-pandemia"
                                       ))

#Observo lo realizado
unique(base_antropologia$año_ingreso_carrera_r)
table(base_antropologia$año_ingreso_carrera_r)








# 4.4 Variable Comuna de Residencia ####
# Realizada por Javiera
unique(base_antropologia$sd_05) 

#primero la cambio el nombre a la variable


freq(base_antropologia$comuna, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()



# recodifico por distancia a la universidad
base_antropologia <- base_antropologia %>%
  mutate(comuna_distancia = case_when(
    comuna %in% c("santiago_centro", "providencia", "estacion_central", "quinta_normal", "recoleta", "santa_lucia") ~ "Vive muy cerca",
    comuna %in% c("nunoa", "san_miguel", "la_cisterna", "conchali", "lo_prado", "pedro_aguirre_cerda", "la_granja", "lo_espejo") ~ "Vive a distancia cercana",
    comuna %in% c("macul", "la_florida", "penalolen", "maipu", "pudahuel", "san_joaquin", "renca", "cerro_navia", "quilicura", "huechuraba", "vitacura", "las_condes", "la_reina") ~ "Vive a distancia media",
    comuna %in% c("puente_alto", "la_pintana", "san_bernardo", "buin", "talagante", "penaflor", "curacavi", "lampa", "melipilla", "calera_de_tango", "til_til", "paine") ~ "Vive a mucha distancia",
    TRUE ~ "Fuera de Santiago" # Para cualquier comuna no listada
  ))


freq(base_antropologia$comuna_distancia, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()



# 4.5 Fuera RM ####
# Realizada por Javiera


# 4.6 reg ####
# Realizada por Javiera







##############ACÁ VOY###########



# 3.3.6. Variable Clase Social####
# Realizada por Sebastián 
unique(base_antropologia$sd_06) 

#renombro
base_antropologia <- base_antropologia %>% dplyr::rename(clase_social = sd_06)
unique(base_antropologia$clase_social) 
table(base_antropologia$clase_social)

#Ordeno las categorías porque son ordinales
class(base_antropologia$clase_social) # están en "character"

base_antropologia$clase_social <- base_antropologia$clase_social %>% fct_relevel(c("Clase social baja", "Clase social media - baja", "Clase social media", "Clase social media - alta")) 
class(base_antropologia$clase_social) # ahora están ordenadas y en "factor"

#Observo lo realizado
table(base_antropologia$clase_social)

# 3.3.7. Variable Educación Figura Paterna####
# Realizada por: Joaquín
# Observo Categorías
unique(base_antropologia$sd_07) 

# Quito valores en español, cambio espacios por guión y elimino -
base_antropologia <- base_antropologia %>%
  mutate(
    sd_07 = stringi::stri_trans_general(sd_07, "Latin-ASCII"),
    sd_07 = tolower(sd_07),  
    sd_07 = gsub(" ", "_", sd_07),
    sd_07 = gsub("-+$", "", sd_07),
  )

# renombro
base_antropologia <- base_antropologia %>% dplyr::rename(nivel_educacion_padre= sd_07)


base_antropologia <- base_antropologia %>%
  mutate(nivel_educacion_padre=case_when(nivel_educacion_padre ==  "profesional_(carreras_4_o_mas_anos)" ~ "Educación Profesional",
                                         nivel_educacion_padre == "magister_o_maestria" ~ "Educación Profesional",
                                         nivel_educacion_padre == "doctorado" ~ "Educación Profesional",
                                         nivel_educacion_padre == "tecnico_nivel_superior_(carreras_1_a_3_anos)" ~"Educación Técnica",
                                         nivel_educacion_padre ==  "educacion_media_tecnica_profesional" ~ "Educación Técnica",
                                         nivel_educacion_padre == "educacion_media" ~ "Educación Media",
                                         nivel_educacion_padre == "educacion_basica_" ~ "Educación Básica",
                                         nivel_educacion_padre == "ensenanza_basica_completa" ~ "Educación Básica",
                                         nivel_educacion_padre == "educacion_basica_completa_" ~ "Educación Básica",
                                         nivel_educacion_padre == "educacion_basica_hasta_sexto_" ~ "Educación Básica",
                                         nivel_educacion_padre == "no_se_"  ~ NA,
                                         nivel_educacion_padre == "no_se" ~ NA,
                                         nivel_educacion_padre == "sin_figura_paterna_" ~ NA,
                                         TRUE ~ nivel_educacion_padre))

#Observo lo realizado
unique(base_antropologia$nivel_educacion_padre)
table(base_antropologia$nivel_educacion_padre)

#Ordeno las categorías porque son ordinales
class(base_antropologia$nivel_educacion_padre) # están en "character"

base_antropologia$nivel_educacion_padre <- base_antropologia$nivel_educacion_padre %>% fct_relevel(c("Educación Básica", "Educación Media", "Educación Técnica", "Educación Profesional")) 
class(base_antropologia$nivel_educacion_padre) # ahora están ordenadas y en "factor"

#Observo lo realizado
table(base_antropologia$nivel_educacion_padre)

# 3.3.8. Variable Educación Figura Materna####
# Realizado por: Joaquín
# Observo:
unique(base_antropologia$sd_08) 

# Quito valores en español, cambio espacios por guión y elimino -
base_antropologia <- base_antropologia %>%
  mutate(
    sd_08 = stringi::stri_trans_general(sd_08, "Latin-ASCII"),
    sd_08 = tolower(sd_08),  
    sd_08 = gsub(" ", "_", sd_08),
    sd_08 = gsub("-+$", "", sd_08),
  )

# renombro
base_antropologia <- base_antropologia %>% dplyr::rename(nivel_educacion_madre= sd_08)

# recodifico
base_antropologia <- base_antropologia %>%
  mutate(nivel_educacion_madre=case_when(nivel_educacion_madre ==  "profesional_(carreras_4_o_mas_anos)" ~ "Educación Profesional",
                                         nivel_educacion_madre == "magister_o_maestria" ~ "Educación Profesional",
                                         nivel_educacion_madre == "doctorado" ~ "Educación Profesional",
                                         nivel_educacion_madre == "tecnico_nivel_superior_(carreras_1_a_3_anos)" ~"Educación Técnica",
                                         nivel_educacion_madre ==  "educacion_media_tecnica_profesional" ~ "Educación Técnica",
                                         nivel_educacion_madre == "educacion_media" ~ "Educación Media",
                                         nivel_educacion_madre == "profesional_incompleto" ~ "Educación Media",
                                         nivel_educacion_madre == "educacion_media_incompleta" ~ "Educación Básica",
                                         TRUE ~ nivel_educacion_madre))

unique(base_antropologia$nivel_educacion_madre)

table(base_antropologia$nivel_educacion_madre)


#Ordeno las categorías porque son ordinales
class(base_antropologia$nivel_educacion_madre) # están en "character"

base_antropologia$nivel_educacion_madre <- base_antropologia$nivel_educacion_madre %>% fct_relevel(c("Educación Básica", "Educación Media", "Educación Técnica", "Educación Profesional")) 
class(base_antropologia$nivel_educacion_madre) # ahora están ordenadas y en "factor"

#Observo lo realizado
table(base_antropologia$nivel_educacion_madre)


# 3.3.9. Variable último Colegio####
unique(base_antropologia$sd_09)

#renombro
base_antropologia <- base_antropologia %>% dplyr::rename(ultimo_colegio = sd_09)
unique(base_antropologia$ultimo_colegio) 
table(base_antropologia$ultimo_colegio)

#Ordeno las categorías porque son ordinales
class(base_antropologia$ultimo_colegio) # están en "character"

base_antropologia$ultimo_colegio <- base_antropologia$ultimo_colegio %>% fct_relevel(c("Público", "Particular subvencionado", "Particular")) 
class(base_antropologia$ultimo_colegio) # ahora están ordenadas y en "factor"

#Observo lo realizado
table(base_antropologia$ultimo_colegio)


#3.4.Variables de Estrés académico####

#3.4.1. Horas de estudio semana ####
# realizado por: Noel 

class(base_antropologia$ea_01) # transformar en factor y ordenar: NOEL 

#primero la cambio el nombre a la variable
base_antropologia <- base_antropologia %>% dplyr::rename (ea_01_horas_estudio_semana = "ea_01"  )

unique(base_antropologia$ea_01_horas_estudio_semana)
table(base_antropologia$ea_01_horas_estudio_semana)

#cambio a factor
class(base_antropologia$ea_01_horas_estudio_semana)

base_antropologia <- base_antropologia %>%
  mutate(horas_estudio_semana = as.factor(ea_01_horas_estudio_semana))


#3.4.2. Horas de estudio fin de semana ####
# realizado por: Noel 

unique(base_antropologia$ea_02) 

#primero la cambio el nombre a la variable
base_antropologia <- base_antropologia %>% dplyr::rename (ea_02_horas_estudio_fin_semana = "ea_02")
names(base_antropologia)

#cambio a factor
class(base_antropologia$ea_02_horas_estudio_fin_semana)

base_antropologia <- base_antropologia %>%
  mutate(ea_02_horas_estudio_fin_semana = as.factor(ea_02_horas_estudio_fin_semana))

#Observo tabla
table(base_antropologia $ea_02_horas_estudio_fin_semana)


#3.4.3. Carga académica actual  ####
# realizado por: Samanta
unique(base_antropologia$ea_03) 

# renombro
base_antropologia <- base_antropologia %>% dplyr::rename(ea_03_descripcion_carga_academica = ea_03)
names(base_antropologia)
class(base_antropologia$ea_03_descripcion_carga_academica)

# Ordeno
base_antropologia$ea_03_descripcion_carga_academica <- base_antropologia$ea_03_descripcion_carga_academica %>% fct_relevel(c("Ligera", "Moderada", "Pesada", "Muy pesada")) 
class(base_antropologia$ea_03_descripcion_carga_academica) # ahora están ordenadas y en "factor"

table(base_antropologia$ea_03_descripcion_carga_academica)

#3.4.4. Notas Último Semestre - FALTA  ####
# realizado por: Matías
unique(base_antropologia$ea_04) # recodificar a número, recodificar a rangos: MATIAS


base_antropologia <- base_antropologia %>% dplyr::rename(ea_04_notas_ultimo_semestre= ea_04)
unique(base_antropologia$ea_04_notas_ultimo_semestre)
class(base_antropologia$ea_04_notas_ultimo_semestre)

base_antropologia <- base_antropologia %>%
  mutate(ea_04_notas_ultimo_semestre= factor(ea_04_notas_ultimo_semestre))


base_antropologia <- base_antropologia %>%
  mutate(ea_04_notas_ultimo_semestre=case_when(ea_04_notas_ultimo_semestre == "Alrededor de 5,8 creo " ~ "5.8",
                                         ea_04_notas_ultimo_semestre == "Entre el año 2024" ~ NA,
                                         ea_04_notas_ultimo_semestre == "considerando que el segundo semestre del 2023 cursaba 4to medio, tuve un 6.7" ~ NA,
                                         ea_04_notas_ultimo_semestre =="fui a un preu y solo fue desempeño asi que no se, solo se que mejore eso cuenta?" ~ NA,
                                         ea_04_notas_ultimo_semestre == "." ~ NA,
                                         ea_04_notas_ultimo_semestre == "5,6(?" ~ "5.6",
                                         ea_04_notas_ultimo_semestre == "58-60" ~ "5.9",
                                         ea_04_notas_ultimo_semestre == "Entre 5.0 y 5.5" ~ "5.3",
                                         ea_04_notas_ultimo_semestre == "No sé :( " ~ NA,
                                         ea_04_notas_ultimo_semestre == "Congele " ~ NA,
                                         ea_04_notas_ultimo_semestre == "congelé " ~ NA,
                                         ea_04_notas_ultimo_semestre == "6,0 aprox" ~ "6.0",
                                         ea_04_notas_ultimo_semestre == "no me acuerdo" ~ NA,
                                         ea_04_notas_ultimo_semestre == "estimo que entre un 55-60 pero no tengo certeza" ~ "5.7",
                                         ea_04_notas_ultimo_semestre == "5,0 aprox" ~ "5.0",
                                         ea_04_notas_ultimo_semestre == "Estoy en primer semestre " ~ NA,
                                         ea_04_notas_ultimo_semestre == "estoy en primer año" ~ NA,
                                         ea_04_notas_ultimo_semestre == "cómo un 5,0 no sé" ~ "5.0",
                                         ea_04_notas_ultimo_semestre == "Arriba de 56 " ~ "5.6",
                                         ea_04_notas_ultimo_semestre == "Ingrese recién este año, así que aún no termino el primer semestre "  ~ NA,
                                         ea_04_notas_ultimo_semestre == "estoy en mi primer semestre, pero entre las 4 notas que tengo hacen promedio 6.4" ~ NA,
                                         ea_04_notas_ultimo_semestre == "A" ~ NA,
                                         ea_04_notas_ultimo_semestre == "nose"  ~ NA,
                                         ea_04_notas_ultimo_semestre == "Cursando primer semestre " ~ NA,
                                         ea_04_notas_ultimo_semestre == "aún tengo muy pocas notas como para responder esta respuesta😿" ~ NA,
                                         ea_04_notas_ultimo_semestre == "Este es mi primer semestre" ~ NA,
                                         ea_04_notas_ultimo_semestre == "62.0" ~ "6.2",
                                         ea_04_notas_ultimo_semestre == "57.0" ~ "5.7",
                                         ea_04_notas_ultimo_semestre == "60.0" ~ "6.0",
                                         ea_04_notas_ultimo_semestre == "58.0" ~ "5.8",
                                         ea_04_notas_ultimo_semestre == "53.0" ~ "5.3",
                                         ea_04_notas_ultimo_semestre == "0.0" ~ NA,
                                         ea_04_notas_ultimo_semestre == "63.0" ~ "6.3",
                                         ea_04_notas_ultimo_semestre == "50.0" ~ "5.0",
                                         ea_04_notas_ultimo_semestre == "64.0" ~ "6.4",
                                         ea_04_notas_ultimo_semestre == "45.0" ~ "4.5",
                                         ea_04_notas_ultimo_semestre == "52.0" ~ "5.2",
                                         ea_04_notas_ultimo_semestre == "59.0" ~ "5.9",
                                         ea_04_notas_ultimo_semestre == "49.0" ~ "4.9",
                                         ea_04_notas_ultimo_semestre == "67.0" ~ "6.7",
                                         ea_04_notas_ultimo_semestre == "48.0" ~ "4.8",
                                         TRUE ~  ea_04_notas_ultimo_semestre ))

unique(base_antropologia$ea_04_notas_ultimo_semestre)


base_antropologia$ea_04_notas_ultimo_semestre <- as.numeric(base_antropologia$ea_04_notas_ultimo_semestre)
class(base_antropologia$ea_04_notas_ultimo_semestre)
table(base_antropologia$ea_04_notas_ultimo_semestre)

base_antropologia <- base_antropologia %>% 
  mutate (notas_ultimo_semestre_intervalo= case_when (ea_04_notas_ultimo_semestre >= 4 & ea_04_notas_ultimo_semestre < 5 ~ "4.0 a 4.9", 
                                                      ea_04_notas_ultimo_semestre >= 5 & ea_04_notas_ultimo_semestre < 6  ~ "5.0 a 5.9", 
                                                      ea_04_notas_ultimo_semestre >= 6 & ea_04_notas_ultimo_semestre < 7 ~ "6.0 a 7.0", 
                                                      ))


table(base_antropologia$notas_ultimo_semestre_intervalo)


#3.4.5. Satisfacción rendimiento  ####
# realizado por: Samanta
unique(base_antropologia$ea_05) 

# renombro
base_antropologia <- base_antropologia %>% dplyr::rename(ea_05_satisfaccion_rendimiento_academico = ea_05)
names(base_antropologia)

# Observo categorías
table(base_antropologia$ea_05_satisfaccion_rendimiento_academico)
class(base_antropologia$ea_05_satisfaccion_rendimiento_academico)

# Ordeno categorías y transformo a factor
base_antropologia$ea_05_satisfaccion_rendimiento_academico <- base_antropologia$ea_05_satisfaccion_rendimiento_academico %>% fct_relevel(c("Muy insatisfecho", "Insatisfecho", "Satisfecho", "Muy satisfecho")) 
class(base_antropologia$ea_05_satisfaccion_rendimiento_academico)

# Recodifico 
base_antropologia <- base_antropologia %>% 
  mutate(ea_05_satisfaccion_rendimiento_academico_r = case_when(ea_05_satisfaccion_rendimiento_academico== "Insatisfecho" ~ "Insatisfecho",
                                                        ea_05_satisfaccion_rendimiento_academico== "Muy insatisfecho" ~ "Insatisfecho",
                                                        ea_05_satisfaccion_rendimiento_academico== "Muy Satisfecho" ~ "Satisfecho",
                                                        ea_05_satisfaccion_rendimiento_academico== "Satisfecho" ~ "Satisfecho"))
# Observo lo realizado
table(base_antropologia$ea_05_satisfaccion_rendimiento_academico_r)


#3.4.6. Nivel de Estrés  ####
# realizado por: Joaquín
unique(base_antropologia$ea_06) 

base_antropologia <- base_antropologia %>% dplyr::rename(ea_06_nivel_estres_ultimo_semestre = ea_06)
table(base_antropologia$ea_06_nivel_estres_ultimo_semestre)
class(base_antropologia$ea_06_nivel_estres_ultimo_semestre)

# recodifico
base_antropologia <- base_antropologia %>%
  mutate(ea_06_nivel_estres_ultimo_semestre_r=case_when(ea_06_nivel_estres_ultimo_semestre == 1 ~ "Estres Bajo",
                                                ea_06_nivel_estres_ultimo_semestre == 2 ~ "Estres Bajo",
                                                ea_06_nivel_estres_ultimo_semestre == 3 ~ "Estres Moderado",
                                                ea_06_nivel_estres_ultimo_semestre == 4 ~ "Estres Alto",
                                                ea_06_nivel_estres_ultimo_semestre == 5 ~ "Estres Alto",
                                                ))
# ordeno variable recodificada
base_antropologia <- base_antropologia %>%
  mutate(ea_06_nivel_estres_ultimo_semestre_r= factor(ea_06_nivel_estres_ultimo_semestre_r, levels = c("Estres Bajo","Estres Moderado", 
                                                                                       "Estres Alto" ), ordered = TRUE))

# observo recodificación
table(base_antropologia$ea_06_nivel_estres_ultimo_semestre_r)


#3.4.7. Estrés ante rendimiento  ####
# realizado por: Joaquín
unique(base_antropologia$ea_07) 

base_antropologia <- base_antropologia %>% dplyr::rename(ea_07_efecto_estres_rendimiento = ea_07)

table(base_antropologia$ea_07_efecto_estres_rendimiento)

#ordeno
base_antropologia <- base_antropologia %>%
  mutate(ea_07_efecto_estres_rendimiento =factor(ea_07_efecto_estres_rendimiento, levels = c("Poco","Moderado", 
                                                                                             "Bastante","Mucho"), ordered = TRUE))
table(base_antropologia$ea_07_efecto_estres_rendimiento)


#recodifico en dos
base_antropologia <- base_antropologia %>%
  mutate(ea_07_efecto_estres_rendimiento_r=case_when(ea_07_efecto_estres_rendimiento == "Mucho" ~ "Bastante",
                                             ea_07_efecto_estres_rendimiento == "Bastante" ~ "Bastante",
                                             ea_07_efecto_estres_rendimiento == "Moderado" ~ "Moderadamente",
                                             ea_07_efecto_estres_rendimiento == "Poco" ~ "Moderadamente"
  ))

base_antropologia <- base_antropologia %>%
  mutate(ea_07_efecto_estres_rendimiento_r= factor(ea_07_efecto_estres_rendimiento_r, levels = c("Moderadamente", 
                                                                                 "Bastante"
  ), ordered = TRUE))


#observo
table(base_antropologia$ea_07_efecto_estres_rendimiento_r)


# ea_08_puede_identificar_por_si_mismo_cuando_se_siente_estresado_debidos_a_factores_relacionados_con_el_ambito_universitario",
unique(base_antropologia$ea_08)


# ea_09: respuesta múltiple procesamiento abajo
# ea_10: respuesta múltiple procesamiento abajo





# 4. Exportar ----------------------------------------------------------------
names(base_antropologia)

write.xlsx(x = base_antropologia,file = "base_antropologia_limpia.xlsx")


# 5. Análisis Univariados y bivariados-------------------------------
names(base_antropologia)

base_antropologia <- read.xlsx("base_antropologia_limpia.xlsx")
libro_codigos<- read.xlsx("Métodos Cuantitativos III (respuestas).xlsx") # dejo una base sin limpiar para observar nombres de preguntas


# 5.1.Sociodemográficas y de identificación -----------------------------------

names(base_antropologia) 

# 5.1.1. n_encuestador ####

# a) Distribución de Frecuencias
n_encuestador_t <- freq(base_antropologia$n_encuestador, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#install.packages("kableExtra")
library(kableExtra)

freq(base_antropologia$n_encuestador, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() %>%
  kable(col.names = c("Encuestador/a", "Frecuencia", "%", "% Acumulado"),
        caption = "Encuestas por Encuestador/a", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "outputs/n_encuestador.png", zoom = 3)

# b) realización de gráfico
# renombro nombre de mi tabla
n_encuestador_t <-  n_encuestador_t %>% 
  rename(Nombre = value, Porcentaje= pct, Frecuencia = freq)


# realizo gráfico
ggplot(n_encuestador_t, aes(x = Frecuencia, y = fct_reorder(Nombre, Frecuencia), fill= Nombre)) +
  geom_col() +
  labs(title = "Encuestas por Encuestador/a",
       subtitle = "según datos de Encuestas Estudiantes Antropología 2024",
       x = "%",  # Esto establece el título del eje x, pero no afecta las etiquetas dentro del gráfico
       y = "Nombre del Encuestador/a") +
  geom_text(aes(label = round(Frecuencia, 1)),  # Ahora esto añade etiquetas a todas las barras
            hjust = 1, size = 3, nudge_x = -0.9, fontface= "bold", color = "white") +
  scale_fill_viridis_d(option = "C", guide = "none") +
  theme_ipsum()


# guardo gráfico
n_encuestador_g <- ggplot(n_encuestador_t, aes(x = Frecuencia, y = fct_reorder(Nombre, Frecuencia), fill= Nombre)) +
  geom_col() +
  labs(title = "Encuestas por Encuestador/a",
       subtitle = "según datos de Encuestas Estudiantes Antropología 2024",
       x = "%",  # Esto establece el título del eje x, pero no afecta las etiquetas dentro del gráfico
       y = "Nombre del Encuestador/a") +
  geom_text(aes(label = round(Frecuencia, 1)),  # Ahora esto añade etiquetas a todas las barras
            hjust = 1, size = 3, nudge_x = -0.9, fontface= "bold", color = "white") +
  scale_fill_viridis_d(option = "C", guide = "none") +
  theme_ipsum()

ggsave("outputs/n_encuestador_g.png", plot = n_encuestador_g, width = 10, height = 7, dpi = 300)

# 5.1.2. identidad_genero ####
# responsable NOEL
# frecuencia
# gráfico
# tabla de contingencia


# 5.1.3. edad ####
# responsable 
# frecuencia
# gráfico
# tabla de contingencia


# 5.1.4. año_ingreso_carrera ####
# responsable 
# frecuencia
# gráfico
# tabla de contingencia

# 5.1.5. comuna ####
# responsable: Sebastián 
# frecuencia
# gráfico
# tabla de contingencia


# 5.1.6. clase_social ####
# responsable: Sebastián 
# frecuencia
# gráfico
# tabla de contingencia


# 5.1.8. nivel_educacion_padre ####
# responsable: 
# frecuencia
# gráfico
# tabla de contingencia



# 5.1.8. nivel_educacion_madre ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia


# 5.1.9. ultimo_colegio ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia



# 5.2. Estrés Académico --------------------------------------------------------

# 5.2.1. ea_01_horas_estudio_semana ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia



# 5.2.2. ea_02_horas_estudio_fin_semana ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia


# 5.2.3. ea_03_descripcion_carga_academica ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia

# 5.2.4. ea_04_notas_ultimo_semestre ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia

# 5.2.5. ea_05_satisfaccion_rendimiento_academico ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia


# 5.2.6. ea_06_nivel_estres_ultimo_semestre ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia

# 5.2.7. ea_07_efecto_estres_rendimiento ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia


# 5.2.8. ea_08 ####
# responsable:  
# frecuencia
# gráfico
# tabla de contingencia


# 5.2.9. ea_09 ####
# responsable:   Sebastian



unique(base_antropologia$ea_09) # SEBASTIÁN
class(base_antropologia$ea_09)

# frecuencia (respuesta múltiple)


#separo las respuestas y creo un vector que las lista
respuestas <- strsplit(base_antropologia$ea_09, ",") # separo las respuestas que tienen coma (,)
respuestas <- unlist(respuestas) #las unlisto, las saco de una lista
unique(respuestas)


#observo las respuestas
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#elimino espacio antes de primera letra
respuestas_limpio <- trimws(respuestas, which = "left")

# obtengo las frecuencias de mis preguntas de respuesta múltiple
freq(respuestas_limpio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# gráfico

#Guardo para graficar
ea_09_graf <- freq(respuestas_limpio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()


ea_09_tabla <- freq(respuestas_limpio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() %>%
  kable(col.names = c("Síntoma", "Frecuencia", "%", "% Acumulado"),
        caption = "Síntomas de Estress", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "outputs/ea_09_tabla.png", zoom = 3)

# renombro nombre de mi tabla
ea_09_graf <-  ea_09_graf %>% 
  rename(Problema = value, Porcentaje= pct)


# realizo gráfico
g_ea_09_graf <- ggplot(ea_09_graf, aes(x = Porcentaje, y = fct_reorder(Problema, Porcentaje), fill= Problema)) +
  geom_col() +
  labs(title = "Síntomas de Estrés Académico",
       subtitle = "según datos de Encuestas Estudiantes Antropología 2024",
       x = "%",
       y = "Síntoma") +
  geom_text(data = ea_09_graf %>% filter(rank(-Porcentaje) <= 12), # Solo añadir texto a las primeras 8 categorías
            aes(label = ifelse(rank(-Porcentaje) <= 12, paste0(round(Porcentaje, 1), "%"), "")),
            hjust = 1, size = 3, nudge_x = -.9, fontface= "bold", color = "white") +
  scale_fill_viridis_d(option = "C", guide = "none") +
  theme_ipsum()

ggsave("outputs/g_ea_09_graf.png", plot = g_ea_09_graf, width = 10, height = 7, dpi = 300)


# tabla de contingencia


# Suponiendo que 'base_antropologia' es tu DataFrame
# Primero, aseguramos que 'ea_09' y 'identidad_genero_r' sean tratados como caracteres
base_antropologia$ea_09 <- as.character(base_antropologia$ea_09)
base_antropologia$identidad_genero_r <- as.character(base_antropologia$identidad_genero_r)

# Separamos las respuestas múltiples de 'ea_09' y las cruzamos con 'identidad_genero_r'
respuestas_ea_09 <- unlist(strsplit(base_antropologia$ea_09, ",\\s*"))
identidades <- rep(base_antropologia$identidad_genero_r, times = sapply(strsplit(base_antropologia$ea_09, ",\\s*"), length))

# Creamos un nuevo DataFrame con las respuestas e identidades
data_cruzada <- data.frame(Respuesta = respuestas_ea_09, IdentidadGenero = identidades)



tabla_porcentajes <- data_cruzada %>%
  select(Respuesta, IdentidadGenero) %>%
  droplevels() %>%
  table() %>% 
  addmargins(., 2) %>%
  prop.table(., 2) %>%
  round(4) * 100

# Convertir la matriz a un data frame para manejar más fácilmente
df_porcentajes <- as.data.frame.matrix(tabla_porcentajes)

# Agregar una fila de totales al data frame
df_porcentajes <- bind_rows(df_porcentajes, Total = colSums(df_porcentajes))

# Mostrar el resultado
print(df_porcentajes)

ctable( x = data_cruzada$Respuesta, y = data_cruzada$IdentidadGenero, prop = "c", justify = "l", chisq = T)

# el cruce no es significativo !




# ea_10_que_estrategias_utiliza_con_mayor_frecuencia_para_manejar_el_estres_academico_seleccione_todas_las_alternativas_que_correspondan_con_su_caso",
unique(base_antropologia$ea_10) # NOEL 

#Preguntas de respuesta multiple

unique(base_antropologia$ea_10)
class(base_antropologia$ea_10)

#tuve que cambiar una categoría porque tenía una "," y al sperar las opciones dentro de la resúesta
#tambien cortaba un parantesis que tenias comas, así que lo cambie a un "/"

base_antropologia <- base_antropologia %>%
  mutate(ea_10 = case_when(
    grepl("Participar en otras actividades creativas \\(música, arte, escritura\\)", ea_10) ~
      gsub("Participar en otras actividades creativas \\(música, arte, escritura\\)", 
           "Participar en otras actividades creativas (música/arte/escritura)", 
           ea_10),
    TRUE ~ ea_10
  ))

#separo las respuestas y creo un vector que las lista
respuestas_ea_10 <- unlist(strsplit(base_antropologia$ea_10, ", ")) # separo las respuestas que tienen coma (,)

#hice la lista altiro
unique(respuestas_ea_10)


#observo las respuestas
freq(respuestas_ea_10, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#elimino espacio antes de primera letra
respuestas_ea_10_limpio <- trimws(respuestas_ea_10, which = "left")

# obtengo las frecuencias de mis preguntas de respuesta múltiple
freq(respuestas_ea_10_limpio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#Guardo para graficar
ea_10_graf <- freq(respuestas_ea_10_limpio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

ea_10_tabla <- freq(respuestas_ea_10_limpio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() %>%
  kable(col.names = c("Estrategias", "Frecuencia", "%", "% Acumulado"),
        caption = "Estrategias del manejo del estres", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "outputs/ea_10_tabla.png", zoom = 3)

install.packages("kableExtra")
library(kableExtra)

ea_10_graf <-  ea_10_graf %>% 
  rename(Problema = value, Porcentaje= pct)



g_ea_10_graf <- ggplot(ea_10_graf, aes(x = Porcentaje, y = fct_reorder(Problema, Porcentaje), fill= Problema)) +
  geom_col() +
  labs(title = "Estrategias del manejo del estres",
       subtitle = "según datos de Encuestas Estudiantes Antropología 2024",
       x = "%",
       y = "Estrategia") +
  geom_text(data = ea_10_graf %>% filter(rank(-Porcentaje) <= 12), # Solo añadir texto a las primeras 8 categorías
            aes(label = ifelse(rank(-Porcentaje) <= 12, paste0(round(Porcentaje, 1), "%"), "")),
            hjust = 1, size = 3, nudge_x = -.9, fontface= "bold", color = "white") +
  scale_fill_viridis_d(option = "C", guide = "none") +
  theme_ipsum()

ggsave("outputs/g_ea_10_graf.png", plot = g_ea_10_graf, width = 10, height = 7, dpi = 300)