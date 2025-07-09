

# I. Preparación base limpia ----------------------------------------------

# 1. Instalo y abro paquetes -------------------------------------------------
# install.packages("pacman")

pacman::p_load(
  tidyverse,      # conjunto de paquetes para manipular y visualizar datos (dplyr, ggplot2, tidyr, readr, forcats, etc.)
  openxlsx,       # leer y escribir archivos Excel (.xlsx) con opciones avanzadas (múltiples hojas, formatos, rangos)
  janitor,        # limpiar nombres de columnas, eliminar duplicados y crear tablas de frecuencia rápido
  DataExplorer,   # generar reportes exploratorios automáticos (gráficos y estadísticas) con poco código
  knitr,          # convertir R Markdown en reportes HTML, PDF o Word mezclando código y texto
  gt,             # crear tablas formateadas y atractivas directamente desde data.frames
  summarytools,   # producir resúmenes estadísticos y tablas de contingencia con salida HTML bonita
  ggthemes,       # añadir temas y paletas extras a los gráficos de ggplot2
  hrbrthemes,     # ofrecer temas modernos y tipográficos para ggplot2
  DescTools,      # colección de herramientas estadísticas (tests, medidas de efecto, funciones varias)
  ineq,            # calcular índices de desigualdad (Gini, curvas de Lorenz, Atkinson, etc.) 
  treemapify
)
options(OutDec= ",") # para que decimales me aparezcan con ,

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

base_antropologia <- base_antropologia %>% 
  dplyr::rename(s_me_tris = s_me_01_en_las_ultimas_dos_semanas_con_que_frecuencia_ha_experimentado_los_siguientes_tres_sintomas_tristeza,
                s_me_ansi = s_me_01_en_las_ultimas_dos_semanas_con_que_frecuencia_ha_experimentado_los_siguientes_tres_sintomas_ansiedad, 
                s_me_estre = s_me_01_en_las_ultimas_dos_semanas_con_que_frecuencia_ha_experimentado_los_siguientes_tres_sintomas_estres)

names(base_antropologia)

#acorto a 10 caracteres para que sea más fácil trabajar
names(base_antropologia) <- substring(names(base_antropologia), 1, 10)

#observo
names(base_antropologia)
names(libro_codigos)

#3.3.Renombro las variables de identificación y las sociodemográficas ####

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


#3.4.Eliminación de casos duplicados ####
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

#jamadues contestó 2 veces. Elimino la primera vez.

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
  geom_col(fill = "#01fb5c") +
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





#3.5.Eliminación de variables mail y nombre de encuestado para mantener anonimato ####
base_antropologia <- base_antropologia %>%
  select(-mail, -nombre_encuestado)




### DEPURAR
write.xlsx(
  base_antropologia,
  file      = "encuesta-antropologia-2025-depurada_p.xlsx",
  sheetName = "Datos Limpios",
  colNames  = TRUE,
  rowNames  = FALSE,
  overwrite = TRUE
)




# II. Procesamiento variables identificación y sociodemográficas --------------

#1. Limpieza y procesamiento
## Sección Amilcar Canala

# edad
table(base_antropologia$edad)
class(base_antropologia$edad)
mean(base_antropologia$edad)
# Estandariza ‘edad’ a numérico y genera la variable de rangos
base_antropologia <- base_antropologia %>% 
  mutate(
    # 1. Limpieza mínima (convierte a numérico; asume que ya corregiste strings como en tu ejemplo)
    edad = as.numeric(edad),
    
    # 2. Agrupación por rangos
    edad_r = case_when(
      between(edad, 18, 20)          ~ "18-20",
      between(edad, 21, 23)          ~ "21-23",
      between(edad, 24, 29)          ~ "24-29",
      edad >= 30                     ~ "30 y más",
      TRUE                           ~ NA_character_     # valores faltantes o fuera de rango
    )
  )
table(base_antropologia$edad_r)

# tabla 
unique(base_antropologia$edad_r)
base_antropologia %>% 
  count(edad_r) %>% 
  mutate(edad_r = factor(edad_r, levels = c("18-20", "21-23", "24-29", "30 y más"))) %>%
  ggplot(aes(x = edad_r, y = n)) +
  geom_col() +
  labs(
    x = "Grupo de edad",
    y = "Número de casos",
    title = "Distribución de edades (edad_r)"
  ) +
  theme_minimal()

# Calcular frecuencias y porcentajes
df_edad <- base_antropologia %>%
  count(edad_r) %>%
  mutate(
    edad_r = factor(edad_r, levels = c("18-20", "21-23", "24-29", "30 y más")),
    porcentaje = round(n / sum(n) * 100, 1),
    etiqueta = paste0(n, " (", porcentaje, "%)")
  )

#NOTA: Sebastián: 
# es preferible que en el eje y, vaya el porcentaje más que el recuento.
# le agregué caption: que indica el nombre de la encuesta.
# puede ser innecesario indicar el nombre "Grupo Etario" y "Número de Personas" si es que es relativamente evidente. 
# les recomiendo ponerse de acuerdo y hacer todos los gráficos con el mismo color y la misma letra
# quizás podemos usar el siguiente verde: #01fb5c que es el de la universidad.


# Genero gráfico
ggplot(df_edad, aes(x = edad_r, y = n)) +
  geom_bar(stat = "identity", fill = "#457b9d", width = 0.6) +
  geom_text(aes(label = etiqueta), vjust = -0.5, size = 4.5, family = "serif") +
  labs(
    title = "Distribución de estudiantes por grupo de edad",
    x = "Grupo etario",
    y = "Número de personas",
    caption = "Encuesta de Estudiantes de Antropología UAH -2025"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, max(df_edad$n) * 1.15)

# Cargar librería
library(kableExtra)

# Generar tabla resumen de edad_r
base_antropologia %>%
  filter(!is.na(edad_r)) %>%                            # Se excluyen NA explícitamente
  count(edad_r) %>%
  mutate(
    edad_r = factor(edad_r, levels = c("18-20", "21-23", "24-29", "30 y más")),
    Porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(edad_r) %>%
  rename(
    `Grupo Etario` = edad_r,
    Frecuencia     = n
  ) %>%
  bind_rows(
    tibble(
      `Grupo Etario` = "Total",
      Frecuencia     = sum(.$Frecuencia),
      Porcentaje     = 100
    )
  ) %>%
  kable(
    col.names = c("Grupo Etario", "Frecuencia", "Porcentaje"),
    caption   = "Distribución de estudiantes por grupo etario",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

# genero
unique(base_antropologia$genero) 

# NOTA SEBASTIÁN: estos paquetes ya están incluidos en tidyverse, no es necesario volver a abrirlos. 
# library(dplyr) 
# library(stringr)


# recodificación (agrupo categorías en una versión detallada y otra simple)
base_antropologia <- base_antropologia %>%
  mutate(
    genero = str_trim(genero),
    identidad_genero_det = case_when(
      genero %in% c("Hombre cisgénero", "Hombre", "Hombre etero") ~ "Hombre cisgenero",
      genero %in% c("Mujer cisgénero", "Femenino") ~ "Mujer cisgenero",
      genero %in% c("Hombre trans/transmasculino", "Mujer trans/transfemenina") ~ "Persona transgénero",
      genero %in% c("No binarie", "Agénero", "Género fluido", "Ninguno") ~ "Persona no binarie / género fluido",
      TRUE ~ "Otro / No especificado"
    ),
    identidad_genero_simple = case_when(
      genero %in% c("Hombre cisgénero", "Hombre", "Hombre etero") ~ "Hombre cisgenero",
      genero %in% c("Mujer cisgénero", "Femenino") ~ "Mujer cisgenero",
      TRUE ~ "Persona de genero diverso"
    )
  )


# tabla 
table(base_antropologia$identidad_genero_det)
table(base_antropologia$identidad_genero_simple)

# tabla con porcentajes
df_simple <- base_antropologia %>%
  count(identidad_genero_simple) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    etiqueta = paste0(n, " (", porcentaje, "%)"),
    identidad_genero_simple = reorder(identidad_genero_simple, n)
  )

# gráfico
ggplot(df_simple, aes(x = identidad_genero_simple, y = n)) +
  geom_col(fill = "#DDA0DD", width = 0.7) +
  geom_text(aes(label = etiqueta), hjust = -0.1, color = "black", size = 4) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Número de casos",
    title = "Distribución por identidad de género (agrupación simple)"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  ylim(0, max(df_simple$n) * 1.2)




# tabla 2 con porcentajes
df_detalle <- base_antropologia %>%
  count(identidad_genero_det) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    etiqueta = paste0(n, " (", porcentaje, "%)"),
    identidad_genero_det = reorder(identidad_genero_det, n)
  )


# gráfico
ggplot(df_detalle, aes(x = identidad_genero_det, y = n)) +
  geom_col(fill = "#DDA0DD", width = 0.7) +
  geom_text(aes(label = etiqueta), hjust = -0.1, color = "black", size = 4) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Número de casos",
    title = "Distribución por identidad de género (detalle)"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  ylim(0, max(df_detalle$n) * 1.2)

#tabla 1: genero simple
# library(kableExtra)

base_antropologia %>%
  filter(!is.na(identidad_genero_simple)) %>%
  count(identidad_genero_simple) %>%
  mutate(
    Porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(desc(n)) %>%
  rename(
    `Identidad de Género (Simple)` = identidad_genero_simple,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `Identidad de Género (Simple)` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  ) %>%
  kable(
    col.names = c("Identidad de Género", "Frecuencia", "Porcentaje"),
    caption = "Distribución por identidad de género (agrupación simple)",
    format = "html",
    digits = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font = "Cambria",
    font_size = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

#tabla 2: genero detalle
base_antropologia %>%
  filter(!is.na(identidad_genero_det)) %>%
  count(identidad_genero_det) %>%
  mutate(
    Porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(desc(n)) %>%
  rename(
    `Identidad de Género (Detalle)` = identidad_genero_det,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `Identidad de Género (Detalle)` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  ) %>%
  kable(
    col.names = c("Identidad de Género", "Frecuencia", "Porcentaje"),
    caption = "Distribución por identidad de género (detalle)",
    format = "html",
    digits = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font = "Cambria",
    font_size = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

# anio_ingreso
# recodificación o limpieza si amerita
unique(base_antropologia$anio_ingreso)
# library(dplyr)
# library(ggplot2)

base_antropologia <- base_antropologia %>%
  mutate(anio_ingreso = as.numeric(anio_ingreso),
         ingreso_pandemia = case_when(
           anio_ingreso <= 2019 ~ "Pre-pandemia",
           anio_ingreso %in% c(2020, 2021) ~ "Pandemia",
           anio_ingreso >= 2022 ~ "Post-pandemia",
           TRUE ~ NA_character_
         ))

# Tabla resumen
df_pandemia <- base_antropologia %>%
  count(ingreso_pandemia) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    etiqueta = paste0(n, " (", porcentaje, "%)"),
    ingreso_pandemia = factor(ingreso_pandemia, levels = c("Pre-pandemia", "Pandemia", "Post-pandemia"))
  )

# Gráfico
ggplot(df_pandemia, aes(x = ingreso_pandemia, y = n)) +
  geom_col(fill = "#DDA0DD", width = 0.6) +
  geom_text(aes(label = etiqueta), vjust = -0.5, size = 5, family = "serif") +
  labs(
    title = "Distribución de estudiantes según periodo de ingreso",
    x = "Periodo de ingreso",
    y = "Número de personas"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, max(df_pandemia$n) * 1.15)

# library(kableExtra)

base_antropologia %>%
  filter(!is.na(ingreso_pandemia)) %>%
  mutate(
    ingreso_pandemia = factor(ingreso_pandemia, 
                              levels = c("Pre-pandemia", "Pandemia", "Post-pandemia"))
  ) %>%
  count(ingreso_pandemia) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  rename(
    `Periodo de Ingreso` = ingreso_pandemia,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `Periodo de Ingreso` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  ) %>%
  kable(
    col.names = c("Periodo de Ingreso", "Frecuencia", "Porcentaje"),
    caption = "Distribución de estudiantes según periodo de ingreso",
    format = "html",
    digits = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font = "Cambria",
    font_size = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )


# comuna_rm
#library(dplyr)
#library(stringr)
#library(ggplot2)

# limpieza de nombres en comuna_rm ------------------------------

table(base_antropologia$comuna_rm)


base_antropologia <- base_antropologia %>%
  mutate(
    comuna_rm = str_to_lower(comuna_rm),                    
    comuna_rm = str_trim(comuna_rm),                         
    comuna_rm = str_replace_all(comuna_rm, "á", "a"),        
    comuna_rm = str_replace_all(comuna_rm, "é", "e"),
    comuna_rm = str_replace_all(comuna_rm, "í", "i"),
    comuna_rm = str_replace_all(comuna_rm, "ó", "o"),
    comuna_rm = str_replace_all(comuna_rm, "ú", "u"),
    comuna_rm = str_replace_all(comuna_rm, "ñ", "n")
  )

base_antropologia <- base_antropologia %>%
  mutate(
    comuna_rm = tolower(comuna_rm),              
    comuna_rm = str_trim(comuna_rm),             
    comuna_rm = case_when(
      comuna_rm == "qulicura" ~ "quilicura",     
      comuna_rm == "la cisterna " ~ "la cisterna", 
      comuna_rm == "la cisterna" ~ "la cisterna",  
      TRUE ~ comuna_rm
    )
  )

#NP: Recomiendo hacer un gráfico geom_col de todas las comunas ordenado de mayor a menor pero realizado de forma vertical. 
### OJO ACÁ. 

# 1. Calcular frecuencias y ordenar
df_plot <- base_antropologia %>%
  count(comuna_rm) %>%
  arrange(desc(n)) %>%
  mutate(comuna_rm = factor(comuna_rm, levels = comuna_rm))


# Tabla resumen
df_cm <- base_antropologia %>%
  count(comuna_rm) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    etiqueta = paste0(n, " (", porcentaje, "%)"))

unique(df_cm$comuna_rm)

df_cm <- df_cm %>%
  mutate(comuna_rm = str_to_title(comuna_rm)) # Primera letra de cada palabra en mayúscula


# 2. Dibujar gráfico

# 1. Asegurar formato y orden descendente
df_cm <- df_cm %>%
  mutate(comuna_rm = str_to_title(comuna_rm)) %>%
  arrange(desc(porcentaje))

# 2. Separar top 20
top_20 <- df_cm %>% slice_max(porcentaje, n = 20)

# 3. Agrupar resto como "Otras"
otras <- df_cm %>%
  filter(!comuna_rm %in% top_20$comuna_rm) %>%
  summarise(comuna_rm = "Otras", porcentaje = sum(porcentaje))

# 4. Combinar y crear orden manual desde mayor a menor, con "Otras" al final
df_final <- bind_rows(top_20, otras)
orden_comunas <- c(top_20 %>% arrange(desc(porcentaje)) %>% pull(comuna_rm), "Otras")

# 5. Reordenar con factor explícito
df_final <- df_final %>%
  mutate(comuna_rm = factor(comuna_rm, levels = orden_comunas))

# 6. Graficar (sin coord_flip, barras horizontales invertidas)
ggplot(df_final, aes(x = fct_rev(comuna_rm), y = porcentaje)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Residencia Actual de Estudiantes por Comuna RM",
    caption = "Encuesta de Estudiantes de Antropología UAH - 2025",
    x = NULL,
    y = NULL
  )


# recodificación por distancia a la universidad ------------------

base_antropologia <- base_antropologia %>%
  mutate(comuna_distancia = case_when(
    comuna_rm %in% c("santiago", "providencia", "estacion central", "quinta normal", "independencia") ~ "Vive muy cerca",
    comuna_rm %in% c("nunoa", "san miguel", "la cisterna", "conchali") ~ "Vive a distancia cercana",
    comuna_rm %in% c("macul", "la florida", "penalolen", "maipu", "pudahuel", "renca", "la granja", "cerrillos", 
                     "lo espejo", "cerro navia", "san joaquin", "quilicura", "huechuraba", "las condes") ~ "Vive a distancia media",
    comuna_rm %in% c("puente alto", "la pintana", "san bernardo", "el bosque") ~ "Vive a mucha distancia",
    comuna_rm %in% c("rancagua", "buin", "san felipe", "maria pinto", "los andes", "til til", "el monte", 
                     "talagante", "paine", "colina", "calera de tango", "melipilla", "lampa", "penaflor") ~ "Fuera de Gran Santiago",
  ))

# Tabla de proporciones
tabla_distancia <- base_antropologia %>%
  count(comuna_distancia) %>%
  mutate(
    porcentaje = round(n / sum(n, na.rm = TRUE) * 100, 1),
    comuna_distancia = factor(comuna_distancia, 
                              levels = c("Vive muy cerca", "Vive a distancia cercana", 
                                         "Vive a distancia media", "Vive a mucha distancia", 
                                         "Fuera de Gran Santiago"))
  )

# Gráfico con porcentajes
# NP: para que no se superpongan las categorías de la distancia estimada, deberías ponerlas en 45° o 90°.
#OJO CON ESTO

ggplot(tabla_distancia, aes(x = comuna_distancia, y = porcentaje)) +
  geom_col(fill = "#DDA0DD", width = 0.7) +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 4.2, family = "sans") +
  labs(
    title = "Distribución de estudiantes según distancia desde su comuna a la UAH",
    x = "Distancia estimada a la universidad",
    y = "Porcentaje de estudiantes"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

#tabla formateada
base_antropologia %>%
  filter(is.na(comuna_distancia)) %>%
  select(comuna_rm, comuna_distancia) 

# library(kableExtra)

base_antropologia %>%
  filter(!is.na(comuna_distancia)) %>%
  mutate(
    comuna_distancia = factor(comuna_distancia,
                              levels = c("Vive muy cerca", "Vive a distancia cercana",
                                         "Vive a distancia media", "Vive a mucha distancia",
                                         "Fuera de Gran Santiago"))
  ) %>%
  count(comuna_distancia) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  rename(
    `Distancia estimada a la UAH` = comuna_distancia,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `Distancia estimada a la UAH` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  ) %>%
  kable(
    col.names = c("Distancia estimada a la UAH", "Frecuencia", "Porcentaje"),
    caption   = "Distribución de estudiantes según distancia desde su comuna a la UAH",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )


#rm_dis
unique(base_antropologia$rm_dis)

#Recodificar y ordenar niveles
base_antropologia <- base_antropologia %>%
  mutate(rm_dis = factor(rm_dis, levels = c("No", "Si")))

#tabla con frecuencia y porcentaje
tabla_rm_dis <- base_antropologia %>%
  count(rm_dis) %>%
  mutate(porcentaje = n / sum(n) * 100)

print(tabla_rm_dis)

#Gráfico de barras con porcentaje y etiquetas
ggplot(tabla_rm_dis, aes(x = rm_dis, y = porcentaje, fill = rm_dis)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("#003366", "#DDA0DD")) +
  labs(
    title = "Distribución de estudiantes según región de procedencia",
    x = "¿Proviene de región fuera de la RM?",
    y = "Porcentaje (%)"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#tabla formateada:
#library(kableExtra)

base_antropologia %>%
  filter(!is.na(rm_dis)) %>%
  count(rm_dis) %>%
  mutate(
    Porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  rename(
    `¿Proviene de región fuera de la RM?` = rm_dis,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `¿Proviene de región fuera de la RM?` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  ) %>%
  kable(
    col.names = c("¿Proviene de región fuera de la RM?", "Frecuencia", "Porcentaje"),
    caption = "Distribución de estudiantes según región de procedencia",
    format = "html",
    digits = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font = "Cambria",
    font_size = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )


# region
unique(base_antropologia$reg)
# Recodificación
base_antropologia <- base_antropologia %>%
  mutate(
    reg_limpia = case_when(
      is.na(reg) ~ NA_character_,
      reg == "Provengo del extranjero" ~ NA_character_,
      TRUE ~ reg
    )
  )

# Tabla con conteos y porcentajes
tabla_regiones <- base_antropologia %>%
  filter(!is.na(reg_limpia)) %>%
  count(reg_limpia) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  arrange(desc(porcentaje))
print(tabla_regiones)

#library(ggplot2)

library(scales)
#gráfico
ggplot(
  tabla_regiones %>% 
    mutate(reg_limpia = fct_reorder(reg_limpia, porcentaje)),  # ordena por porcentaje
  aes(y = reg_limpia, x = porcentaje, fill = reg_limpia)
) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "% (", n, ")")),
            hjust = -0.1, size = 4, family = "sans") +
  scale_x_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Distribución de estudiantes provenientes de regiones fuera de la Región Metropolitana",
    x = "",
    y = "",
    fill = "Región"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#NOTA SEBASTIÁN: Este tipo de gráfico queda mejor en horizontal. 


#tabla formateada:
#library(kableExtra)

tabla_regiones_kable <- base_antropologia %>%
  filter(!is.na(reg_limpia)) %>%
  count(reg_limpia) %>%
  mutate(
    Porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  rename(
    Región = reg_limpia,
    Frecuencia = n
  ) %>%
  arrange(desc(Porcentaje)) %>%
  bind_rows(
    tibble(
      Región = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  )

tabla_regiones_kable %>%
  kable(
    col.names = c("Región", "Frecuencia", "Porcentaje"),
    caption = "Distribución de estudiantes por región de procedencia (excluye Región Metropolitana y casos del extranjero)",
    format = "html",
    digits = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font = "Cambria",
    font_size = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )


## Sección Javiera Durán
# tiempo_u
# recodificación o limpieza si amerita
# tabla 
# gráfico
## Sección Javiera Durán
# tiempo_u
# recodificación o limpieza si amerita
# tabla 
# gráfico

unique(base_antropologia$tiempo_u)
table(base_antropologia$tiempo_u)



table(base_antropologia$tiempo_u)


# ordeno x tiempo
orden_tiempo <- c(
  "Menos de 15 minutos",
  "Entre 15 y 30 minutos",
  "Entre 31 y 45 minutos",
  "Entre 46 minutos y 1 hora",
  "Entre 1 hora y 1 hora 30 minutos",
  "Más de 1 hora 30 minutos"
)
base_antropologia <- base_antropologia %>%
  mutate(tiempo_u = factor(tiempo_u, levels = orden_tiempo, ordered = TRUE))
table(base_antropologia$tiempo_u)



# tabla frecuencia y porcentaje
tabla_tiempo_u<- base_antropologia %>%
  count(tiempo_u) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(tabla_tiempo_u)



base_antropologia %>%
  filter(!is.na(tiempo_u)) %>%
  mutate(
    tiempo_u = factor(tiempo_u,
                      levels = c("Menos de 15 minutos", "Entre 15 y 30 minutos",
                                 "Entre 31 y 45 minutos", "Entre 46 minutos y 1 hora",
                                 "Entre 1 hora y 1 hora 30 minutos", "Más de 1 hora 30 minutos"))
  ) %>%
  count(tiempo_u) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  rename(
    `Tiempo entre la universidad y los hogares` = tiempo_u,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `Tiempo entre la universidad y los hogares` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  ) %>%
  kable(
    col.names = c("Tiempo de traslado", "Frecuencia", "Porcentaje"),
    caption   = "Tiempo de traslado entre la casa y la universidad",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

df_tiempo_u <- base_antropologia %>%
  filter(!is.na(tiempo_u)) %>%
  mutate(
    tiempo_u = factor(tiempo_u,
                      levels = c("Menos de 15 minutos", "Entre 15 y 30 minutos",
                                 "Entre 31 y 45 minutos", "Entre 46 minutos y 1 hora",
                                 "Entre 1 hora y 1 hora 30 minutos", "Más de 1 hora 30 minutos"))
  ) %>%
  count(tiempo_u) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  rename(`Tiempo traslado` = tiempo_u,
         Frecuencia = n)

# Crear gráfico con ggplot2
ggplot(df_tiempo_u, aes(x = `Tiempo traslado`, y = Porcentaje)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Tiempo de traslado entre la casa y la universidad",
    subtitle = "Encuesta de Estudiantes de Antropología UAH 2025",
    x = "Tiempo de traslado",
    y = "Porcentaje"
  ) +
  theme_minimal(base_family = "Cambria") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )


# nacionalidad
# recodificación o limpieza si amerita
# tabla 
# gráfico

names(base_antropologia)

table(base_antropologia$nacionalidad)

base_antropologia <- base_antropologia %>%
  mutate(
    nacionalidad = stringi::stri_trans_general(nacionalidad, "Latin-ASCII"),
    nacionalidad = tolower(nacionalidad),
    nacionalidad = gsub(" ", "_", nacionalidad)
  )

table(base_antropologia$nacionalidad)

#hice una tabla y voy a recodificar chilenos y otros plop

base_antropologia <- base_antropologia %>%
  mutate(nacionalidad_r = case_when(
    nacionalidad == "chilena" ~ "Chilena",
    nacionalidad %in% c("brasilena", "colombiana_", "estadounidense_", "peruana", "venezolano") ~ "Otros",
    TRUE ~ nacionalidad  
  ))

# Tabla resumen
tabla_nacionalidad_r <- base_antropologia %>%
  filter(!is.na(nacionalidad_r)) %>%
  mutate(
    nacionalidad_r = factor(nacionalidad_r,
                            levels = c("Chilena", "Otros"))
  ) %>%
  count(nacionalidad_r) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  rename(
    `Nacionalidad de estudiantes` = nacionalidad_r,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `Nacionalidad de estudiantes` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  )

tabla_nacionalidad_r %>%
  kable(
    col.names = c("Nacionalidad", "Frecuencia", "Porcentaje"),
    caption   = "Nacionalidades de las y los estudiantes de Antropología",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

##ahora grafico


df_plot_nac <- base_antropologia %>%
  filter(!is.na(nacionalidad_r)) %>%
  count(nacionalidad_r) %>%
  mutate(
    nacionalidad_r = factor(nacionalidad_r, levels = c("Chilena", "Otros")),
    porcentaje = round((n / sum(n)) * 100, 1)
  )

ggplot(df_plot_nac, aes(x = nacionalidad_r, y = porcentaje)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Nacionalidad de las y los estudiantes de Antropología",
    subtitle = "Encuesta de Estudiantes de Antropología UAH 2025",
    x = "Nacionalidad",
    y = "Porcentaje"
  ) +
  theme_minimal(base_family = "Cambria") +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

#origen_padre
# recodificación o limpieza si amerita
# tabla 
# gráfico

#corrijo nombres escritos con mayúsculas o espacios
table(base_antropologia$origen_padre)
base_antropologia <- base_antropologia %>%
  mutate(
    origen_padre = stringi::stri_trans_general(origen_padre, "Latin-ASCII"),
    origen_padre = tolower(origen_padre),
    origen_padre = gsub(" ", "_", origen_padre)
  )
table(base_antropologia$origen_padre)

base_antropologia <- base_antropologia %>%
  mutate(origen_padre = recode(origen_padre, "peru_" = "peru",
                               "italia_" = "italia"))
table(base_antropologia$origen_padre)

#agrupo de acuerdo a región 
base_antropologia <- base_antropologia %>%
  mutate(origen_padre_r = case_when(
    origen_padre %in% c("argentina", "peru", "venezuela", "cuba", "italia") ~ "Otros",
    origen_padre == "chile" ~ "Chile",
    TRUE ~ origen_padre  
  ))
table(base_antropologia$origen_padre_r)

#tabla frecuencia y porcentaje

tabla_origen_padre_r<- base_antropologia %>%
  count(origen_padre_r) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(tabla_origen_padre_r)

#tabla linda
tabla_origen_padre_r <- base_antropologia %>%
  filter(!is.na(nacionalidad_r)) %>%
  mutate(
    origen_padre_r = factor(origen_padre_r,
                            levels = c("Chile", "Otros"))
  ) %>%
  count(origen_padre_r) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  rename(
    `País de origen de la figura paterna` = origen_padre_r,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `País de origen de la figura paterna` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  )

tabla_origen_padre_r %>%
  kable(
    col.names = c("Nacionalidad padre", "Frecuencia", "Porcentaje"),
    caption   = "País de origen de los padres del estudiantado",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )


# gráfico
df_plot_origenp <- base_antropologia %>%
  filter(!is.na(origen_padre_r)) %>%
  count(origen_padre_r) %>%
  mutate(
    origen_padre_r = factor(origen_padre_r, levels = c("Chile", "Otros")),
    porcentaje = round((n / sum(n)) * 100, 1)
  )

ggplot(df_plot_origenp, aes(x = origen_padre_r, y = porcentaje)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "País de origen de la figura paterna del estudiantado de Antropología",
    subtitle = "Encuesta de Estudiantes de Antropología UAH 2025",
    x = "Nacionalidad figura paterna",
    y = "Porcentaje"
  ) +
  theme_minimal(base_family = "Cambria") +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )


#origen_madre
# recodificación o limpieza si amerita
# tabla 
# gráfico

table(base_antropologia$origen_madre)
base_antropologia <- base_antropologia %>%
  mutate(
    origen_madre = stringi::stri_trans_general(origen_madre, "Latin-ASCII"),
    origen_madre = tolower(origen_madre),
    origen_madre = gsub(" ", "_", origen_madre)
  )
table(base_antropologia$origen_madre)

base_antropologia <- base_antropologia %>%
  mutate(origen_madre = recode(origen_madre, "colombiana_" = "colombia",
                               "estados_unidos_" = "estados_unidos",
                               "peru_" = "peru",
                               "mexico_" = "mexico",
                               "venezuela_" = "venezuela"))
table(base_antropologia$origen_madre)

#agrupo origen figura materna entre chile y otros
base_antropologia <- base_antropologia %>%
  mutate(origen_madre_r = case_when(
    origen_madre %in% c("argentina", "brasil", "colombia", "estados_unidos", "mexico", "peru", "venezuela") ~ "Otros",
    origen_madre == "chile" ~ "Chile",
    TRUE ~ origen_madre  
  ))
table(base_antropologia$origen_madre_r)

#tabla frecuencia y porcentaje

tabla_origen_madre_r<- base_antropologia %>%
  count(origen_madre_r) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(tabla_origen_madre_r)

#tabla linda
tabla_origen_madre_r <- base_antropologia %>%
  filter(!is.na(origen_madre_r)) %>%
  mutate(
    origen_madre_r = factor(origen_madre_r,
                            levels = c("Chile", "Otros"))
  ) %>%
  count(origen_madre_r) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  rename(
    `País de origen de la figura materna` = origen_madre_r,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `País de origen de la figura materna` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  )

tabla_origen_madre_r %>%
  kable(
    col.names = c("Nacionalidad figura materna", "Frecuencia", "Porcentaje"),
    caption   = "País de origen de la figura materna del estudiantado",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )


# gráfico
df_plot_origenm <- base_antropologia %>%
  filter(!is.na(origen_madre_r)) %>%
  count(origen_madre_r) %>%
  mutate(
    origen_madre_r = factor(origen_madre_r, levels = c("Chile", "Otros")),
    porcentaje = round((n / sum(n)) * 100, 1)
  )

ggplot(df_plot_origenm, aes(x = origen_madre_r, y = porcentaje)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "País de origen de la figura materna del estudiantado de Antropología",
    subtitle = "Encuesta de Estudiantes de Antropología UAH 2025",
    x = "Nacionalidad figura materna",
    y = "Porcentaje"
  ) +
  theme_minimal(base_family = "Cambria") +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

#pueblo_o
# recodificación o limpieza si amerita
# tabla 
# gráfico

table(base_antropologia$pueblo_o)


#NOTA: ACÁ HAY ALGO RARO, SALEN 12 QUE SON DE PUEBLO ORIGINARIO Y LUEGO 5

tabla_pueblo_o <- base_antropologia %>%
  filter(!is.na(pueblo_o)) %>%
  mutate(
    pueblo_o = factor(pueblo_o,
                            levels = c("No", "Sí"))
  ) %>%
  count(pueblo_o) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  rename(
    `¿Pertenece a algún pueblo indígena?` = pueblo_o,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `total estudiantes` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  )

tabla_nacionalidad_r %>%
  kable(
    col.names = c("Pertenencia idígena", "Frecuencia", "Porcentaje"),
    caption   = "Pertenencia a pueblos indígenas",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

##ahora grafico


df_plot_pueblo_o <- base_antropologia %>%
  filter(!is.na(pueblo_o)) %>%
  count(pueblo_o) %>%
  mutate(
    pueblo_o = factor(pueblo_o, levels = c("No", "Sí")),
    porcentaje = round((n / sum(n)) * 100, 1)
  )

ggplot(df_plot_pueblo_o, aes(x = pueblo_o, y = porcentaje)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Pertenencia a pueblos indígenas",
    subtitle = "Encuesta de Estudiantes de Antropología UAH 2025",
    x = "Pertenencia",
    y = "Porcentaje"
  ) +
  theme_minimal(base_family = "Cambria") +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

#pueblo_os
# recodificación o limpieza si amerita
# tabla 
# gráfico

table(base_antropologia$pueblo_os)

#tabla frecuencia y porcentaje

tabla_pueblo_os<- base_antropologia %>%
  count(pueblo_os) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(tabla_pueblo_os)

#tabla linda
tabla_pueblo_os <- base_antropologia %>%
  filter(!is.na(pueblo_os)) %>%
  mutate(
    pueblo_os = factor(pueblo_os,
                      levels = c("Aymara", "Diaguita", "Mapuche"))
  ) %>%
  count(pueblo_os) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  rename(
    `Pueblo indígena de pertenencia` = pueblo_os,
    Frecuencia = n
  ) %>%
  bind_rows(
    tibble(
      `total estudiantes` = "Total",
      Frecuencia = sum(.$Frecuencia),
      Porcentaje = 100
    )
  )

tabla_pueblo_os %>%
  kable(
    col.names = c("Pueblo de pertenencia", "Frecuencia", "Porcentaje"),
    caption   = "Pueblos indígenas de pertenencia",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

#ACÁ HAY UN PROBLEMA. 



##ahora grafico


df_plot_pueblo_os <- base_antropologia %>%
  filter(!is.na(pueblo_os)) %>%
  count(pueblo_os) %>%
  mutate(
    pueblo_os = factor(pueblo_os, levels = c("Aymara", "Diaguita", "Mapuche")),
    porcentaje = round((n / sum(n)) * 100, 1)
  )

ggplot(df_plot_pueblo_os, aes(x = pueblo_os, y = porcentaje)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Pueblos indígenas de pertenencia del estudiantado",
    subtitle = "Encuesta de Estudiantes de Antropología UAH 2025",
    x = "Pertenencia",
    y = "Porcentaje"
  ) +
  theme_minimal(base_family = "Cambria") +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

### Este gráfico, como son pocas categorías puede ser de torta. 


## Sección Catalina Castro
#ne_p
# recodificación o limpieza si amerita

names(base_antropologia)

table(base_antropologia$ne_p)


base_antropologia <- base_antropologia %>%
  mutate(ne_p_r = case_when(
    ne_p %in% c("Doctorado", "Magíster o maestria") ~ "Posgrado",
    ne_p %in% c("Profesional", "Técnico nivel superior") ~ "Profesional/Técnico",
    ne_p %in% c("Educación media", "Educación media técnica profesional") ~ "Educación Media",
    ne_p %in% c("Educación básica", "No cuenta con estudios formales") ~ "Educación Básica o Sin Estudios",
    TRUE ~ ne_p  # Mantiene el resto de las categorías tal como están ("No cuenta con estudios formales", "No sé", etc.)
  ))

###reviso el resultado 
table(base_antropologia$ne_p_r)

# tabla formateada
#install.packages ("kableExtra")
library(kableExtra)

base_antropologia %>%
  filter(ne_p != "Sin respuesta") %>%
  count(ne_p) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    `Nivel Educativo` = ne_p,
    Frecuencia       = n
  ) %>%
  bind_rows(
    tibble(
      `Nivel Educativo` = "Total",
      Frecuencia        = sum(.$Frecuencia),
      Porcentaje        = 100
    )
  ) %>%
  kable(
    col.names = c("Nivel Educativo", "Frecuencia", "Porcentaje"),
    caption   = "Nivel Educativo de Padre de Estudiantes",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

# gráfico
# Crear dataframe con los conteos
df_plot <- base_antropologia %>%
  count(ne_p_r) %>%  
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)  # calcula % y redondea a 2 decimales
  )


# Grafico 1: recodificación
# versión: vertical
#library(tidyverse)

ggplot(df_plot, aes(
  x = fct_relevel(
    fct_reorder(ne_p_r, porcentaje, .desc = TRUE),
    "No sé",
    after = Inf
  ),
  y = porcentaje
)) +
  geom_col(fill = "#DDA0DD") +
  theme_minimal() +
  labs(
    x     = "Nivel educativo recodificado",
    y     = "Porcentaje (%)",
    title = "Distribución del Nivel Educativo"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# versión: horizontal con %
ggplot(df_plot, aes(
  x = porcentaje,
  y = fct_reorder(ne_p_r, porcentaje)
)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(
    aes(label = paste0(
      formatC(porcentaje, format = "f", digits = 1, decimal.mark = ","), 
      "%"
    )),
    hjust = -0.05,
    size  = 3
  ) +
  coord_cartesian(xlim = c(0, max(df_plot$porcentaje) * 1.1)) +
  theme_minimal() +
  labs(
    title   = "Nivel Educativo del Padre de Estudiantes de Antropología",
    caption = "Encuesta de Estudiantes de Antropología UAH 2025",
    x       = NULL,
    y       = NULL
  ) +
  theme(
    axis.text.y = element_text(size = 10)
  )

# Gráfico con todas las categorías con treemap: observar más especificamente 
# 1. Calcular frecuencias y porcentajes sobre 'ne_p'
df_plot_ne_p <- base_antropologia %>%
  count(ne_p) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  )

# 2. Gráfico no recodificado con Treemap con ne_p 
# este es bueno  para mostrar más categorías sin recodificar

if (!requireNamespace("cowplot", quietly = TRUE)) {
  install.packages("cowplot", dependencies = TRUE)
}
library(cowplot)


# 1. Preparar los datos
df_plot_ne_p <- base_antropologia %>%
  count(ne_p) %>%
  mutate(porcentaje = n / sum(n) * 100)

# 2. Definir paleta neon con tantos colores como categorías haya
n_levels   <- n_distinct(df_plot_ne_p$ne_p)
neon_colors <- c(
  "#C95C35", "#0A7575", "#8F9089", "#E86A92",
  "#A29F15", "#4682B4", "#B5651D", "#FF8C00",
  "#6B8E23", "#FFD700", "#7FFF00"
)[1:n_levels]

# 3. Crear el treemap con estética oscura y neón
p_treemap <- ggplot(df_plot_ne_p, aes(
  area  = porcentaje,
  fill  = ne_p,
  label = paste0(
    ne_p, "\n",
    formatC(porcentaje, format = "f", digits = 1, decimal.mark = ","), "%"
  )
)) +
  geom_treemap(colour = "#1E1D23", size = 0.2, alpha = 0.9) +
  geom_treemap_text(
    colour   = "white",
    place    = "centre",
    grow     = TRUE,
    reflow   = TRUE,
    fontface = "bold",
    min.size = 4
  ) +
  scale_fill_manual(values = neon_colors) +
  theme_void() +
  theme(legend.position = "none")

# 4. Barra de título neon
title_bar <- ggdraw() +
  draw_label(
    "DISTRIBUCIÓN DEL NIVEL EDUCATIVO",
    fontface   = "bold",
    fontfamily = "sans",     # CORRECTO: fontfamily en lugar de family
    size       = 24,
    colour     = "black",
    x          = 0.5,
    y          = 0.5,
    hjust      = 0.5,
    vjust      = 0.5
  ) +
  theme(
    plot.background = element_rect(fill = "#39FF14", colour = NA),
    plot.margin     = margin(0, 0, 0, 0)
  )

# 5. Combinar barra y treemap, y añadir caption al pie
combined <- plot_grid(
  title_bar,
  p_treemap,
  ncol        = 1,
  rel_heights = c(0.12, 1)
)

final_plot <- ggdraw(combined) +
  draw_label(
    "Encuesta de Estudiantes de Antropología UAH 2025",
    fontface   = "plain",
    fontfamily = "sans",
    size       = 10,
    colour     = "black",
    x          = 0.5,
    y          = 0.02,
    hjust      = 0.5,
    vjust      = 0
  )


print(final_plot)



#ne_m
# recodificación o limpieza si amerita

base_antropologia <- base_antropologia %>%
  mutate(ne_m_r = case_when(
    ne_m %in% c("Doctorado", "Magíster o maestria") ~ "Posgrado",
    ne_m %in% c("Profesional", "Técnico nivel superior") ~ "Profesional/Técnico",
    ne_m %in% c("Educación básica", "Educación media", "Educación media técnica profesional") ~ "Escolar",
    TRUE ~ ne_m  # Mantiene el resto de las categorías tal como están ("No cuenta con estudios formales", "No sé", etc.)
  ))

#reviso
table(base_antropologia$ne_m_r)

# tabla 

base_antropologia %>%
  filter(ne_m != "Sin respuesta") %>%
  count(ne_m) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    `Nivel Educativo` = ne_m,
    Frecuencia       = n
  ) %>%
  bind_rows(
    tibble(
      `Nivel Educativo` = "Total",
      Frecuencia        = sum(.$Frecuencia),
      Porcentaje        = 100
    )
  ) %>%
  kable(
    col.names = c("Nivel Educativo", "Frecuencia", "Porcentaje"),
    caption   = "Nivel Educativo de Madres de Estudiantes",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

# gráfico


#realizo el data frame para poder graficar

df_plot <- base_antropologia %>%
  count(ne_m_r) %>%  
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)  # calcula % y redondea a 2 decimales
  )

#Grafico

ggplot(df_plot, aes(
  x = porcentaje,
  y = fct_reorder(ne_m_r, porcentaje)
)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(
    aes(label = paste0(
      formatC(porcentaje, format = "f", digits = 1, decimal.mark = ","), 
      "%"
    )),
    hjust = -0.05,
    size  = 3
  ) +
  coord_cartesian(xlim = c(0, max(df_plot$porcentaje) * 1.1)) +
  theme_minimal() +
  labs(
    title   = "Nivel Educativo de la Madre de Estudiantes de Antropología",
    caption = "Encuesta de Estudiantes de Antropología UAH 2025",
    x       = NULL,
    y       = NULL
  ) +
  theme(
    axis.text.y = element_text(size = 10)
  )


#####clase_social

names(base_antropologia)
table(base_antropologia$clase_social)

unique(base_antropologia$clase_social)

# recodificación o limpieza si amerita

base_antropologia <- base_antropologia %>%
  mutate(clase_social_r = case_when(
    clase_social %in% c("Clase baja", "Clase media - baja") ~ "Clase baja",
    clase_social == "Clase media" ~ "Clase media",
    clase_social == "Clase media - alta" ~ "Clase media - alta",
    TRUE ~ clase_social
  ))

#reviso
table(base_antropologia$clase_social_r)

# tabla 

base_antropologia %>%
  filter(clase_social != "Sin respuesta") %>%
  count(clase_social) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    `Clase Social` = clase_social,
    Frecuencia       = n
  ) %>%
  bind_rows(
    tibble(
      `Clase Social` = "Total",
      Frecuencia        = sum(.$Frecuencia),
      Porcentaje        = 100
    )
  ) %>%
  kable(
    col.names = c("Clase Social", "Frecuencia", "Porcentaje"),
    caption   = "Clase social de Estudiantes",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

# gráfico

df_plot <- base_antropologia %>%
  count(clase_social_r) %>%  
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)  # calcula % y redondea a 2 decimales
  )

#Grafico

ggplot(df_plot, aes(
  x = porcentaje,
  y = fct_reorder(clase_social_r, porcentaje)
)) +
  geom_col(fill = "#DDA0DD") +
  geom_text(
    aes(label = paste0(
      formatC(porcentaje, format = "f", digits = 1, decimal.mark = ","), 
      "%"
    )),
    hjust = -0.05,
    size  = 3
  ) +
  coord_cartesian(xlim = c(0, max(df_plot$porcentaje) * 1.1)) +
  theme_minimal() +
  labs(
    title   = "Clase social de Estudiantes de Antropología",
    caption = "Encuesta de Estudiantes de Antropología UAH 2025",
    x       = NULL,
    y       = NULL
  ) +
  theme(
    axis.text.y = element_text(size = 10)
  )

#futuro_laboral_1

names(base_antropologia)
table(base_antropologia$fut_laboral_1)

# recodificación o limpieza si amerita

base_antropologia <- base_antropologia %>%
  mutate(fut_laboral_1_r = case_when(
    fut_laboral_1 %in% c("Consultoría (empresas privadas o proyectos independientes)", "Consultoría / investigación aplicada en empresas privadas", "Industria de recursos naturales (minería, energía, medio ambiente)") ~ "Sector privado",
    fut_laboral_1 %in% c("No creó encontrar trabajo en esto", "No sé") ~ "No sé",
    fut_laboral_1 == "Mundo académico (investigación y docencia universitaria)" ~ "Mundo académico",
    fut_laboral_1 == "Organismos estatales (ministerios, subsecretarías, municipios)" ~ "Organismos estatales",
    fut_laboral_1 == "Organismos internacionales (ONU, UNESCO, etc.)" ~ "Organismos internacionales",
    fut_laboral_1 == "Organizaciones no gubernamentales (ONGs) u organizaciones de la sociedad civil" ~ "Organizaciones sociales/ONG'S",
    TRUE ~ fut_laboral_1  
  ))

table(base_antropologia$fut_laboral_1_r)

# tabla 

base_antropologia %>%
  filter(fut_laboral_1_r != "Sin respuesta") %>%
  count(fut_laboral_1_r) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    `Elección futuro laboral` = fut_laboral_1_r,
    Frecuencia       = n
  ) %>%
  bind_rows(
    tibble(
      `Elección futuro laboral` = "Total",
      Frecuencia        = sum(.$Frecuencia),
      Porcentaje        = 100
    )
  ) %>%
  kable(
    col.names = c("Elección de sector laboral", "Frecuencia", "Porcentaje"),
    caption   = "Primera opción de futuro laboral",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

# gráfico
# 2. Calcular frecuencias y porcentajes
data_plot <- base_antropologia %>%
  count(fut_laboral_1_r, name = "n") %>%
  mutate(porcentaje = n / sum(n)) %>%
  arrange(desc(porcentaje)) %>%
  mutate(fut_laboral_1_r = factor(fut_laboral_1_r, levels = fut_laboral_1_r))

# 3. Graficar
ggplot(data_plot, aes(x = fut_laboral_1_r, y = porcentaje)) +
  geom_segment(aes(xend = fut_laboral_1_r, yend = porcentaje), y = 0, color = "pink") +
  geom_point(color = "purple", size = 4, alpha = 0.6) +
  geom_text(aes(label = percent(porcentaje, accuracy = 0.1)),
            hjust = -0.2, size = 4) +
  coord_flip() +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = "Porcentaje",
    title = "Primera opción futuro laboral"
  ) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.1)))

###futuro_laboral_2

names(base_antropologia)
table(base_antropologia$fut_laboral_1)

# recodificación o limpieza si amerita

base_antropologia <- base_antropologia %>%
  mutate(fut_laboral_2_r = case_when(
    fut_laboral_2 %in% c("Consultoría (empresas privadas o proyectos independientes)", "Consultoría / investigación aplicada en empresas privadas", "Industria de recursos naturales (minería, energía, medio ambiente)") ~ "Sector privado",
    fut_laboral_2 %in% c("No creó encontrar trabajo en esto", "No sé") ~ "No sé",
    fut_laboral_2 %in% c("Mundo académico (investigación y docencia universitaria)", "Magister en egiptología además de estudios para poder ser docente") ~ "Mundo académico",
    fut_laboral_2 == "Organismos estatales (ministerios, subsecretarías, municipios)" ~ "Organismos estatales",
    fut_laboral_2 == "Organismos internacionales (ONU, UNESCO, etc.)" ~ "Organismos internacionales",
    fut_laboral_2 == "Organizaciones no gubernamentales (ONGs) u organizaciones de la sociedad civil" ~ "Organizaciones sociales/ONG'S",
    TRUE ~ fut_laboral_2  
  ))

table(base_antropologia$fut_laboral_2_r)


# tabla 

base_antropologia %>%
  filter(fut_laboral_2_r != "Sin respuesta") %>%
  count(fut_laboral_2_r) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    `Elección futuro laboral` = fut_laboral_2_r,
    Frecuencia       = n
  ) %>%
  bind_rows(
    tibble(
      `Elección futuro laboral` = "Total",
      Frecuencia        = sum(.$Frecuencia),
      Porcentaje        = 100
    )
  ) %>%
  kable(
    col.names = c("Elección futuro laboral", "Frecuencia", "Porcentaje"),
    caption   = "Segunda opción futuro laboral",
    format    = "html",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )


# gráfico

data_plot <- base_antropologia %>%
  count(fut_laboral_2_r, name = "n") %>%
  mutate(porcentaje = n / sum(n)) %>%
  arrange(desc(porcentaje)) %>%
  mutate(fut_laboral_2_r = factor(fut_laboral_2_r, levels = fut_laboral_2_r))

# 3. Graficar
ggplot(data_plot, aes(x = fut_laboral_2_r, y = porcentaje)) +
  geom_segment(aes(xend = fut_laboral_2_r, yend = porcentaje), y = 0, color = "orchid") +
  geom_point(color = "purple", size = 4, alpha = 0.6) +
  geom_text(aes(label = percent(porcentaje, accuracy = 0.1)),
            hjust = -0.2, size = 4) +
  coord_flip() +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = "Porcentaje",
    title = "Segunda opción futuro laboral"
  ) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.1)))



#### SALUD MENTAL #####

glimpse(base_antropologia)

glimpse(libro_codigos)

##### SMe_01
#sme_01_tristeza

table(base_antropologia$s_me_tris)

#recodifico

base_antropologia <- base_antropologia %>%
  mutate(sme_tristeza_r = case_when(
    s_me_ansi == "Ocasionalmente" ~ "Ocasionalmente",
    s_me_ansi %in% c("Siempre", "Frecuentemente") ~ "Regularmente",
    s_me_ansi %in% c("Casi nunca", "Nunca") ~ "Muy pocas veces",
    TRUE ~ NA
  ))
#reviso
table(base_antropologia$sme_tristeza_r)

#tabla formateada

base_antropologia %>%
  filter(s_me_tris != "Sin respuesta") %>%
  count(s_me_tris) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    `Regularidad sensación de tristeza últimas dos semanas` = s_me_tris,
    Frecuencia       = n
  ) %>%
  bind_rows(
    tibble(
      `Regularidad sensación de tristeza últimas dos semanas` = "Total",
      Frecuencia        = sum(.$Frecuencia),
      Porcentaje        = 100
    )
  ) %>%
  kable(
    col.names = c("Regularidad", "Frecuencia", "Porcentaje"),
    caption   = "Regularidad sensación de tristeza últimas dos semanas",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

#gráfico

data_plot <- base_antropologia %>%
  count(sme_tristeza_r, name = "n") %>%
  mutate(porcentaje = n / sum(n)) %>%
  arrange(desc(porcentaje)) %>%
  mutate(sme_tristeza_r = factor(sme_tristeza_r, levels = sme_tristeza_r))

# 3. Graficar
ggplot(data_plot, aes(x = sme_tristeza_r, y = porcentaje)) +
  geom_segment(aes(xend = sme_tristeza_r, yend = porcentaje), y = 0, color = "orchid") +
  geom_point(color = "purple", size = 4, alpha = 0.6) +
  geom_text(aes(label = percent(porcentaje, accuracy = 0.1)),
            hjust = -0.2, size = 4) +
  coord_flip() +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = "Porcentaje",
    title = "Sensación de tristeza últimas dos semanas"
  ) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.1)))



#SMe_01_ansiedad
table(base_antropologia$s_me_ansi)

#recodifico
base_antropologia <- base_antropologia %>%
  mutate(sme_ansiedad_r = case_when(
    s_me_ansi == "Ocasionalmente" ~ "Ocasionalmente",
    s_me_ansi %in% c("Siempre", "Frecuentemente") ~ "Regularmente",
    s_me_ansi %in% c("Casi nunca", "Nunca") ~ "Muy pocas veces",
    TRUE ~ NA
  ))

#reviso

table(base_antropologia$sme_ansiedad_r)

#tabla formateada
base_antropologia %>%
  filter(s_me_ansi != "Sin respuesta") %>%
  count(s_me_ansi) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    Regularidad = s_me_ansi,
    Frecuencia  = n
  ) %>%
  bind_rows(
    tibble(
      Regularidad = "Total",
      Frecuencia  = sum(base_antropologia$s_me_estre != "Sin respuesta"),
      Porcentaje  = 100
    )
  ) %>%
  kable(
    col.names = c("Regularidad", "Frecuencia", "Porcentaje"),
    caption   = "Regularidad sensación de ansiedad últimas dos semanas",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

#gráfico

# 1. Crear los datos manualmente
data_plot <- data.frame(
  sme_ansiedad_r = c("Muy pocas veces", "Ocasionalmente", "Regularmente"),
  n = c(15, 36, 105)
) %>%
  mutate(
    porcentaje = n / sum(n),
    sme_ansiedad_r = factor(sme_ansiedad_r, levels = sme_ansiedad_r[order(porcentaje, decreasing = TRUE)])
  )

# 2. Gráfico lollipop
ggplot(data_plot, aes(x = sme_ansiedad_r, y = porcentaje)) +
  geom_segment(aes(xend = sme_ansiedad_r, yend = porcentaje), y = 0, color = "orchid") +
  geom_point(color = "purple", size = 4, alpha = 0.6) +
  geom_text(aes(label = percent(porcentaje, accuracy = 0.1)),
            hjust = -0.2, size = 4) +
  coord_flip() +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = "Porcentaje",
    title = "Sensación de ansiedad últimas dos semanas"
  ) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.1)))

#SMe_01_estres

table(base_antropologia$s_me_estre)

#recodifico

base_antropologia <- base_antropologia %>%
  mutate(sme_estres_r = case_when(
    s_me_ansi == "Ocasionalmente" ~ "Ocasionalmente",
    s_me_ansi %in% c("Siempre", "Frecuentemente") ~ "Regularmente",
    s_me_ansi %in% c("Casi nunca", "Nunca") ~ "Muy pocas veces",
    TRUE ~ NA
  ))

#reviso
table(base_antropologia$sme_estres_r)

#tabla formateada

base_antropologia %>%
  filter(s_me_estre != "Sin respuesta") %>%
  count(s_me_estre) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    Regularidad = s_me_estre,
    Frecuencia  = n
  ) %>%
  bind_rows(
    tibble(
      Regularidad = "Total",
      Frecuencia  = sum(base_antropologia$s_me_estre != "Sin respuesta"),
      Porcentaje  = 100
    )
  ) %>%
  kable(
    col.names = c("Regularidad", "Frecuencia", "Porcentaje"),
    caption   = "Regularidad sensación de estrés últimas dos semanas",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )


#grafico

# 1. Crear los datos manualmente
data_plot <- data.frame(
  sme_estres_r = c("Muy pocas veces", "Ocasionalmente", "Regularmente"),
  n = c(15, 36, 105)
) %>%
  mutate(
    porcentaje = n / sum(n),
    sme_estres_r = factor(sme_estres_r, levels = sme_estres_r[order(porcentaje, decreasing = TRUE)])
  )

# 2. Gráfico lollipop
ggplot(data_plot, aes(x = sme_estres_r, y = porcentaje)) +
  geom_segment(aes(xend = sme_estres_r, yend = porcentaje), y = 0, color = "orchid") +
  geom_point(color = "purple", size = 4, alpha = 0.6) +
  geom_text(aes(label = percent(porcentaje, accuracy = 0.1)),
            hjust = -0.2, size = 4) +
  coord_flip() +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = "Porcentaje",
    title = "Sensación de estrés últimas dos semanas"
  ) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.1)))

#####SMe_02
#observo

unique(base_antropologia$sme_02_que)

#Observo los datos

table(base_antropologia$sme_02_que)

#Recodifico

base_antropologia <- base_antropologia %>%
  mutate(estres_r = case_when(
    sme_02_que == "A diario" ~ "A diario",
    sme_02_que == "Al menos una vez por semana" ~ "Al menos una vez por semana",
    sme_02_que == "Al menos una vez al mes" ~ "Al menos una al mes",
    sme_02_que %in% c("Al menos una vez cada seis meses", "Una vez al año o menos") ~ "Una vez al año o menos",
    TRUE ~ NA
  ))

#Reviso 

table(base_antropologia$estres_r)

#tabla

base_antropologia %>%
  filter(sme_02_que != "Sin respuesta") %>%
  count(sme_02_que) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    `Regularidad sintomas físicos de estres` = sme_02_que,
    Frecuencia       = n
  ) %>%
  bind_rows(
    tibble(
      `Regularidad sintomas físicos de estres` = "Total",
      Frecuencia        = sum(.$Frecuencia),
      Porcentaje        = 100
    )
  ) %>%
  kable(
    col.names = c("Regularidad", "Frecuencia", "Porcentaje"),
    caption   = "Regularidad sintomas físicos de estres",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )


#grafico

# 1. Crear los datos manualmente
data_estres <- data.frame(
  estres_r = c(
    "A diario",
    "Al menos una vez por semana",
    "Al menos una al mes",
    "Una vez al año o menos"
  ),
  n = c(46, 62, 33, 13)
) %>%
  mutate(
    porcentaje = n / sum(n),
    estres_r = factor(estres_r, levels = estres_r[order(porcentaje, decreasing = TRUE)])
  )

# 2. Gráfico lollipop
ggplot(data_estres, aes(x = estres_r, y = porcentaje)) +
  geom_segment(aes(xend = estres_r, yend = porcentaje), y = 0, color = "orchid") +
  geom_point(color = "purple", size = 4, alpha = 0.6) +
  geom_text(aes(label = percent(porcentaje, accuracy = 0.1)),
            hjust = -0.2, size = 4) +
  coord_flip() +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = "Porcentaje",
    title = "Frecuencia de sensación de estrés (últimas dos semanas)"
  ) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.1)))


#####SMe_03_en_

#Observo los datos
table(base_antropologia$sme_03_en_)

#Recodifico

base_antropologia <- base_antropologia %>%
  mutate(
    tratamiento_psicologico_r = case_when(
      sme_03_en_ == "No" ~ "No",
      sme_03_en_ %in% c(
        "Sí, diagnosticado/a pero sin tratamiento",
        "Sí, otro tipo de tratamiento",
        "Sí, tratado/a con medicación",
        "Sí, tratado/a con medicación y psicoterapia",
        "Sí, tratado/a con psicoterapia"
      ) ~ "Sí",
      TRUE ~ NA_character_
    )
  )

#Reviso

table(base_antropologia$tratamiento_psicologico_r)

#tabla

base_antropologia %>%
  filter(tratamiento_psicologico_r != "Sin respuesta") %>%
  count(tratamiento_psicologico_r) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Porcentaje)) %>%
  rename(
    `Tratamiento psicologico` = tratamiento_psicologico_r,
    Frecuencia       = n
  ) %>%
  bind_rows(
    tibble(
      `Tratamiento psicologico` = "Total",
      Frecuencia        = sum(.$Frecuencia),
      Porcentaje        = 100
    )
  ) %>%
  kable(
    col.names = c("Respuesta", "Frecuencia", "Porcentaje"),
    caption   = "Estudiantes con y sin tratamiento psicológico",
    digits    = 2
  ) %>%
  kable_classic(
    full_width = FALSE,
    html_font  = "Cambria",
    font_size  = 15
  ) %>%
  footnote(
    general       = "Encuesta de Estudiantes de Antropología UAH 2025",
    general_title = ""
  )

#grafico

data <- data.frame(
  category = c("No", "Sí"),
  count = c(84, 72)
)

# Calcular proporciones y posiciones
data <- data %>%
  mutate(
    fraction = count / sum(count),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(category, "\n", count, " (", round(fraction * 100, 1), "%)")
  )

# Gráfico tipo dona
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 5) +
  scale_fill_brewer(palette = "Pastel1") +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "¿Ha recibido tratamiento psicológico?")+
scale_fill_manual(values = c("No" = "#CD96CD", "Sí" = "#DDA0DD"))



#### ================ TABLAS ANÁLISIS BIVARIADO  ========================== ######

#1. Clase social y 1ra opción fut. laboral 

base_antropologia %>%
  filter(!is.na(clase_social_r), !is.na(fut_laboral_1_r)) %>%
  select(fut_laboral_1_r, clase_social_r) %>%  # ahora primera variable = filas, segunda = columnas
  droplevels() %>%
  table(.) %>%
  addmargins(., margin = 2) %>%                # suma márgenes por columna
  prop.table(., margin = 2) %>%                # proporciones
  round(4) * 100


#2. Id. de género y tratamiento psicológico 

base_antropologia %>%
  filter(!is.na(identidad_genero_simple), !is.na(tratamiento_psicologico_r)) %>%
  select(tratamiento_psicologico_r, identidad_genero_simple) %>%  # ahora primera variable = filas, segunda = columnas
  droplevels() %>%
  table(.) %>%
  addmargins(., margin = 2) %>%                # suma márgenes por columna
  prop.table(., margin = 2) %>%                # proporciones
  round(4) * 100

#3. Id. de género y clase social

base_antropologia %>%
  filter(!is.na(identidad_genero_simple), !is.na(clase_social_r)) %>%
  select(clase_social_r, identidad_genero_simple) %>%  # ahora primera variable = filas, segunda = columnas
  droplevels() %>%
  table(.) %>%
  addmargins(., margin = 2) %>%                # suma márgenes por columna
  prop.table(., margin = 2) %>%                # proporciones
  round(4) * 100


#4. pueblo originario y clase social

base_antropologia %>%
  filter(!is.na(pueblo_o), !is.na(clase_social_r)) %>%
  select(clase_social_r, pueblo_o) %>%  # ahora primera variable = filas, segunda = columnas
  droplevels() %>%
  table(.) %>%
  addmargins(., margin = 2) %>%                # suma márgenes por columna
  prop.table(., margin = 2) %>%                # proporciones
  round(4) * 100

#5. futuro laboral y género

base_antropologia %>%
  filter(!is.na(fut_laboral_1_r), !is.na(identidad_genero_simple)) %>%
  select(fut_laboral_1_r, identidad_genero_simple) %>%  # ahora primera variable = filas, segunda = columnas
  droplevels() %>%
  table(.) %>%
  addmargins(., margin = 2) %>%                # suma márgenes por columna
  prop.table(., margin = 2) %>%                # proporciones
  round(4) * 100

#6. edad y sensación de ansiedad

base_antropologia %>%
  filter(!is.na(edad_r), !is.na(sme_ansiedad_r)) %>%
  select(edad_r, sme_ansiedad_r) %>%  # ahora primera variable = filas, segunda = columnas
  droplevels() %>%
  table(.) %>%
  addmargins(., margin = 2) %>%                # suma márgenes por columna
  prop.table(., margin = 2) %>%                # proporciones
  round(4) * 100


####ELEGIR 4

#Test de hipótesis 1
#1. Id. de género y tratamiento psicológico 

base_antropologia %>%
  filter(!is.na(identidad_genero_simple), !is.na(tratamiento_psicologico_r)) %>%
  select(tratamiento_psicologico_r, identidad_genero_simple) %>%  # ahora primera variable = filas, segunda = columnas
  droplevels() %>%
  table(.) %>%
  addmargins(., margin = 2) %>%                # suma márgenes por columna
  prop.table(., margin = 2) %>%                # proporciones
  round(4) * 100
#tabla de contingencia
tabla_genero_tratamiento <- base_antropologia %>%
  filter(!is.na(identidad_genero_simple), !is.na(tratamiento_psicologico_r)) %>%
  select(tratamiento_psicologico_r, identidad_genero_simple) %>%
  droplevels() %>%
  table()
print(tabla_genero_tratamiento)
#Test de fisher
tabla <- matrix(
  c(33, 43, 8,
    14, 48, 10),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    "tratamiento_psicologico_r" = c("No", "Sí"),
    "identidad_genero_simple" = c("Hombre cisgénero", "Mujer cisgénero", "Persona de género diverso")
  )
)

resultado_fisher <- fisher.test(tabla)
resultado_fisher$p.value
#p-valor < 0.05, rechazamos la hipótesis nula (H₀) de independencia.Hay una asociación significativa entre la identidad de género y el tratamiento psicológico en tu muestra.

#Test de hipótesis 2



########## ------ INDICE ----- #######







#2. Guardo la base de datos limpia


write.xlsx(
  base_antropologia,
  file      = "encuesta-antropologia-2025-depurada.xlsx",
  sheetName = "Datos Limpios",
  colNames  = TRUE,
  rowNames  = FALSE,
  overwrite = TRUE
)




#nota sebastián: también se topan las categorías, recomiendo gráfico geom_col() ordenado de mayor a menor







###hasta acá. 


# Realizada Amilcar

#Rename sd_03


#primero la cambio el nombre a la variable

table(base_antropologia$edad)
class(base_antropologia$edad)


mean(base_antropologia$edad)


#Proceso de recodificación
# Estandariza ‘edad’ a numérico y genera la variable de rangos
base_antropologia <- base_antropologia %>% 
  mutate(
    # 1. Limpieza mínima (convierte a numérico; asume que ya corregiste strings como en tu ejemplo)
    edad = as.numeric(edad),
    
    # 2. Agrupación por rangos
    edad_r = case_when(
      between(edad, 18, 20)          ~ "18-20",
      between(edad, 21, 23)          ~ "21-23",
      between(edad, 24, 29)          ~ "24-29",
      edad >= 30                     ~ "30 y más",
      TRUE                           ~ NA_character_     # valores faltantes o fuera de rango
    )
  )

table(base_antropologia$edad_r)



base_antropologia %>% 
  count(edad_r) %>%                            # cuenta casos por grupo de edad
  mutate(edad_r = fct_reorder(edad_r, n, .desc = TRUE)) %>%  # ordena barras
  ggplot(aes(x = edad_r, y = n)) +
  geom_col() +
  labs(
    x = "Grupo de edad",
    y = "Número de casos",
    title = "Distribución de edades (edad_r)"
  ) +
  theme_minimal()

 
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