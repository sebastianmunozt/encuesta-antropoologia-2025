# Ayudantía 8
# Análisis de datos numéricos 

# Cargar Paquetes 
pacman::p_load(tidyverse,# Universo de paquetes : tidyr, dplyr, ggplot2,readr,purrr,tibble, stringr, forcats
               openxlsx,#Leer archivos xlsx
               readxl,#Leer archivos xl      
               writexl,#Guardar tablas formato excel
               DataExplorer,#Exploración rápida
               summarytools,#Contiene la función freq() que muestra las frecuencias absolutas y relativas de una variable
               kableExtra,#Tablas elegantes
               webshot2,#Exportar tablas
               knitr,#Dar formato a las tablas
               gt,#Dar formato a las tablas
               dplyr,#Manipulación de datos (%>% , count, mutate)
               ggplot2,#Sistema de gráficos
               forcats,#Manipulación de factores (fct_reorder)
               scales,#Para mostrar porcentajes correctamente
               viridis,#Paletas de colores 
               hrbrthemes,#Permite aplicar temas (theme_ipsum)
               DescTools)#Permite sacar coef de gini

# Importar base de datos
cuestionario <- read.xlsx("Cuestionario Antropología 2025.xlsx")

cuestionario <- read.xlsx("Cuestionario Antropología 2025.xlsx") %>%
  dplyr::select(5:ncol(.)) # seleccionar desde la columna 3 en adelante

# Explorar base
glimpse(cuestionario)
str(cuestionario)
summary(cuestionario)
names(cuestionario)

# Limpieza de datos: Renombrar variables----------------------------------------

# Homologar nombres
cuestionario <- janitor::clean_names(cuestionario)
names(cuestionario)

# Renombrar variables
cuestionario <- cuestionario %>%
  dplyr::rename(
    sd_01_edad = sd_01_que_edad_tiene_ingrese_solo_numeros_no_ponga_la_palabra_anos,
    sd_02_genero = sd_02_se_entiende_por_identidad_de_genero_la_vivencia_interna_y_personal_del_genero_tal_como_cada_persona_lo_experimenta_profundamente_esta_puede_coincidir_o_no_con_el_sexo_asignado_al_momento_del_nacimiento_asi_las_personas_cisgenero_se_identifican_con_la_identidad_de_genero_que_se_les_asigno_al_nacer_y_no_buscan_transitar_de_genero_por_otro_lado_las_personas_transgenero_no_se_identifican_con_la_identidad_de_genero_que_se_les_asigno_al_nacer_y_por_lo_tanto_transitan_de_genero_para_expresar_aquel_que_realmente_les_identifica_segun_estas_definiciones_cual_es_su_identidad_de_genero,
    sd_03_anio_carrera = sd_03_en_que_ano_ingreso_a_la_carrera,
    sd_04_comuna_actual = sd_04_en_que_comuna_de_la_region_metropolitana_reside_actualmente_si_vive_fuera_de_la_region_metropolitana_marque_otra,
    sd_05_procedencia = sd_05_proviene_de_una_region_distinta_a_la_region_metropolitana_si_usted_realizo_la_mayoria_de_su_educacion_secundaria_fuera_de_la_region_metropolitana_ya_sea_en_otras_regiones_de_chile_o_el_extranjero_deberia_contestar_si,
    sd_06_region = sd_06_de_que_region_proviene_en_caso_de_seleccionar_otro_especifique_cual,
    sd_07_tiempo_recorr = sd_07_cuanto_tiempo_tarda_aproximadamente_en_llegar_desde_su_comuna_de_residencia_hasta_la_universidad,
    sd_08_nacionalidad = sd_08_indique_su_nacionalidad_en_caso_de_seleccionar_otro_especifique_cual,
    sd_09_m_pais = sd_09_m_indique_el_pais_de_origen_de_su_madre_o_figura_materna_en_caso_de_seleccionar_otro_pais_especifique_cual,
    sd_09_p_pais = sd_09_p_indique_el_pais_de_origen_de_su_padre_o_figura_paterna_en_caso_de_seleccionar_otro_pais_especifique_cual,
    sd_10_o_pueblos_orig = sd_10_o_en_esta_pregunta_se_consideran_los_pueblos_originarios_reconocidos_por_el_estado_chileno_indique_cual_es_el_pueblo_originario_al_que_pertenece_en_caso_de_seleccionar_otro_especifique_cual,
    sd_10_pert_pueblos_orig = sd_10_pertenece_usted_a_un_pueblo_originario_si_indica_si_ingrese_cual,
    sd_11_nivel_educ_padre = sd_11_indique_el_maximo_nivel_educativo_completado_obtenido_por_su_padre_o_figura_paterna,
    sd_12_nivel_educ_madre = sd_12_indique_el_maximo_nivel_educativo_completado_obtenido_por_su_madre_o_figura_materna,
    sd_13_clase_social = sd_13_en_la_sociedad_existen_comunmente_distintos_grupos_o_clases_sociales_las_personas_de_clase_alta_son_aquellas_que_cuentan_con_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_baja_por_su_parte_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_tambien_categorias_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_considera_que_pertenece,
    sd_14_a_ambito_laboral = sd_14_a_una_vez_que_obtengas_tu_titulo_en_antropologia_en_que_ambito_laboral_te_gustaria_iniciar_tu_carrera_como_primera_opcion,
    sd_14_b_ambito_laboral = sd_14_b_una_vez_que_obtengas_tu_titulo_en_antropologia_en_que_ambito_laboral_te_gustaria_iniciar_tu_carrera_como_segunda_opcion,
    rs_01_frec_diaria = rs_01_con_que_frecuencia_diaria_promedio_usa_las_redes_sociales_en_actividades_que_no_son_laborales_ni_academicas_durante_la_semana_para_obtener_obtener_un_valor_aproximado_puede_observarlo_mediante_la_suma_de_los_tiempos_que_pasa_en_sus_apliaciones_mas_utilizadas_por_dia_como_instagram_facebook_tik_tok_por_ejemplo_si_en_promedio_pasa_2_horas_al_dia_en_instagram_y_2_hora_al_dia_en_tik_tok_tendria_un_total_de_4_horas_por_lo_que_deberia_marcar_entre_3_y_4_horas,
    rs_02_distraccion = rs_02_considera_que_el_uso_de_las_redes_sociales_le_distrae_de_otras_actividades_de_su_vida_cotidiana_tales_como_estudio_actividades_de_ocio_quehaceres_del_hogar_etc,
    rs_03_01_mas_tiempo = rs_03_01_cual_es_la_red_social_en_la_que_pasa_mas_tiempo,
    rs_03_02_mas_tiempo = rs_03_02_cual_es_la_segunda_red_social_en_la_que_pasa_mas_tiempo,
    rs_03_03_notif = rs_03_03_cuales_redes_sociales_mantiene_con_notificaciones_activas_seleccione_las_alternativas_que_estime_conveniente,
    rs_03_04_uso = rs_03_04_el_uso_que_mayormente_usted_le_da_a_una_red_es_social_es_es_para,
    rs_03_05_uso = rs_03_05_el_segundo_uso_que_mayormente_usted_le_da_a_una_red_es_social_es_es_para,
    rs_04_clases = rs_04_utiliza_redes_sociales_dentro_de_la_sala_de_clases,
    rs_05_frec_revis = rs_05_con_que_frecuencia_revisa_sus_redes_sociales_mientras_esta_realizando_una_tarea_academica_o_laboral_que_requiere_su_concentracion,
    rs_06_ansied_estr = rs_06_alguna_vez_ha_sentido_ansiedad_o_estres_al_no_poder_usar_sus_redes_sociales,
    rs_07_relac = rs_07_como_sientes_que_el_uso_de_redes_sociales_impacta_tus_relaciones_cercanas_como_amigxs_familia_pareja_en_tu_vida_cotidiana,
    rs_08_neces = rs_08_has_sentido_la_necesidad_de_revisar_tus_redes_sociales_inmediatamente_despues_de_despertar_o_antes_de_dormir,
    rs_09_sobreest = rs_09_que_tan_frecuentemente_sientes_que_estas_sobreestimulado_a_por_el_uso_constante_de_pantallas_y_redes_sociales_por_ejemplo_sensacion_de_agotamiento_irritabilidad_fatiga_mental,
    rs_10_autoest = rs_10_has_experimentado_problemas_de_autoestima_debido_al_uso_de_redes_sociales,
    rs_11_desconect = rs_11_le_parece_util_desconectarse_de_las_redes_sociales_para_mejorar_su_bienestar_emocional,
    rs_12_desinstal = rs_12_que_tan_frecuentemente_desinstala_aplicaciones_por_su_bienestar_emocional,
    s_me_01_tristeza = s_me_01_en_las_ultimas_dos_semanas_con_que_frecuencia_ha_experimentado_los_siguientes_tres_sintomas_tristeza,
    s_me_01_ansiedad = s_me_01_en_las_ultimas_dos_semanas_con_que_frecuencia_ha_experimentado_los_siguientes_tres_sintomas_ansiedad,
    s_me_01_estres = s_me_01_en_las_ultimas_dos_semanas_con_que_frecuencia_ha_experimentado_los_siguientes_tres_sintomas_estres,
    sme_02_sintomas = sme_02_que_tan_a_menudo_experimenta_sintomas_fisicos_asociados_al_estres_como_dolores_de_cabeza_tension_muscular_o_problemas_de_sueno_es_importante_destacar_que_estos_sintomas_los_relacione_con_estres_y_no_con_otras_causas_como_resfrios_ejercicios_etcetera,
    sme_03_diagnos_trat = sme_03_en_los_ultimos_12_meses_has_sido_diagnosticado_a_o_tratado_a_por_un_profesional_por_algun_trastorno_psicologico_o_psiquiatrico_por_ejemplo_anorexia_ansiedad_trastorno_por_deficit_de_atencion_e_hiperactividad_tdah_trastorno_bipolar_bulimia_depresion_insomnio_otros_trastornos_del_sueno_trastorno_obsesivo_compulsivo_toc_ataques_de_panico_fobias_esquizofrenia_abuso_o_adiccion_a_sustancias_alcohol_u_otras_drogas_otras_adicciones_por_ejemplo_juego_internet_o_conductas_sexuales_considera_el_diagnostico_y_tratamiento_realizado_solo_por_profesionales,
    sm_01_seg_barrio = sm_01_cuan_seguro_o_inseguro_percibe_usted_su_barrio,
    sm_02_seg_comuna = sm_02_que_tan_seguro_a_se_siente_usted_al_caminar_solo_a_por_su_comuna_de_residencia,
    sm_03_espacio = sm_03_en_cual_de_estos_espacios_publicos_se_siente_mas_inseguro_a,
    sm_04_seg_hora = sm_04_en_que_franja_horaria_se_siente_mas_inseguro_a_de_transitar_los_espacios_publicos,
    sm_05_seg_transp = sm_05_en_que_tipo_de_transporte_se_siente_mas_inseguro_a,
    sm_06_seg_ruta = sm_06_al_momento_de_transportarse_cotidianamente_selecciona_usted_alguna_ruta_que_le_haga_sentirse_mas_seguro_a,
    sm_07_medidas = sm_07_cuales_de_estas_medidas_de_seguridad_usted_adopta_para_sentirse_mas_seguro_a,
    sm_08_app = sm_08_utiliza_algun_tipo_de_aplicacion_para_reportar_incidentes_por_ejemplo_sosafe,
    sm_09_instit = sm_09_en_caso_de_presenciar_algun_incidente_ha_acudido_a_alguna_institucion_de_seguridad_por_ejemplo_carabineros_pdi_seguridad_ciudadana,
    sm_10_activ = sm_10_ha_dejado_de_realizar_alguna_actividad_por_sentirse_inseguro_a_puede_marcar_mas_de_1,
    pm_01_01_genero_music = pm_01_01_que_genero_s_musical_es_suele_escuchar_con_mayor_frecuencia,
    pm_01_02_genero_music = pm_01_02_que_genero_s_el_segundo_genero_musical_es_que_suele_escuchar_con_mayor_frecuencia,
    pm_02_frec_escucha = pm_02_con_que_frecuencia_escucha_musica,
    pm_03_fan = pm_03_sigue_activamente_a_algun_artista_o_banda,
    pm_03_01_fan_año = pm_03_01_mencione_a_1_artista_o_banda_que_mas_ha_seguido_y_le_ha_provocado_mayor_interes_durante_el_ultimo_ano,
    pm_04_asist_evento = pm_04_en_su_vida_ha_ido_alguna_vez_a_ver_algun_recital_concierto_tocata_o_espectaculo_de_musica_en_vivo,
    pm_05_asist_cuantos = pm_05_ha_asistido_a_eventos_musicales_en_el_ultimo_ano_a_cuantos,
    pm_06_impedim = pm_06_que_es_lo_que_mas_le_impide_participar_de_eventos_seleccione_hasta_3,
    pm_07_instrum = pm_07_toca_algun_instrumento_musical,
    pm_08_crea_music = pm_08_realiza_alguna_forma_de_creacion_musical,
    pm_09_lugar = pm_09_en_que_lugar_suele_crear_musica,
    influencia_music = crees_que_la_antropologia_ha_influido_en_tus_practicas_musicales_en_caso_de_que_tu_respuesta_sea_si_cuentanos_tu_experiencia,
    cd_01_presentaciones = cd_01_considera_importante_que_durante_las_clases_el_la_docente_utilice_presentaciones_visualmente_atractivas_y_bien_organizadas_para_apoyar_la_comprension_de_los_temas,
    cd_02_oratoria = cd_02_para_usted_es_importante_que_el_la_docente_tenga_una_buena_oratoria_y_proyecte_su_voz_de_manera_que_facilite_la_atencion_y_comprension_en_clase,
    cd_03_preguntas = cd_03_cuan_importante_considera_que_el_la_docente_haga_preguntas_a_los_estudiantes_para_fomentar_su_participacion_y_asegurar_la_comprension_del_contenido,
    cd_04_dinamicas = cd_04_que_tan_importante_es_para_usted_que_el_la_docente_incorpore_dinamicas_como_juegos_o_ejercicios_interactivos_para_reforzar_el_aprendizaje_en_clase,
    cd_05_grup = cd_05_hasta_que_punto_considera_importante_que_el_la_docente_incorpore_ejercicios_grupales_en_sus_clases_para_promover_el_aprendizaje_colaborativo,
    cd_06_dispo = cd_06_considera_relevante_que_el_la_docente_tenga_disponibilidad_horaria_fuera_de_clases_para_responder_a_las_dudas_de_los_estudiantes_por_correo_reuniones_virtuales_o_presenciales_u_otros_medios,
    cd_07_programa = cd_07_considera_importante_que_el_la_docente_se_cina_a_la_programacion_pautada_al_principio_de_semestre,
    cd_08_retroalim = cd_08_en_que_medida_considera_importante_que_el_la_docente_proporcione_retroalimentacion_detallada_y_clara_sobre_el_desempeno_en_trabajos_y_evaluaciones,
    cd_09_plazos = cd_09_que_tan_importante_es_para_usted_que_el_la_docente_sea_flexible_al_momento_de_extender_plazos_de_entrega_cuando_existan_justificaciones_adecuadas,
    cd_10_recursos_alt = cd_10_cree_que_es_necesario_que_el_la_docente_tenga_disponibilidad_de_recursos_alternativos_en_caso_de_inasistencia_justificadas_pruebas_recuperativas_examenes_orales,
    as_01_conoce_metod = as_01_cuanto_cree_que_sabe_sobre_los_metodos_de_autocuidado_sexual_y_reproductivos_para_prevenir_enfermedades_de_transmision_sexual_y_embarazos_no_deseados,
    as_02_cual_metod = as_02_cuales_de_los_siguientes_metodos_anticonceptivos_conoce_seleccione_todas_las_opciones_que_correspondan,
    as_03_ets = as_03_cuales_de_las_siguientes_enfermedades_de_transmision_sexual_ets_conoce,
    as_04_its = as_04_conoce_alguna_de_las_siguientes_formas_de_infecciones_de_transmision_sexual_its,
    as_05_confianza = as_05_tiene_alguien_de_confianza_con_quien_hablar_de_su_salud_sexual,
    as_06_fuentes = as_06_cuales_son_las_principales_fuentes_que_utiliza_para_informarse_sobre_metodos_anticonceptivos,
    as_07_particip = as_07_ha_participado_en_instancias_de_difusion_sobre_conocimiento_de_autocuidado_sexual_talleres_conversatorios_charlas_ferias_universitarias_etc,
    as_08_situacion = as_08_cual_de_las_siguientes_afirmaciones_describe_mejor_su_situacion_actual_respecto_a_la_actividad_sexual,
    as_09_metodos = as_09_que_metodos_anticonceptivos_ha_utilizado_en_los_ultimos_seis_meses,
    as_10_chequeos = as_10_con_que_frecuencia_acude_a_chequeos_medicos_relacionados_con_su_salud_sexual,
    as_11_import_chequeos = as_11_del_uno_al_cinco_encuentra_usted_importante_la_realizacion_de_chequeos_de_salud_sexual_y_reproductiva_de_manera_periodica,
    comentario = fin_de_la_encuesta_si_desea_puede_realizar_algun_comentario_adicional
  )

names(cuestionario)

# Análisis de datos numéricos---------------------------------------------------
# Variables de interés: sd_01_edad 

# Verificar categorías
unique(cuestionario$sd_01_edad)

# Análisis Univariado ####

# I. Medidas de tendencia central #### 

# Queremos analizar la variable edad: sd_01_edad
# Revisamos la clase o tipo de la variable para asegurarnos de que sea numérica
class(cuestionario$sd_01_edad)

# Si no es numérica (por ejemplo, si es "character"), la convertimos:
cuestionario$sd_01_edad <- as.numeric(cuestionario$sd_01_edad)
# Pero en este caso no es necesario, pues ya es numerica

# 1. Media (promedio) de edad
## En R base: usamos `mean()` con `na.rm = TRUE` para ignorar los NA
mean(cuestionario$sd_01_edad, na.rm = TRUE) 

#🔶Práctica
# Calcule la media de la variable sd_03_anio_carrera (con r base)
# mean(datos$______, na.rm = ______) 

## En tidyverse: usamos `summarise()` dentro de un %>%
cuestionario %>%
  summarise(media_edad = mean(sd_01_edad, na.rm = TRUE))

#🔶Práctica
# Calcule la media de la variable sd_03_anio_carrera (con tidyverse)
# datos %>%
#   summarise(media_____ = mean(____, na.rm = ______))

# 2. Mediana: valor que divide la distribución en dos partes iguales
median(cuestionario$sd_01_edad, na.rm = TRUE)

#🔶Práctica
# Calcule la mediana de la variable sd_03_anio_carrera (con r base)
# ______(datos$____, na.rm = _____)

cuestionario %>%
  summarise(mediana_edad = median(sd_01_edad, na.rm = TRUE))

#🔶Práctica
# Calcule la mediana de la variable sd_03_anio_carrera (con tidyverse)
# datos %>%
#   summarise(mediana_____ = ______(______, na.rm = ______))

# 3. Moda: valor que más se repite 
# Necesitamos haber definido previamente una función llamada Mode()
# Función para calcular la moda (valor más frecuente)
Mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- na.omit(x)  # elimina los NA si corresponde
  }
  ux <- unique(x)  # identifica los valores únicos
  ux[which.max(tabulate(match(x, ux)))]  # devuelve el más frecuente
}

# Calculamos la moda
Mode(cuestionario$sd_01_edad, na.rm = TRUE)

#🔶Práctica
# Calcule la moda de la variable sd_03_anio_carrera 
# ______(______$______, na.rm = ________) 

# II. Medidas de dispersión ####

# Estas medidas muestran cuánto varían los datos respecto a la media

# 1. Varianza 
# La varianza mide cuánto se dispersan los datos respecto a la media 
# Es decir, cuánto varían los valores entre sí y respecto al promedio

# Calcular la varianza manualmente paso a paso:
# paso 1: calcular media
media_edad <- mean(cuestionario$sd_01_edad, na.rm = TRUE)  

# paso 2: calcular suma de cuadrados
sum_cuadrados_dif_edad <- sum((cuestionario$sd_01_edad - media_edad)^2, na.rm = TRUE)  

# paso 3: n sin NA: eliminamos NA y dividimos la suma de cuadrados de las diferencias entre n-1
n_edad <- sum(!is.na(cuestionario$sd_01_edad))
varianza_edad <- sum_cuadrados_dif_edad / (n_edad - 1)  
# Fórmula de varianza: dividir la suma de los cuadrados de las diferencias entre n-1

# Imprimir la varianza para ver el resultado
varianza_edad 

# Alternativa más directa con función var()
var_edad <- var(cuestionario$sd_01_edad, na.rm = TRUE)
var_edad

# Interpretamos
# En promedio, los valores de edad se desvían al cuadrado unos 13.93 años cuadrados respecto a la media
# Pero los "años cuadrados" no son muy intuitivos, por eso casi siempre se prefiere la desviación estándar, 
# que es la raíz cuadrada de la varianza

# 2. Desviación estándar 
# Raíz cuadrada de la varianza
# Es mejor interpretar la DE porque usa las mismas unidades que la variable original, 
# mientras que la varianza usa unidades al cuadrado, que no tienen mucho sentido práctico

# Calculamos DE
# Sacando la raíz cuadrada de la varianza:
sqrt(var(cuestionario$sd_01_edad, na.rm = TRUE))

# O calculándola directamente: 
sd(cuestionario$sd_01_edad, na.rm = TRUE)

# Interpretamos:
# En promedio, los valores de edad se desvían 3.7 años respecto del promedio, 
# Las edades varían, en promedio, unos 3.7 años respecto a la media.

# III. summarise y group_by para comparaciones-----------------------------------------------
# Vamos a realizar comparaciones de las edades respecto a las personas que 
# cuentan con diagnóstico y/o tratamiento psicológico y/o psiquiátrico
# Comparando por género autopercibido

# Utilizamos las variables: 
#sd_01_edad: edad de estudiantes de antropología

#sme_03_diagnos_trat: si están diagnosticadxs y tratadxs psicológica y/o psiquiátricamente 
# ("Sí, tratado/a con medicación y psicoterapia")

#sd_02_genero: género autoidentificado

# SUMMARISE: Sumario/resumen 
# Calculamos medidas resumen de las edades para toda la muestra (sumario general edades)
cuestionario %>%
  summarise(media = mean(sd_01_edad, na.rm = TRUE),
            mediana = median(sd_01_edad, na.rm = TRUE),
            desv.est = sd(sd_01_edad, na.rm = TRUE),
            minimo = min(sd_01_edad, na.rm = TRUE), #agrego minimo
            maximo = max(sd_01_edad, na.rm = TRUE)) #agrego maximo

# COMPARACIONES
# Ahora calculamos los datos de edades de las personas tratadas con medicación y psicoterapia
# calculamos media, mediana y desviación estándar de las edades, segmentadas por género 
# solo para quienes respondieron "sí" a diagnóstico y tratamiento.
cuestionario %>%
  filter(sme_03_diagnos_trat == "Sí, tratado/a con medicación y psicoterapia") %>%
  group_by(sd_02_genero) %>%
  summarise(media = mean(sd_01_edad, na.rm = TRUE),
            mediana = median(sd_01_edad, na.rm = TRUE),
            desv.est = sd(sd_01_edad, na.rm = TRUE)) 

# Interpretación:
# Quienes se identifican como hombres cisgénero tienen la edad media más alta (26.4 años),
# con una alta dispersión (desviación estándar de 7 años), lo que sugiere mayor heterogeneidad 
# en edades dentro del grupo.
# Las personas de género fluido tienen una edad media de 23.5 años, con una dispersión muy baja (0.707), 
# lo que indica que tienen edades muy similares (casi todas alrededor de 23 años).
# Las mujeres cisgénero tienen una edad media de 22.1 años, también con una desviación estándar moderada (2.98), 
# es decir, hay algo de diversidad en edades pero no tanta como en hombres cisgénero.
# Para "No binarie" y "Ser vivo", no hay desviación estándar porque probablemente solo hay un caso por grupo, 
# por lo que no se puede calcular variabilidad.

# Comparamos con los datos de edades de las personas diagnosticadas sin tratamiento (de la variable sme_03_diagnos_trat)
# Se calcularon la media, mediana y desviación estándar de la edad, por género, para las personas que están diagnosticadas sin tratamiento.
cuestionario %>%
  filter(sme_03_diagnos_trat == "Sí, diagnosticado/a pero sin tratamiento") %>%
  group_by(sd_02_genero) %>%
  summarise(media = mean(sd_01_edad, na.rm = TRUE),
            mediana = median(sd_01_edad, na.rm = TRUE),
            desv.est = sd(sd_01_edad, na.rm = TRUE)) 

# Interpretación:
# La edad promedio en los hombres cisgénero es de 22.5 años, con una mediana ligeramente menor (22), 
# lo que sugiere una distribución apenas sesgada hacia edades mayores. La desviación estándar es de 2.52, 
# indicando una variabilidad moderada en las edades (varian moderadamente).
# La edad promedio de mujeres cisgénero presenta una media de edad de 23.1 años y una mediana de 23, 
# valores prácticamente iguales, lo que sugiere una distribución simétrica. La desviación estándar es de 2.85, 
# ligeramente mayor que en los hombres cis, lo cual indica una variabilidad también moderada. 
# Este grupo tiene una media y mediana iguales de 22 años. Sin embargo, no se puede calcular la desviación estándar (NA), 
# lo que sugiere que solo hay un caso en esta categoría. Esto limita el análisis estadístico, 
# pero muestra la presencia de identidades de género disidentes que se presentan como personas diagnosticadas sin tratamiento


# ARRANGE: Ordenar ####
# Ordenamos las personas tratadas por edad promedio (de mayor a menor)
cuestionario %>%
  filter(sme_03_diagnos_trat == "Sí, tratado/a con medicación y psicoterapia") %>%
  group_by(sd_02_genero) %>%
  summarise(media = mean(sd_01_edad, na.rm = TRUE),
            mediana = median(sd_01_edad, na.rm = TRUE),
            desv.est = sd(sd_01_edad, na.rm = TRUE)) %>%
  arrange(desc(media)) #desc = descendiente, ordena medias de mayor a menor

# Guardamos
edad_por_trat_media <- cuestionario %>%
  filter(sme_03_diagnos_trat == "Sí, tratado/a con medicación y psicoterapia") %>%
  group_by(sd_02_genero) %>%
  summarise(media = mean(sd_01_edad, na.rm = TRUE),
            mediana = median(sd_01_edad, na.rm = TRUE),
            desv.est = sd(sd_01_edad, na.rm = TRUE)) %>%
  arrange(desc(media))

# Ordenamos por mediana de edad
edad_por_trat_mediana <- cuestionario %>%
  filter(sme_03_diagnos_trat == "Sí, tratado/a con medicación y psicoterapia") %>%
  group_by(sd_02_genero) %>%
  summarise(media = mean(sd_01_edad, na.rm = TRUE),
            mediana = median(sd_01_edad, na.rm = TRUE),
            desv.est = sd(sd_01_edad, na.rm = TRUE)) %>%
  arrange(desc(mediana))

# Ordenamos por desviación estándar (más variabilidad en edades)
edad_por_desvio <- cuestionario %>%
  filter(sme_03_diagnos_trat== "Sí, tratado/a con medicación y psicoterapia") %>%
  group_by(sd_02_genero) %>%
  summarise(media = mean(sd_01_edad, na.rm = TRUE),
            mediana = median(sd_01_edad, na.rm = TRUE),
            desv.est = sd(sd_01_edad, na.rm = TRUE)) %>%
  arrange(desc(desv.est))

# Incluimos también mínimos y máximos
edad_por_media_minmax <- cuestionario %>%
  filter(sme_03_diagnos_trat == "Sí, tratado/a con medicación y psicoterapia") %>%
  group_by(sd_02_genero) %>%
  summarise(media = mean(sd_01_edad, na.rm = TRUE),
            mediana = median(sd_01_edad, na.rm = TRUE),
            desv.est = sd(sd_01_edad, na.rm = TRUE),
            minimo = min(sd_01_edad, na.rm = TRUE),
            maximo = max(sd_01_edad, na.rm = TRUE)) %>%
  arrange(desc(media))

#🔶Ejercicio práctico: calcular resumen de MTC para la variable sd_03_anio_carrera, de clase media (sd_13_clase_social)
# agrupando por género (sd_02_genero)
cuestionario %>%
  filter(sd_13_clase_social == "Clase media") %>%
  group_by(sd_02_genero) %>%
  summarise(media = mean(sd_03_anio_carrera, na.rm = TRUE),
            mediana = median(sd_03_anio_carrera, na.rm = TRUE),
            desv.est = sd(sd_03_anio_carrera, na.rm = TRUE),
            minimo = min(sd_03_anio_carrera, na.rm = TRUE),
            maximo = max(sd_03_anio_carrera, na.rm = TRUE)) %>%
  arrange(desc(media))

#Ahora calcular para la clase baja
cuestionario %>%
  filter(___________ == "___________") %>%
  group_by(sd_02_genero) %>%
  summarise(media = mean(sd_03_anio_carrera, na.rm = TRUE),
            mediana = median(sd_03_anio_carrera, na.rm = TRUE),
            desv.est = sd(sd_03_anio_carrera, na.rm = TRUE),
            minimo = min(sd_03_anio_carrera, na.rm = TRUE),
            maximo = max(sd_03_anio_carrera, na.rm = TRUE)) %>%
  arrange(desc(media))

# IV. Coeficiente de Gini (no es muy habitual en edad, pero se puede calcular)------------------
library(DescTools)

Gini(cuestionario$sd_01_edad, na.rm = TRUE)

# Interpretamos:
# Este valor está muy cerca de 0, lo que significa que la distribución de las edades es bastante homogénea 
# o equitativa dentro del conjunto total de personas encuestadas.

# Coeficiente de Gini por personas diagnosticadas y/o tratadas (distribución desigual de edades)
cuestionario %>%
  group_by(sme_03_diagnos_trat) %>%
  summarise(coeficiente_gini = Gini(sd_01_edad, na.rm = TRUE), 
            media = mean(sd_01_edad, na.rm = TRUE)) %>% 
  arrange(desc(coeficiente_gini))

# Interpretación:
# Primera categoría tiene el coeficiente de Gini más alto (0.0853), lo que indica que la distribución de edades 
# entre las personas que no tienen un diagnóstico o tratamiento es la más desigual en comparación con las otras categorías. 
# Su edad media es de 22.2 años.
# última categoría Esta categoría tiene el coeficiente de Gini más bajo (0.0149), lo que significa que 
# la distribución de edades es la más equitativa o menos desigual entre todos los grupos. 
# Su edad media es de 22.3 años.

