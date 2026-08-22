#-----------------------------------------------------------------------------
# code1.R
# Este script hace básicamente 6 cosas, EN ESTE ORDEN:
# 1) Carga librerías
# 2) Define funciones (gráficos, traducción, exploración de variables)
# 3) Limpia y transforma datos de encuesta (survey, race_long, life_long,
#    race_dummies, life_dummies, survey_enriched) — TODO esto queda igual
#    que en el code1 original y NUNCA se modifica.
# 4) EXPLORA cada variable de las bases ORIGINALES tal cual (tipo + valores
#    únicos reales) y exporta ese resumen a outputs/exploracion_variables.csv
# 5) Construye la BASE OFICIAL DE TRABAJO: survey_es, que es la versión
#    ENRIQUECIDA (incluye las variables race_*/life_* generadas) Y TRADUCIDA
#    de survey_enriched — con los NOMBRES de columna en español y los
#    VALORES de las variables categóricas en español. También genera
#    race_long_es y life_long_es (columnas y valores traducidos). Las tres
#    se guardan como archivos .csv en la misma carpeta de la base original
#    (data/survey-data/). A partir de aquí, survey_es es la base que deben
#    usar TODOS los scripts siguientes del proyecto — survey/survey_enriched
#    quedan solo como referencia intacta del dato crudo, nunca para trabajar.
# 6) Retoma la generación de gráficos: 20 gráficos de barras, cada uno
#    exportado como PDF + SVG + PNG independientes a
#    outputs/output_code1/graficas_individuales/.
#
# TODA la salida de este script (CSV de exploración/verificación y gráficos)
# queda dentro de outputs/output_code1/ — cada script del proyecto tendrá su
# propia subcarpeta (code2.R -> outputs/output_code2/, etc.) para no mezclar
# resultados entre scripts.
#
# NOTA IMPORTANTE SOBRE ALCANCE DE LA TRADUCCIÓN:
# survey_enriched trae, además de las 18 variables demográficas/de uso que
# graficamos, otra batería de preguntas (q_control, q_altruism, q_bonus_05/
# 20/50, q_data_value_05/20/50/100/any(_1_text), q_sell_your_data,
# q_sell_consumer_data, q_small_biz_use, q_census_use, q_research_society,
# q_attn_check, showdata, incentive, connect) que NO se usan en ningún
# gráfico de este script y cuyo contenido exacto no conozco (parecen ser
# una batería sobre valoración de datos personales). Esas columnas SE
# CONSERVAN en survey_es con su nombre y valores originales en inglés, para
# no inventar una traducción sin conocer la pregunta real. Si quieres que
# también se traduzcan, dime qué mide cada una (o pégame sus valores únicos)
# y agrego esos diccionarios/renombres.
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Packages
#-----------------------------------------------------------------------------
packages <- unique(c(
  "tidyverse", "lubridate", "janitor", "skimr", "snakecase",
  "stringr", "dplyr", "gridExtra", "scales", "viridis",
  "patchwork", "fastDummies", "readxl", "openxlsx", "tidymodels",
  "Matrix", "caret", "pscl", "xgboost", "Metrics",
  "numDeriv", "optimParallel", "parallel", "furrr", "progressr",
  "tibble", "readr", "forcats", "ggplot2", "tidyr", "rlang",
  "showtext", "sysfonts", "svglite", "ragg", "purrr", "here"
))
load_pkg <- function(pkgs) {
  ok <- vapply(pkgs, function(p) {
    suppressPackageStartupMessages(require(p, character.only = TRUE))
  }, logical(1))
  if (any(!ok)) {
    stop("Packages not installed/loaded: ", paste(pkgs[!ok], collapse = ", "))
  }
  invisible(ok)
}
load_pkg(packages)
#-----------------------------------------------------------------------------
# Rutas del proyecto (independientes del working directory actual)
# `here::here()` ubica la RAÍZ del proyecto (donde está el .Rproj, o un
# archivo .here, o la carpeta .git) y arma la ruta desde ahí — así el script
# funciona igual sin importar si lo corres parado en R/, en la raíz, o desde
# otra máquina. Si tu proyecto NO tiene un .Rproj (o here() no encuentra la
# raíz correcta), crea un archivo vacío llamado ".here" en la carpeta que
# contiene "data/", "outputs/" y "fonts/" (la carpeta cod_grupo2, no R/).
#-----------------------------------------------------------------------------
ruta_survey_csv <- here::here("data", "survey-data", "survey.csv")
# Toda la salida de ESTE script (code1.R) vive en su propia subcarpeta
# dentro de outputs/, para no mezclarse con la salida de otros scripts del
# proyecto (code2.R tendría outputs/output_code2/, etc.).
ruta_outputs    <- here::here("outputs", "output_code1")
ruta_datos      <- here::here("data", "survey-data")
ruta_graficas   <- here::here("outputs", "output_code1", "graficas_individuales")
ruta_fuente     <- here::here("fonts", "times.ttf")
#-----------------------------------------------------------------------------
# Plotting setup (DEFINIR pal y fuente ANTES de bar_plot)
#-----------------------------------------------------------------------------
loc_ok <- base::Sys.setlocale("LC_ALL", "es_ES.UTF-8")
if (identical(loc_ok, "")) {
  warning(
    "Could not set locale 'es_ES.UTF-8'; ",
    "separators and month names will use the default locale."
  )
}
pal <- c(
  azul_1   = "#4E79A7",
  naranja  = "#F28E2B",
  rojo     = "#E15759",
  turquesa = "#76B7B2",
  verde    = "#59A14F",
  amarillo = "#EDC948",
  morado   = "#B07AA1",
  rosa     = "#FF9DA7",
  cafe     = "#9C755F",
  gris     = "#BAB0AC"
)
# Fuente: Times New Roman REAL, cargada desde un .ttf local del proyecto
# (reproducible: no depende de internet ni de que el sistema operativo la
# tenga instalada). Coloca el archivo en fonts/times.ttf dentro del
# repositorio (fonts/times.ttf, fonts/timesbd.ttf si quieres negrita, etc.).
# Si el archivo NO existe todavía (por ejemplo, recién clonaste el repo y no
# lo has copiado), el script cae de vuelta a "Tinos" (Google Fonts, métrica
# compatible con Times New Roman) y avisa con un warning — así el script no
# se rompe, pero el resultado no es la fuente real hasta que agregues el .ttf.
if (file.exists(ruta_fuente)) {
  sysfonts::font_add("times_new_roman", regular = ruta_fuente)
} else {
  warning(
    "No se encontró '", ruta_fuente, "'. Usando 'Tinos' (Google Fonts) como ",
    "sustituto temporal de Times New Roman. Para usar la fuente real y que ",
    "el proyecto sea reproducible sin internet, copia el archivo times.ttf ",
    "a la carpeta 'fonts/' del repositorio."
  )
  sysfonts::font_add_google("Tinos", "times_new_roman")
}
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)
#-----------------------------------------------------------------------------
# Functions
#-----------------------------------------------------------------------------
# Tema global estandarizado (fuente Times New Roman, tamaños grandes y
# nítidos para que se lean bien incluso reducidos a 0.8\textwidth en Overleaf)
tema_estandar <- ggplot2::theme_minimal(base_family = "times_new_roman", base_size = 15) +
  ggplot2::theme(
    plot.title          = ggplot2::element_text(size = 19, face = "bold", hjust = 0,
                                                 margin = ggplot2::margin(b = 10)),
    axis.title.x        = ggplot2::element_text(size = 15, margin = ggplot2::margin(t = 8)),
    axis.title.y        = ggplot2::element_text(size = 15, margin = ggplot2::margin(r = 8)),
    axis.text           = ggplot2::element_text(size = 13),
    plot.margin         = ggplot2::margin(t = 14, r = 24, b = 12, l = 12),
    panel.grid.major.y  = ggplot2::element_blank(),
    panel.grid.minor    = ggplot2::element_blank(),
    panel.grid.major.x  = ggplot2::element_line(color = "grey90", linewidth = 0.4)
  )
# Horizontal bar plot — cada BARRA dentro del gráfico usa un color distinto,
# tomado en orden de la paleta `pal` y reciclado si hay más categorías que
# colores. var_name se pasa como STRING (no como columna desnuda) para poder
# iterar sobre una lista de especificaciones más abajo.
bar_plot <- function(df, var_name, titulo, eje_x, fill_pal = pal,
                     text_size = 5) {

  var_sym <- rlang::sym(var_name)

  conteos <- df %>%
    dplyr::filter(!is.na(!!var_sym)) %>%
    dplyr::count(!!var_sym, name = "n") %>%
    dplyr::mutate(
      pct     = n / sum(n) * 100,
      var_chr = as.character(!!var_sym)
    ) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(var_chr = forcats::fct_reorder(var_chr, n))

  # Paleta reciclada: un color distinto por barra, estable entre ejecuciones
  fill_vals <- rep(unname(fill_pal), length.out = nrow(conteos))
  names(fill_vals) <- levels(conteos$var_chr)

  ggplot2::ggplot(conteos, ggplot2::aes(x = var_chr, y = n, fill = var_chr)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", pct), y = n),
      hjust  = -0.1, size = text_size, colour = "grey20",
      family = "times_new_roman"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::labs(title = titulo, x = eje_x, y = "Número de participantes") +
    tema_estandar
}
#-----------------------------------------------------------------------------
# Helper functions
#-----------------------------------------------------------------------------
mode_safe <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
reorder_drop_id <- function(df, ref_ids) {
  df %>%
    dplyr::filter(response_id %in% ref_ids) %>%
    dplyr::slice(match(ref_ids, response_id))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
# Explora tipo de dato + todos los valores únicos de un conjunto de variables
# de un data frame. Devuelve una tabla (una fila por variable) pensada para
# revisar a mano ANTES de decidir cómo traducir cada una.
explorar_variables <- function(df, vars) {
  purrr::map_dfr(vars, function(v) {
    x <- df[[v]]
    valores <- sort(unique(stringr::str_squish(as.character(stats::na.omit(x)))))
    tibble::tibble(
      variable         = v,
      tipo             = class(x)[1],
      n_valores_unicos = length(valores),
      valores_unicos   = paste(valores, collapse = " | ")
    )
  })
}
# Traduce un vector simple (un valor por celda) con un diccionario nombrado
# c("valor original" = "valor en español"). Recorta espacios sobrantes antes
# de comparar. Cualquier valor que NO esté en el diccionario se conserva tal
# cual (no se pierde información ni se generan NA por descuido).
traducir <- function(x, dic) {
  x_limpio <- stringr::str_squish(x)
  dplyr::recode(x_limpio, !!!dic, .default = x_limpio)
}
# Traduce una columna de RESPUESTA MÚLTIPLE combinada en una sola celda
# (ej. "White or Caucasian, Asian") separando por coma, traduciendo cada
# parte con el diccionario y volviendo a unir con ", ".
traducir_combo <- function(x, dic) {
  vapply(x, function(s) {
    if (is.na(s) || s == "") return(NA_character_)
    partes    <- stringr::str_split(stringr::str_squish(s), ",\\s*")[[1]]
    partes_es <- ifelse(partes %in% names(dic), unname(dic[partes]), partes)
    paste(partes_es, collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
}
#-----------------------------------------------------------------------------
# Load Survey Data
#-----------------------------------------------------------------------------
survey <- readr::read_csv(
  ruta_survey_csv,
  show_col_types = FALSE
) %>%
  janitor::clean_names()
#-----------------------------------------------------------------------------
# Process Race Question (multi-response)
#-----------------------------------------------------------------------------
race_levels <- c(
  "White or Caucasian",
  "Black or African American",
  "Asian",
  "American Indian/Native American or Alaska Native",
  "Native Hawaiian or Other Pacific Islander",
  "Other"
)
race_long <- survey %>%
  dplyr::select(response_id, race_raw = q_demos_race) %>%
  dplyr::filter(!is.na(race_raw) & race_raw != "") %>%
  dplyr::mutate(
    race_raw = race_raw %>%
      stringr::str_replace_all("\\s*/\\s*", "/") %>%
      stringr::str_replace_all("\\s*,\\s*", ", ") %>%
      stringr::str_squish()
  ) %>%
  tidyr::separate_rows(race_raw, sep = ",\\s*") %>%
  dplyr::mutate(race_raw = stringr::str_trim(race_raw))
race_dummies <- race_long %>%
  dplyr::mutate(
    race  = factor(race_raw, levels = race_levels),
    value = 1L
  ) %>%
  dplyr::select(-race_raw) %>%
  tidyr::pivot_wider(
    names_from   = race,
    values_from  = value,
    names_prefix = "race_",
    values_fill  = 0L
  ) %>%
  janitor::clean_names()
needed_race <- paste0("race_", janitor::make_clean_names(race_levels))
race_dummies[setdiff(needed_race, names(race_dummies))] <- 0L
#-----------------------------------------------------------------------------
# Process Life Changes Question (multi-response)
#-----------------------------------------------------------------------------
life_levels <- c(
  "Lost a job",
  "Moved place of residence",
  "Divorce",
  "Had a child",
  "Became pregnant"
)
life_long <- survey %>%
  dplyr::select(response_id, life_raw = q_life_changes) %>%
  dplyr::filter(!is.na(life_raw) & life_raw != "") %>%
  dplyr::mutate(
    life_raw = life_raw %>%
      stringr::str_replace_all("\\s*/\\s*", "/") %>%
      stringr::str_replace_all("\\s*,\\s*", ", ") %>%
      stringr::str_squish()
  ) %>%
  tidyr::separate_rows(life_raw, sep = ",\\s*") %>%
  dplyr::mutate(life_raw = stringr::str_trim(life_raw))
life_dummies <- life_long %>%
  dplyr::mutate(
    life  = factor(life_raw, levels = life_levels),
    value = 1L
  ) %>%
  dplyr::select(-life_raw) %>%
  tidyr::pivot_wider(
    names_from   = life,
    values_from  = value,
    names_prefix = "life_",
    values_fill  = 0L
  ) %>%
  janitor::clean_names()
needed_life <- paste0("life_", janitor::make_clean_names(life_levels))
life_dummies[setdiff(needed_life, names(life_dummies))] <- 0L
#-----------------------------------------------------------------------------
# Merge Enriched Variables into Survey
#-----------------------------------------------------------------------------
survey       <- survey       %>% dplyr::distinct(response_id, .keep_all = TRUE)
race_dummies <- race_dummies %>% dplyr::distinct(response_id, .keep_all = TRUE)
life_dummies <- life_dummies %>% dplyr::distinct(response_id, .keep_all = TRUE)
survey_enriched <- survey %>%
  dplyr::left_join(race_dummies, by = "response_id", relationship = "one-to-one") %>%
  dplyr::left_join(life_dummies, by = "response_id", relationship = "one-to-one")
colnames(survey_enriched)
#=============================================================================
# EXPLORACIÓN DE VARIABLES (correr esto ANTES de tocar los diccionarios)
# Para cada variable usada en los gráficos: tipo de dato + TODOS sus valores
# únicos (recortando espacios), sobre las bases ORIGINALES sin tocar. Se
# imprime en consola y además se exporta a outputs/exploracion_variables.csv.
#=============================================================================
vars_survey_a_explorar <- c(
  "q_prolific_mturk", "q_demos_age", "q_demos_hispanic", "q_demos_race",
  "q_demos_education", "q_demos_gender", "q_sexual_orientation",
  "q_demos_state", "q_amazon_use_howmany", "q_amazon_use_hh_size",
  "q_amazon_use_how_oft", "q_substance_use_1", "q_substance_use_2",
  "q_substance_use_3", "q_personal_1", "q_personal_2", "q_life_changes",
  "q_demos_income"
)
resumen_survey <- explorar_variables(survey, vars_survey_a_explorar)
resumen_race   <- explorar_variables(race_long, "race_raw")
resumen_life   <- explorar_variables(life_long, "life_raw")
resumen_variables <- dplyr::bind_rows(resumen_survey, resumen_race, resumen_life)

print(resumen_variables, n = Inf, width = Inf)

dir.create(ruta_outputs, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(resumen_variables, file.path(ruta_outputs, "exploracion_variables.csv"))
message(
  "\n>>> Revisa 'outputs/exploracion_variables.csv' (tipo + valores únicos ",
  "reales de cada variable) antes de ajustar los diccionarios de traducción ",
  "de abajo. <<<\n"
)
#-----------------------------------------------------------------------------
# Diccionarios de VALORES (inglés -> español)
# Verificados contra outputs/exploracion_variables.csv real. Los de raza y
# cambios de vida son exactos porque vienen literalmente de race_levels/
# life_levels definidos arriba. Cualquier valor que NO aparezca en un
# diccionario se conserva tal cual (en inglés) en survey_es — así que un
# gráfico "a medias traducido" es la señal de que falta agregar esa entrada.
#-----------------------------------------------------------------------------
dic_raza <- setNames(
  c("Blanco o caucásico", "Negro o afroamericano", "Asiático",
    "Indígena americano/nativo americano o nativo de Alaska",
    "Nativo de Hawái o de las islas del Pacífico", "Otro"),
  race_levels
)
dic_cambios_vida <- setNames(
  c("Perdió el empleo", "Cambió de lugar de residencia", "Divorcio",
    "Tuvo un hijo/a", "Quedó embarazada"),
  life_levels
)
# Sí/No simple (q_prolific_mturk, q_demos_hispanic)
dic_si_no <- c("Yes" = "Sí", "No" = "No")
# Sí/No + "Prefer not to say" + "I stopped..."; se usa para
# q_substance_use_1/2/3 y q_personal_1/2 — la categoría que no exista en
# cada variable simplemente no hace match y no genera error.
dic_uso_sustancias <- c(
  "Yes" = "Sí",
  "No" = "No",
  "Prefer not to say" = "Prefiero no decir",
  "I stopped in the recent past" = "Dejé de consumir recientemente"
)
dic_genero <- c(
  "Female" = "Femenino", "Male" = "Masculino",
  "Other" = "Otro", "Prefer not to say" = "Prefiero no decir"
)
dic_edad <- c(
  "18 - 24 years" = "18 - 24 años", "25 - 34 years" = "25 - 34 años",
  "35 - 44 years" = "35 - 44 años", "45 - 54 years" = "45 - 54 años",
  "55 - 64 years" = "55 - 64 años", "65 and older" = "65 años o más"
)
# Verificado: 5 categorías exactas (formato estándar tipo Pew Research)
dic_educacion <- c(
  "Less than high school degree" = "Menos que educación secundaria",
  "High school degree or equivalent (e.g. GED)" =
    "Bachillerato o equivalente (por ejemplo, GED)",
  "Some college, no degree" = "Estudios universitarios sin título",
  "Bachelor's degree" = "Título universitario (licenciatura)",
  "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS, etc)" =
    "Posgrado o título profesional (maestría, MBA, doctorado, JD, MD, DDS, etc.)"
)
# Verificado: 3 categorías exactas
dic_orientacion <- c(
  "heterosexual (straight)" = "Heterosexual",
  "LGBTQ+" = "LGBTQ+",
  "prefer not to say" = "Prefiero no decir"
)
# Verificado: 7 categorías exactas, con espacios alrededor del guion
dic_ingreso <- c(
  "Less than $25,000" = "Menos de $25,000",
  "$25,000 - $49,999" = "$25,000 - $49,999",
  "$50,000 - $74,999" = "$50,000 - $74,999",
  "$75,000 - $99,999" = "$75,000 - $99,999",
  "$100,000 - $149,999" = "$100,000 - $149,999",
  "$150,000 or more" = "$150,000 o más",
  "Prefer not to say" = "Prefiero no decir"
)
# Verificado: 3 categorías exactas (q_amazon_use_how_oft)
dic_frecuencia <- c(
  "Less than 5 times per month" = "Menos de 5 veces al mes",
  "5 - 10 times per month" = "5 - 10 veces al mes",
  "More than 10 times per month" = "Más de 10 veces al mes"
)
# Verificado: 4 categorías exactas (q_amazon_use_howmany y q_amazon_use_hh_size)
dic_conteo_hogar <- c(
  "1 (just me!)" = "1 (solo yo)",
  "2" = "2",
  "3" = "3",
  "4+" = "4+"
)
dic_estados <- c(
  "Alabama" = "Alabama", "Alaska" = "Alaska", "Arizona" = "Arizona",
  "Arkansas" = "Arkansas", "California" = "California", "Colorado" = "Colorado",
  "Connecticut" = "Connecticut", "Delaware" = "Delaware",
  "District of Columbia" = "Distrito de Columbia", "Florida" = "Florida",
  "Georgia" = "Georgia", "Hawaii" = "Hawái", "Idaho" = "Idaho",
  "Illinois" = "Illinois", "Indiana" = "Indiana", "Iowa" = "Iowa",
  "Kansas" = "Kansas", "Kentucky" = "Kentucky", "Louisiana" = "Luisiana",
  "Maine" = "Maine", "Maryland" = "Maryland", "Massachusetts" = "Massachusetts",
  "Michigan" = "Míchigan", "Minnesota" = "Minnesota", "Mississippi" = "Misisipi",
  "Missouri" = "Misuri", "Montana" = "Montana", "Nebraska" = "Nebraska",
  "Nevada" = "Nevada", "New Hampshire" = "Nuevo Hampshire",
  "New Jersey" = "Nueva Jersey", "New Mexico" = "Nuevo México",
  "New York" = "Nueva York", "North Carolina" = "Carolina del Norte",
  "North Dakota" = "Dakota del Norte", "Ohio" = "Ohio", "Oklahoma" = "Oklahoma",
  "Oregon" = "Oregón", "Pennsylvania" = "Pensilvania",
  "Rhode Island" = "Rhode Island", "South Carolina" = "Carolina del Sur",
  "South Dakota" = "Dakota del Sur", "Tennessee" = "Tennessee",
  "Texas" = "Texas", "Utah" = "Utah", "Vermont" = "Vermont",
  "Virginia" = "Virginia", "Washington" = "Washington",
  "West Virginia" = "Virginia Occidental", "Wisconsin" = "Wisconsin",
  "Wyoming" = "Wyoming", "Puerto Rico" = "Puerto Rico",
  "I did not reside in the United States" = "No residía en Estados Unidos"
)
#-----------------------------------------------------------------------------
# Diccionarios de NOMBRES de columna (inglés -> español)
# Formato: nombre_nuevo_en_espanol = "nombre_viejo_en_ingles", listo para
# usar con dplyr::rename(!!!diccionario). Cubren: columnas de identificación,
# las 18 variables graficadas, y las 11 variables dummy generadas por code1
# (race_*/life_*). El resto de columnas de survey_enriched (batería
# q_control/q_altruism/bonus/data_value/etc., ver nota al inicio del script)
# NO tiene diccionario todavía y se deja tal cual.
#-----------------------------------------------------------------------------
nombres_es_survey <- c(
  id_respuesta               = "response_id",
  duracion_segundos          = "duration_in_seconds",
  fecha_registro             = "recorded_date",
  plataforma_participacion   = "q_prolific_mturk",
  grupo_edad                 = "q_demos_age",
  origen_hispano             = "q_demos_hispanic",
  raza_combinada             = "q_demos_race",
  nivel_educativo            = "q_demos_education",
  rango_ingresos             = "q_demos_income",
  genero                     = "q_demos_gender",
  orientacion_sexual         = "q_sexual_orientation",
  estado                     = "q_demos_state",
  personas_comparten_cuenta  = "q_amazon_use_howmany",
  tamano_hogar               = "q_amazon_use_hh_size",
  frecuencia_compra_amazon   = "q_amazon_use_how_oft",
  consumo_cigarrillos        = "q_substance_use_1",
  consumo_marihuana          = "q_substance_use_2",
  consumo_alcohol            = "q_substance_use_3",
  diabetes_hogar             = "q_personal_1",
  silla_ruedas_hogar         = "q_personal_2",
  cambios_vida_combinado     = "q_life_changes"
)
nombres_es_race_dummies <- c(
  raza_negra_afroamericana             = "race_black_or_african_american",
  raza_blanca_caucasica                = "race_white_or_caucasian",
  raza_asiatica                        = "race_asian",
  raza_otra                            = "race_other",
  raza_indigena_americana_nativa_alaska = "race_american_indian_native_american_or_alaska_native",
  raza_nativa_hawaiana_pacifico         = "race_native_hawaiian_or_other_pacific_islander"
)
nombres_es_life_dummies <- c(
  cambio_perdida_empleo = "life_lost_a_job",
  cambio_mudanza         = "life_moved_place_of_residence",
  cambio_divorcio        = "life_divorce",
  cambio_tuvo_hijo       = "life_had_a_child",
  cambio_embarazo        = "life_became_pregnant"
)
#-----------------------------------------------------------------------------
# BASE OFICIAL DE TRABAJO: survey_es
# Es la versión ENRIQUECIDA (incluye las 11 dummies race_*/life_*) Y
# TRADUCIDA (nombres de columna Y valores en español) de survey_enriched.
# `survey`, `race_long`, `life_long`, `race_dummies`, `life_dummies` y
# `survey_enriched` (todo en inglés) NO se modifican y quedan solo como
# referencia del dato crudo — a partir de aquí, survey_es es la base que
# deben leer todos los scripts siguientes del proyecto.
#-----------------------------------------------------------------------------
survey_es <- survey_enriched %>%
  dplyr::mutate(
    q_prolific_mturk      = traducir(q_prolific_mturk,      dic_si_no),
    q_demos_age           = traducir(q_demos_age,           dic_edad),
    q_demos_hispanic      = traducir(q_demos_hispanic,      dic_si_no),
    q_demos_gender        = traducir(q_demos_gender,        dic_genero),
    q_demos_education     = traducir(q_demos_education,     dic_educacion),
    q_sexual_orientation  = traducir(q_sexual_orientation,  dic_orientacion),
    q_demos_state         = traducir(q_demos_state,         dic_estados),
    q_amazon_use_howmany  = traducir(q_amazon_use_howmany,  dic_conteo_hogar),
    q_amazon_use_hh_size  = traducir(q_amazon_use_hh_size,  dic_conteo_hogar),
    q_amazon_use_how_oft  = traducir(q_amazon_use_how_oft,  dic_frecuencia),
    q_substance_use_1     = traducir(q_substance_use_1,     dic_uso_sustancias),
    q_substance_use_2     = traducir(q_substance_use_2,     dic_uso_sustancias),
    q_substance_use_3     = traducir(q_substance_use_3,     dic_uso_sustancias),
    q_personal_1          = traducir(q_personal_1,          dic_uso_sustancias),
    q_personal_2          = traducir(q_personal_2,          dic_uso_sustancias),
    q_demos_income        = traducir(q_demos_income,        dic_ingreso),
    q_demos_race          = traducir_combo(q_demos_race,    dic_raza),
    q_life_changes        = traducir_combo(q_life_changes,  dic_cambios_vida)
  ) %>%
  dplyr::rename(!!!nombres_es_survey) %>%
  dplyr::rename(!!!nombres_es_race_dummies) %>%
  dplyr::rename(!!!nombres_es_life_dummies)

# Copias traducidas (columnas Y valores) de las tablas "long" derivadas
# de survey (una categoría por fila) — race_long/life_long en sí NO son
# "bases originales": son bases derivadas de survey, así que sus versiones
# _es también son derivadas, no una traducción de un archivo fuente.
race_long_es <- race_long %>%
  dplyr::mutate(race_raw = traducir(race_raw, dic_raza)) %>%
  dplyr::rename(id_respuesta = response_id, raza = race_raw)
life_long_es <- life_long %>%
  dplyr::mutate(life_raw = traducir(life_raw, dic_cambios_vida)) %>%
  dplyr::rename(id_respuesta = response_id, cambio_vida = life_raw)

# Verificación rápida: vuelve a listar tipo + valores únicos, pero ahora
# sobre las copias YA TRADUCIDAS (con sus nombres de columna en español).
# Si algún valor sigue en inglés aquí, es la señal exacta de qué falta
# agregar al diccionario correspondiente.
vars_survey_es_a_explorar <- names(nombres_es_survey)[
  match(vars_survey_a_explorar, nombres_es_survey)
]
verificacion_traduccion <- dplyr::bind_rows(
  explorar_variables(survey_es, vars_survey_es_a_explorar) %>% dplyr::mutate(base = "survey_es"),
  explorar_variables(race_long_es, "raza")                 %>% dplyr::mutate(base = "race_long_es"),
  explorar_variables(life_long_es, "cambio_vida")          %>% dplyr::mutate(base = "life_long_es")
)
readr::write_csv(verificacion_traduccion, file.path(ruta_outputs, "verificacion_traduccion.csv"))
message(
  "Verificación de traducción guardada en 'outputs/verificacion_traduccion.csv' — ",
  "revísala para confirmar que no queden valores en inglés."
)

# Guardar las copias traducidas como archivos, EN LA MISMA RUTA donde está
# el archivo original (data/survey-data/). survey_es.csv es a partir de
# ahora la base que deben leer los siguientes scripts del proyecto.
dir.create(ruta_datos, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(survey_es,    file.path(ruta_datos, "survey_es.csv"))
readr::write_csv(race_long_es, file.path(ruta_datos, "race_long_es.csv"))
readr::write_csv(life_long_es, file.path(ruta_datos, "life_long_es.csv"))
message(
  "Copias traducidas guardadas en: ", ruta_datos,
  " (survey_es.csv, race_long_es.csv, life_long_es.csv)"
)
#-----------------------------------------------------------------------------
# Bar Plots — especificaciones (todas apuntan a survey_es / race_long_es /
# life_long_es, con nombres de columna ya en español)
#-----------------------------------------------------------------------------
plot_specs <- list(
  list(df = survey_es,    var = "plataforma_participacion", titulo = "Participación en Amazon MTurk",
       eje_x = "Respuesta",                                   nombre = "participacion_amazon_mturk"),
  list(df = survey_es,    var = "grupo_edad",                titulo = "Distribución por grupo de edad",
       eje_x = "Grupo de edad",                                nombre = "distribucion_grupo_edad"),
  list(df = survey_es,    var = "origen_hispano",             titulo = "Origen hispano/latino",
       eje_x = "Respuesta",                                   nombre = "origen_hispano_latino"),
  list(df = survey_es,    var = "raza_combinada",             titulo = "Combinaciones de raza declaradas",
       eje_x = "Combinación", text_size = 4,                  nombre = "combinaciones_raza_declaradas"),
  list(df = race_long_es, var = "raza",                       titulo = "Participantes por raza",
       eje_x = "Raza",                                        nombre = "participantes_por_raza"),
  list(df = survey_es,    var = "nivel_educativo",            titulo = "Nivel educativo",
       eje_x = "Nivel educativo", text_size = 4,               nombre = "nivel_educativo"),
  list(df = survey_es,    var = "genero",                     titulo = "Identidad de género declarada",
       eje_x = "Género",                                       nombre = "identidad_genero_declarada"),
  list(df = survey_es,    var = "orientacion_sexual",         titulo = "Orientación sexual declarada",
       eje_x = "Orientación",                                  nombre = "orientacion_sexual_declarada"),
  list(df = survey_es,    var = "estado",                     titulo = "Distribución por estado",
       eje_x = "Estado", text_size = 3.5, alto = 9,            nombre = "distribucion_por_estado"),
  list(df = survey_es,    var = "personas_comparten_cuenta",  titulo = "Personas que comparten la cuenta de Amazon",
       eje_x = "Número de personas",                           nombre = "personas_comparten_cuenta_amazon"),
  list(df = survey_es,    var = "tamano_hogar",                titulo = "Tamaño del hogar",
       eje_x = "Personas en el hogar",                         nombre = "tamano_hogar"),
  list(df = survey_es,    var = "frecuencia_compra_amazon",   titulo = "Frecuencia de pedidos en Amazon",
       eje_x = "Frecuencia",                                   nombre = "frecuencia_pedidos_amazon"),
  list(df = survey_es,    var = "consumo_cigarrillos",        titulo = "Consumo de cigarrillos en el hogar",
       eje_x = "Respuesta",                                    nombre = "consumo_cigarrillos_hogar"),
  list(df = survey_es,    var = "consumo_marihuana",          titulo = "Consumo de marihuana en el hogar",
       eje_x = "Respuesta",                                    nombre = "consumo_marihuana_hogar"),
  list(df = survey_es,    var = "consumo_alcohol",            titulo = "Consumo de alcohol en el hogar",
       eje_x = "Respuesta",                                    nombre = "consumo_alcohol_hogar"),
  list(df = survey_es,    var = "diabetes_hogar",             titulo = "Diabetes en el hogar",
       eje_x = "Respuesta",                                    nombre = "diabetes_hogar"),
  list(df = survey_es,    var = "silla_ruedas_hogar",         titulo = "Uso de silla de ruedas en el hogar",
       eje_x = "Respuesta",                                    nombre = "uso_silla_ruedas_hogar"),
  list(df = survey_es,    var = "cambios_vida_combinado",     titulo = "Cambios importantes de vida en 2021",
       eje_x = "Combinación", text_size = 4,                  nombre = "cambios_vida_2021_combinaciones"),
  list(df = life_long_es, var = "cambio_vida",                titulo = "Cambios importantes de vida en 2021",
       eje_x = "Cambio de vida",                               nombre = "cambios_vida_2021_individual"),
  list(df = survey_es,    var = "rango_ingresos",             titulo = "Rango de ingresos",
       eje_x = "Respuesta",                                    nombre = "rango_ingresos")
)
#-----------------------------------------------------------------------------
# Generar y exportar los 20 gráficos
# Cada gráfico se guarda como TRES archivos independientes (PDF, SVG y PNG)
# en outputs/output_code1/graficas_individuales/, nombrados con número de
# orden + nombre descriptivo (ej. grafica_01_participacion_amazon_mturk.pdf/.svg/.png).
#-----------------------------------------------------------------------------
output_dir <- ruta_graficas
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

plots_survey <- vector("list", length(plot_specs))

for (i in seq_along(plot_specs)) {
  spec <- plot_specs[[i]]

  p <- bar_plot(
    df        = spec$df,
    var_name  = spec$var,
    titulo    = spec$titulo,
    eje_x     = spec$eje_x,
    text_size = spec$text_size %||% 5
  )
  plots_survey[[i]] <- p

  ancho <- spec$ancho %||% 10
  alto  <- spec$alto  %||% 6
  base_name <- sprintf("grafica_%02d_%s", i, spec$nombre)

  # PDF vectorial — se usa cairo_pdf (en vez de pdf()) para que showtext
  # incruste correctamente la fuente Times New Roman/Tinos.
  grDevices::cairo_pdf(
    filename = file.path(output_dir, paste0(base_name, ".pdf")),
    width = ancho, height = alto
  )
  print(p)
  grDevices::dev.off()

  # SVG vectorial
  svglite::svglite(
    filename = file.path(output_dir, paste0(base_name, ".svg")),
    width = ancho, height = alto
  )
  print(p)
  grDevices::dev.off()

  # PNG raster de alta resolución (ragg da mejor antialiasing/nitidez que
  # el png() de base, especialmente para texto)
  ragg::agg_png(
    filename = file.path(output_dir, paste0(base_name, ".png")),
    width = ancho, height = alto, units = "in", res = 300
  )
  print(p)
  grDevices::dev.off()

  message("Guardado: ", base_name, " (.pdf, .svg y .png)")
}

message(
  "Los ", length(plot_specs), " gráficos fueron exportados en PDF, SVG y PNG a: ",
  output_dir
)
