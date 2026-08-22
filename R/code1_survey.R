#-----------------------------------------------------------------------------
# code1.R 
#Este script hace básicamente 4 cosas:

#Carga librerías

#Define funciones (especialmente para gráficos)

#Limpia y transforma datos de encuesta

#Genera y guarda gráficos descriptivos

# code1.R — Carga, limpieza y visualización descriptiva de datos de encuesta.
# Configura el entorno (locale es_ES.UTF-8 y paleta de colores personalizada).
# Define bar_plot(), función reutilizable de barras horizontales con porcentajes
# y paleta reciclada, y dos helpers: mode_safe() para moda robusta ignorando
# NAs, y reorder_drop_id() para filtrar y reordenar filas por vector de IDs.
# Lee survey.csv y limpia nombres con janitor. Procesa dos preguntas de
# respuesta múltiple (raza y cambios de vida en 2021) separando respuestas
# concatenadas en filas, creando dummies 0/1 por categoría y garantizando
# que todas las columnas esperadas existan aunque nadie haya seleccionado
# esa opción. Une las dummies al survey base (one-to-one por response_id)
# generando survey_enriched, dataset híbrido listo para modelado. Genera 20
# gráficos de barras individuales sobre variables demográficas y de uso de
# Amazon, los agrupa en una lista y los exporta todos a survey_barplots.pdf.

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Packages
#Configura el entorno gráfico antes de crear cualquier visualización.

#Tiene dos partes:

#Configuración regional (locale)

#Definición de paleta de colores personalizada
#-----------------------------------------------------------------------------

packages <- unique(c(
  "tidyverse", "lubridate", "janitor", "skimr", "snakecase",
  "stringr", "dplyr", "gridExtra", "scales", "viridis",
  "patchwork", "fastDummies", "readxl", "openxlsx", "tidymodels",
  "Matrix", "caret", "pscl", "xgboost", "Metrics",
  "numDeriv", "optimParallel", "parallel", "furrr", "progressr",
  "tibble", "readr", "forcats", "ggplot2", "tidyr"
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
# Plotting setup (DEFINIR pal ANTES de bar_plot)
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

#-----------------------------------------------------------------------------
# Functions
#Este bloque define funciones reutilizables.
#En términos profesionales: aquí se encapsula lógica repetitiva para que el código sea:
  
# ✔ Modular

# ✔ Limpio

# ✔ Reproducible

# ✔ Escalable
#-----------------------------------------------------------------------------

# Horizontal bar plot (robusto y consistente con paleta)
bar_plot <- function(df, var, title, x_lab,
                     fill_pal = pal, text_size = 3) {
  
  # Conteos (excluye NA)
  counts <- df %>%
    dplyr::filter(!is.na({{ var }})) %>%
    dplyr::count({{ var }}, name = "n") %>%
    dplyr::mutate(
      pct = n / sum(n) * 100,
      var_chr = as.character({{ var }})
    ) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(var_chr = forcats::fct_reorder(var_chr, n))
  
  # Paleta reciclada (estable)
  fill_vals <- rep(unname(fill_pal), length.out = nrow(counts))
  names(fill_vals) <- levels(counts$var_chr)
  
  ggplot2::ggplot(counts, ggplot2::aes(x = var_chr, y = n, fill = var_chr)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", pct), y = n),
      hjust = -0.1, size = text_size, colour = "black"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::labs(title = title, x = x_lab, y = "Number of participants") +
    ggplot2::theme_minimal()
}

#-----------------------------------------------------------------------------
# Helper functions

#Este bloque define funciones auxiliares que apoyan el procesamiento de datos.
#No generan gráficos, no leen archivos — ayudan a manipular datos de forma robusta.

#En este script hay dos helper functions:

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

#-----------------------------------------------------------------------------
# Load Survey Data

#Este bloque:
  
#  Lee el archivo principal de datos

#Lo guarda en un objeto llamado survey

#Limpia los nombres de las columnas

#Es el punto donde el proyecto comienza a trabajar con datos reales.

#-----------------------------------------------------------------------------

survey <- readr::read_csv(
  "data/survey-data/survey.csv",
  show_col_types = FALSE
) %>%
  janitor::clean_names()

#-----------------------------------------------------------------------------
# Process Race Question (multi-response)

# este es uno de los bloques MÁS importantes del script desde el punto de vista de feature engineering.
# Este bloque transforma una pregunta de encuesta con respuesta múltiple en una sola celda en variables dummy listas para modelado.
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

#Este bloque es estructuralmente idéntico al de Race, pero aplicado a otra pregunta de la encuesta:
  
#  Cambios importantes de vida en 2021

#Desde el punto de vista técnico, aquí se repite el mismo patrón de feature engineering para variables multirespuesta.
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

# Verificando que no haya duplicados por ID

# Uniéndolo todo en un solo dataset final

# Creando el dataset listo para modelado
#-----------------------------------------------------------------------------

survey       <- survey       %>% dplyr::distinct(response_id, .keep_all = TRUE)
race_dummies <- race_dummies %>% dplyr::distinct(response_id, .keep_all = TRUE)
life_dummies <- life_dummies %>% dplyr::distinct(response_id, .keep_all = TRUE)

survey_enriched <- survey %>%
  dplyr::left_join(race_dummies, by = "response_id", relationship = "one-to-one") %>%
  dplyr::left_join(life_dummies, by = "response_id", relationship = "one-to-one")

colnames(survey_enriched)

#-----------------------------------------------------------------------------
# Bar Plots (ejecución individual)

# script donde se generan y ejecutan los gráficos de barras de forma individual.
#“ejecución individual”?

#Significa que cada gráfico se crea y se imprime por separado, normalmente llamando a una función (por ejemplo bar_plot()) para una variable específica.

#En vez de usar un loop automático que genere todos los gráficos juntos, aquí se hacen llamadas explícitas como:


# Toma variables del dataset procesado (survey)

# Aplica la función de gráfico de barras

# Usa la paleta de colores definida anteriormente (pal)

# Muestra el gráfico en pantalla o lo guarda
#-----------------------------------------------------------------------------

bar_plot(survey, q_prolific_mturk, "Participation in Amazon MTurk", "Response")
bar_plot(survey, q_demos_age, "Age group distribution", "Age group")
bar_plot(survey, q_demos_hispanic, "Hispanic/Latino origin", "Response",
         fill_pal = pal[c("azul_1", "naranja")])
bar_plot(survey, q_demos_race, "Race combinations (declared)", "Combination",
         text_size = 2.8)
bar_plot(race_long, race_raw, "Participants by race (individual count)", "Race")
bar_plot(survey, q_demos_education, "Education level", "Education", text_size = 2.8)
bar_plot(survey, q_demos_gender, "Declared gender identity", "Gender")
bar_plot(survey, q_sexual_orientation, "Declared sexual orientation", "Orientation")
bar_plot(survey, q_demos_state, "State distribution", "State", text_size = 2.4)
bar_plot(survey, q_amazon_use_howmany, "People sharing Amazon account", "Number of people")
bar_plot(survey, q_amazon_use_hh_size, "Household size", "People in household")
bar_plot(survey, q_amazon_use_how_oft, "Amazon order frequency", "Frequency")
bar_plot(survey, q_substance_use_1, "Household cigarette use", "Response")
bar_plot(survey, q_substance_use_2, "Household marijuana use", "Response")
bar_plot(survey, q_substance_use_3, "Household alcohol use", "Response")
bar_plot(survey, q_personal_1, "Diabetes in household", "Response")
bar_plot(survey, q_personal_2, "Wheelchair use in household", "Response")
bar_plot(survey, q_life_changes, "Life changes in 2021 (combinations)", "Combination",
         text_size = 2.8)
bar_plot(life_long, life_raw, "Life changes in 2021 (individual)", "Life change")
bar_plot(survey, q_demos_income, "Income range", "Response")

#------------------------------------------------------------------------------
# Collect all survey plots

# marca la sección donde se agrupan o combinan todos los gráficos del survey en un solo objeto o layout.
# Guardan los plots en objetos

# Se combinan en una figura compuesta

# Se organizan en filas/columnas

# Opcionalmente, se exportan a un archivo (PDF, PNG, etc.)


#------------------------------------------------------------------------------

plots_survey <- list(
  bar_plot(survey, q_prolific_mturk, "Participation in Amazon MTurk", "Response"),
  bar_plot(survey, q_demos_age, "Age group distribution", "Age group"),
  bar_plot(survey, q_demos_hispanic, "Hispanic/Latino origin", "Response",
           fill_pal = pal[c("azul_1", "naranja")]),
  bar_plot(survey, q_demos_race, "Race combinations (declared)", "Combination",
           text_size = 2.8),
  bar_plot(race_long, race_raw, "Participants by race (individual count)", "Race"),
  bar_plot(survey, q_demos_education, "Education level", "Education", text_size = 2.8),
  bar_plot(survey, q_demos_gender, "Declared gender identity", "Gender"),
  bar_plot(survey, q_sexual_orientation, "Declared sexual orientation", "Orientation"),
  bar_plot(survey, q_demos_state, "State distribution", "State", text_size = 2.4),
  bar_plot(survey, q_amazon_use_howmany, "People sharing Amazon account", "Number of people"),
  bar_plot(survey, q_amazon_use_hh_size, "Household size", "People in household"),
  bar_plot(survey, q_amazon_use_how_oft, "Amazon order frequency", "Frequency"),
  bar_plot(survey, q_substance_use_1, "Household cigarette use", "Response"),
  bar_plot(survey, q_substance_use_2, "Household marijuana use", "Response"),
  bar_plot(survey, q_substance_use_3, "Household alcohol use", "Response"),
  bar_plot(survey, q_personal_1, "Diabetes in household", "Response"),
  bar_plot(survey, q_personal_2, "Wheelchair use in household", "Response"),
  bar_plot(survey, q_life_changes, "Life changes in 2021 (combinations)", "Combination",
           text_size = 2.8),
  bar_plot(life_long, life_raw, "Life changes in 2021 (individual)", "Life change"),
  bar_plot(survey, q_demos_income, "Income range", "Response")
)

#------------------------------------------------------------------------------
# Save all plots into a single PDF

# marca la sección donde todos los gráficos generados y/o combinados se exportan a un único archivo PDF.
# Después de:
  
#   Generar los gráficos individuales

# Agruparlos en una figura final

# Aquí el script:
  
#   Abre un dispositivo PDF

# Envía los gráficos al archivo

# Cierra el dispositivo para guardar correctamente

# Es el paso de exportación final.

#------------------------------------------------------------------------------

out_pdf <- "survey_barplots.pdf"
grDevices::pdf(out_pdf, width = 10, height = 7)
for (p in plots_survey) print(p)
grDevices::dev.off()
message("Saved: ", out_pdf)
