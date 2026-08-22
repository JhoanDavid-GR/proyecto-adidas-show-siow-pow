#-----------------------------------------------------------------------------
# code1.R
# Este script hace básicamente 4 cosas:
# Carga librerías
# Define funciones (especialmente para gráficos)
# Limpia y transforma datos de encuesta
# Genera y guarda gráficos descriptivos — cada uno como PDF y SVG independientes
#
# code1.R — Carga, limpieza y visualización descriptiva de datos de encuesta.
# Configura el entorno (locale es_ES.UTF-8, paleta de colores personalizada
# y fuente Tinos —sustituto de Times New Roman— para texto nítido en PDF/SVG).
# Define bar_plot(), función reutilizable de barras horizontales con
# porcentajes y un color sólido por gráfico, y dos helpers: mode_safe() para
# moda robusta ignorando NAs, y reorder_drop_id() para filtrar y reordenar
# filas por vector de IDs.
# Lee survey.csv y limpia nombres con janitor. Procesa dos preguntas de
# respuesta múltiple (raza y cambios de vida en 2021) separando respuestas
# concatenadas en filas, creando dummies 0/1 por categoría y garantizando
# que todas las columnas esperadas existan aunque nadie haya seleccionado
# esa opción. Une las dummies al survey base (one-to-one por response_id)
# generando survey_enriched, dataset híbrido listo para modelado. Genera 20
# gráficos de barras a partir de una lista de especificaciones y exporta
# cada uno como PDF (vector, cairo) y SVG independientes a
# outputs/figures/graficas_individuales/.
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Packages
# Configura el entorno gráfico antes de crear cualquier visualización.
# Tiene tres partes:
# Configuración regional (locale)
# Definición de paleta de colores personalizada
# Registro de fuente Tinos (sustituto métrico de Times New Roman)
#-----------------------------------------------------------------------------
packages <- unique(c(
  "tidyverse", "lubridate", "janitor", "skimr", "snakecase",
  "stringr", "dplyr", "gridExtra", "scales", "viridis",
  "patchwork", "fastDummies", "readxl", "openxlsx", "tidymodels",
  "Matrix", "caret", "pscl", "xgboost", "Metrics",
  "numDeriv", "optimParallel", "parallel", "furrr", "progressr",
  "tibble", "readr", "forcats", "ggplot2", "tidyr", "rlang",
  "showtext", "svglite"
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
# Fuente: Tinos es el reemplazo de Google Fonts métricamente compatible con
# Times New Roman (mismo espaciado/anchos), lo que garantiza texto nítido y
# consistente tanto en el PDF (vía cairo) como en el SVG, sin depender de que
# la fuente propietaria "Times New Roman" esté instalada en el sistema.
# Si tienes la fuente Times New Roman real instalada localmente y prefieres
# usarla en vez de Tinos, reemplaza la línea de abajo por:
#   sysfonts::font_add("times_new_roman", regular = "ruta/a/times.ttf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)
sysfonts::font_add_google("Tinos", "times_new_roman")
#-----------------------------------------------------------------------------
# Functions
# Este bloque define funciones reutilizables.
# En términos profesionales: aquí se encapsula lógica repetitiva para que el
# código sea:
# ✔ Modular
# ✔ Limpio
# ✔ Reproducible
# ✔ Escalable
#-----------------------------------------------------------------------------
# Tema global estandarizado (fuente Times/Tinos en todos los gráficos)
tema_estandar <- ggplot2::theme_minimal(base_family = "times_new_roman", base_size = 12) +
  ggplot2::theme(
    plot.title          = ggplot2::element_text(size = 13, face = "bold", hjust = 0,
                                                 margin = ggplot2::margin(b = 8)),
    axis.title.x        = ggplot2::element_text(size = 11, margin = ggplot2::margin(t = 6)),
    axis.title.y        = ggplot2::element_text(size = 11, margin = ggplot2::margin(r = 6)),
    axis.text           = ggplot2::element_text(size = 10),
    plot.margin         = ggplot2::margin(t = 12, r = 20, b = 10, l = 10),
    panel.grid.major.y  = ggplot2::element_blank(),
    panel.grid.minor    = ggplot2::element_blank(),
    panel.grid.major.x  = ggplot2::element_line(color = "grey90", linewidth = 0.4)
  )
# Horizontal bar plot (robusto, un color sólido por gráfico, fuente Times/Tinos)
# var_name se pasa como STRING (no como columna desnuda) para poder iterar
# sobre una lista de especificaciones más abajo.
bar_plot <- function(df, var_name, titulo, eje_x, color_barra,
                     text_size = 3.2) {

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

  ggplot2::ggplot(conteos, ggplot2::aes(x = var_chr, y = n)) +
    ggplot2::geom_col(fill = color_barra, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", pct), y = n),
      hjust  = -0.1, size = text_size, colour = "grey25",
      family = "times_new_roman"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(title = titulo, x = eje_x, y = "Número de participantes") +
    tema_estandar
}
#-----------------------------------------------------------------------------
# Helper functions
# Este bloque define funciones auxiliares que apoyan el procesamiento de datos.
# No generan gráficos, no leen archivos — ayudan a manipular datos de forma
# robusta. En este script hay dos helper functions:
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
#-----------------------------------------------------------------------------
# Load Survey Data
# Este bloque:
# Lee el archivo principal de datos
# Lo guarda en un objeto llamado survey
# Limpia los nombres de las columnas
# Es el punto donde el proyecto comienza a trabajar con datos reales.
#-----------------------------------------------------------------------------
survey <- readr::read_csv(
  "data/survey-data/survey.csv",
  show_col_types = FALSE
) %>%
  janitor::clean_names()
#-----------------------------------------------------------------------------
# Process Race Question (multi-response)
# este es uno de los bloques MÁS importantes del script desde el punto de
# vista de feature engineering. Este bloque transforma una pregunta de
# encuesta con respuesta múltiple en una sola celda en variables dummy
# listas para modelado.
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
# Este bloque es estructuralmente idéntico al de Race, pero aplicado a otra
# pregunta de la encuesta: Cambios importantes de vida en 2021.
# Desde el punto de vista técnico, aquí se repite el mismo patrón de feature
# engineering para variables multirespuesta.
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
# Bar Plots — especificaciones
# En vez de 20 llamadas manuales repetidas (una para "ver en pantalla" y otra
# para exportar, como en versiones anteriores), se define UNA lista con los
# parámetros de cada gráfico y se recorre una sola vez. Esto evita cómputo
# duplicado y evita que el índice de color se reinicie a mitad de camino.
#-----------------------------------------------------------------------------
plot_specs <- list(
  list(df = survey,    var = "q_prolific_mturk",     titulo = "Participación en Amazon MTurk",
       eje_x = "Respuesta",                            nombre = "participacion_amazon_mturk"),
  list(df = survey,    var = "q_demos_age",           titulo = "Distribución por grupo de edad",
       eje_x = "Grupo de edad",                        nombre = "distribucion_grupo_edad"),
  list(df = survey,    var = "q_demos_hispanic",      titulo = "Origen hispano/latino",
       eje_x = "Respuesta",                            nombre = "origen_hispano_latino"),
  list(df = survey,    var = "q_demos_race",          titulo = "Combinaciones de raza declaradas",
       eje_x = "Combinación", text_size = 2.6,         nombre = "combinaciones_raza_declaradas"),
  list(df = race_long, var = "race_raw",              titulo = "Participantes por raza",
       eje_x = "Raza",                                 nombre = "participantes_por_raza"),
  list(df = survey,    var = "q_demos_education",     titulo = "Nivel educativo",
       eje_x = "Nivel educativo", text_size = 2.6,     nombre = "nivel_educativo"),
  list(df = survey,    var = "q_demos_gender",        titulo = "Identidad de género declarada",
       eje_x = "Género",                               nombre = "identidad_genero_declarada"),
  list(df = survey,    var = "q_sexual_orientation",  titulo = "Orientación sexual declarada",
       eje_x = "Orientación",                          nombre = "orientacion_sexual_declarada"),
  list(df = survey,    var = "q_demos_state",         titulo = "Distribución por estado",
       eje_x = "Estado", text_size = 2.2, alto = 9,    nombre = "distribucion_por_estado"),
  list(df = survey,    var = "q_amazon_use_howmany",  titulo = "Personas que comparten la cuenta de Amazon",
       eje_x = "Número de personas",                   nombre = "personas_comparten_cuenta_amazon"),
  list(df = survey,    var = "q_amazon_use_hh_size",  titulo = "Tamaño del hogar",
       eje_x = "Personas en el hogar",                 nombre = "tamano_hogar"),
  list(df = survey,    var = "q_amazon_use_how_oft",  titulo = "Frecuencia de pedidos en Amazon",
       eje_x = "Frecuencia",                           nombre = "frecuencia_pedidos_amazon"),
  list(df = survey,    var = "q_substance_use_1",     titulo = "Consumo de cigarrillos en el hogar",
       eje_x = "Respuesta",                            nombre = "consumo_cigarrillos_hogar"),
  list(df = survey,    var = "q_substance_use_2",     titulo = "Consumo de marihuana en el hogar",
       eje_x = "Respuesta",                            nombre = "consumo_marihuana_hogar"),
  list(df = survey,    var = "q_substance_use_3",     titulo = "Consumo de alcohol en el hogar",
       eje_x = "Respuesta",                            nombre = "consumo_alcohol_hogar"),
  list(df = survey,    var = "q_personal_1",          titulo = "Diabetes en el hogar",
       eje_x = "Respuesta",                            nombre = "diabetes_hogar"),
  list(df = survey,    var = "q_personal_2",          titulo = "Uso de silla de ruedas en el hogar",
       eje_x = "Respuesta",                            nombre = "uso_silla_ruedas_hogar"),
  list(df = survey,    var = "q_life_changes",        titulo = "Cambios importantes de vida en 2021",
       eje_x = "Combinación", text_size = 2.6,         nombre = "cambios_vida_2021_combinaciones"),
  list(df = life_long, var = "life_raw",              titulo = "Cambios importantes de vida en 2021",
       eje_x = "Cambio de vida",                       nombre = "cambios_vida_2021_individual"),
  list(df = survey,    var = "q_demos_income",        titulo = "Rango de ingresos",
       eje_x = "Respuesta",                            nombre = "rango_ingresos")
)
#-----------------------------------------------------------------------------
# Generar y exportar los 20 gráficos
# Cada gráfico se guarda como DOS archivos independientes (PDF y SVG) en
# outputs/figures/graficas_individuales/, nombrados con número de orden +
# nombre descriptivo (ej. grafica_01_participacion_amazon_mturk.pdf/.svg).
#-----------------------------------------------------------------------------
output_dir <- "outputs/figures/graficas_individuales"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

graph_colors <- rep(unname(pal), length.out = length(plot_specs))

plots_survey <- vector("list", length(plot_specs))

for (i in seq_along(plot_specs)) {
  spec <- plot_specs[[i]]

  p <- bar_plot(
    df          = spec$df,
    var_name    = spec$var,
    titulo      = spec$titulo,
    eje_x       = spec$eje_x,
    color_barra = graph_colors[i],
    text_size   = spec$text_size %||% 3.2
  )
  plots_survey[[i]] <- p

  ancho <- spec$ancho %||% 10
  alto  <- spec$alto  %||% 6
  base_name <- sprintf("grafica_%02d_%s", i, spec$nombre)

  # PDF vectorial — se usa cairo_pdf (en vez de pdf()) para que showtext
  # incruste correctamente la fuente Tinos/Times.
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

  message("Guardado: ", base_name, " (.pdf y .svg)")
}

message(
  "Los ", length(plot_specs), " gráficos fueron exportados en PDF y SVG a: ",
  output_dir
)
