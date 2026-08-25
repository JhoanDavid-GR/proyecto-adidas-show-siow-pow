# =============================================================================
# code2.R — CONSTRUCCIÓN COMÚN DE V_raw / W_raw / Y_amount (ADIDAS)
#
# PROYECTO: Predicción SoW/SioW Adidas — Maestría
#
# ROL DE ESTE SCRIPT (arquitectura de dos niveles):
#   Script 2 (este archivo) = construcción "agnóstica de método de modelado"
#   del problema predictivo: carga, limpieza, ingeniería de variables,
#   partición TRAIN/TEST reproducible, y diagnóstico exploratorio (EDA).
#   NO aplica transformaciones específicas de un modelo (winsorización
#   destructiva, estandarización, selección de variables por Spearman,
#   discretización de Y). Esas piezas se documentan aquí como
#   "TRASLADADO A CODE3" y se implementarán en code3.R (preparación
#   específica para el modelo POGIT / conteo).
#
#   Y_amount es el monto real en dólares gastado en Adidas en T2
#   (NO la versión discretizada/conteo Y_count; esa construcción,
#   junto con c_train, queda para code3.R).
#
# BASE FUENTE: adaptado de Code2_survey_MODIFICADO.R (versión ya adaptada
# a Adidas). No quedan referencias a Apple ni a la marca tech original:
# todo el pipeline trabaja exclusivamente sobre Adidas.
#
# TRAZABILIDAD: cada bloque conserva su numeración original entre
# corchetes [BLOQUE ORIGINAL n] para poder comparar bloque por bloque
# contra Code2_survey_MODIFICADO.R. Todo cambio de fondo está marcado
# con el patrón:
#   # PROBLEMA DETECTADO -> EXPLICACIÓN -> PROPUESTA
# =============================================================================

# -----------------------------------------------------------------------------
# BLOQUE 1 — Carga de paquetes  [BLOQUE ORIGINAL 1 — MODIFICADO]
#
# PROBLEMA DETECTADO: el original no usaba here::here(), por lo que las
# rutas relativas ("data/survey-data/...") solo funcionan si R arranca
# exactamente en la carpeta raíz del proyecto. code1.R ya tuvo este mismo
# problema (error real reportado por el usuario).
# EXPLICACIÓN: si code2.R se ejecuta desde una subcarpeta (p. ej. "R/"),
# las rutas relativas fallan igual que en code1.R.
# PROPUESTA: usar el mismo mecanismo de here::here() de code1.R para
# todas las rutas de este script (lectura y escritura).
# -----------------------------------------------------------------------------

packages <- c(
  "dplyr","tidyr","stringr","tibble",
  "readr","janitor","lubridate",
  "ggplot2","scales","patchwork",
  "fastDummies","openxlsx","caret",
  "here","forcats","purrr"
)
invisible(lapply(packages, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  require(p, character.only = TRUE)
}))

# Dependencias para exportación de gráficas individuales PDF + SVG + PNG,
# igual que en code1.R (showtext/sysfonts para tipografía, svglite para
# SVG, ragg para PNG con buen anti-aliasing de texto).
for (p in c("showtext","sysfonts","svglite","ragg")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(showtext)
library(sysfonts)
library(svglite)

# -----------------------------------------------------------------------------
# Rutas reproducibles (here::here) — NUEVO, mismo mecanismo que code1.R
# -----------------------------------------------------------------------------

ruta_survey_es    <- here::here("data", "survey-data", "survey_es.csv")
ruta_purchases    <- here::here("data", "survey-data", "amazon-purchases.csv")
ruta_salidas      <- here::here("salidas", "codigo_de_salida2")
ruta_graficas     <- here::here("salidas", "codigo_de_salida2", "graficas_individuales")
ruta_fuente       <- here::here("fonts", "times.ttf")

dir.create(ruta_salidas,  showWarnings = FALSE, recursive = TRUE)
dir.create(ruta_graficas, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Tipografía Times New Roman real — MISMO MECANISMO que code1.R
# Busca fonts/times.ttf; si no existe, reporta el problema con warning()
# explícito y usa Tinos (Google Fonts) como sustituto temporal, en vez de
# sustituir la fuente en silencio.
# -----------------------------------------------------------------------------

if (file.exists(ruta_fuente)) {
  sysfonts::font_add("times_new_roman", regular = ruta_fuente)
} else {
  warning(
    "No se encontro '", ruta_fuente, "'. Usando 'Tinos' (Google Fonts) como ",
    "sustituto temporal de Times New Roman en las graficas de code2.R. ",
    "Para usar la tipografia real, coloca el archivo times.ttf en la ",
    "carpeta 'fonts/' en la raiz del proyecto."
  )
  sysfonts::font_add_google("Tinos", "times_new_roman")
}
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# -----------------------------------------------------------------------------
# Tema y paleta estándar de gráficas (mismo estilo/tamaños que code1.R:
# titulo 19, ejes 15, texto general 13). Sustituye al tema "poster" de
# 44-54pt del script original (BLOQUE 22 original), que se consideraba
# ilegible fuera de una presentación en pantalla completa.
# -----------------------------------------------------------------------------

pal <- c(
  "#4E79A7","#E15759","#59A14F","#F28E2B","#76B7B2",
  "#B07AA1","#EDC948","#FF9DA7","#9C755F","#BAB0AC"
)

tema_grafica <- ggplot2::theme_minimal(base_family = "times_new_roman", base_size = 13) +
  ggplot2::theme(
    plot.title         = ggplot2::element_text(size = 19, face = "bold", color = "#000000", hjust = 0),
    plot.subtitle      = ggplot2::element_text(size = 15, color = "#1a1a1a"),
    axis.title         = ggplot2::element_text(size = 15, face = "bold", color = "#1f1f1f"),
    axis.text          = ggplot2::element_text(size = 13, color = "#2e2e2e"),
    legend.text        = ggplot2::element_text(size = 13, color = "#1a1a1a"),
    legend.title       = ggplot2::element_text(size = 15, face = "bold", color = "#000000"),
    strip.text         = ggplot2::element_text(size = 13, face = "bold", color = "#1a1a1a"),
    panel.grid.minor   = ggplot2::element_blank(),
    plot.background    = ggplot2::element_rect(fill = "white", color = NA),
    panel.background   = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin        = ggplot2::margin(t = 14, r = 20, b = 14, l = 14)
  )

# Helper de exportación: cada gráfica se guarda como PDF + SVG + PNG
# independientes, igual que en code1.R (no un PDF combinado).
exportar_grafica <- function(plot_obj, nombre_archivo, ancho = 10, alto = 7) {
  ruta_pdf <- file.path(ruta_graficas, paste0(nombre_archivo, ".pdf"))
  ruta_svg <- file.path(ruta_graficas, paste0(nombre_archivo, ".svg"))
  ruta_png <- file.path(ruta_graficas, paste0(nombre_archivo, ".png"))

  grDevices::cairo_pdf(filename = ruta_pdf, width = ancho, height = alto)
  showtext::showtext_begin(); print(plot_obj); showtext::showtext_end()
  grDevices::dev.off()

  svglite::svglite(filename = ruta_svg, width = ancho, height = alto)
  showtext::showtext_begin(); print(plot_obj); showtext::showtext_end()
  grDevices::dev.off()

  ragg::agg_png(filename = ruta_png, width = ancho, height = alto, units = "in", res = 300)
  showtext::showtext_begin(); print(plot_obj); showtext::showtext_end()
  grDevices::dev.off()

  message("Guardado: ", nombre_archivo, " (PDF + SVG + PNG)")
}

# -----------------------------------------------------------------------------
# Formato numérico es-CO y tablas de métricas como gráfica — NUEVO, a
# petición del usuario: las tablas compactas de indicadores/pruebas (VIF,
# diagnóstico de outliers, resumen de variables) se exportan ADEMÁS del CSV
# como imagen PDF+SVG+PNG con la MISMA tipografía y tamaños que las
# gráficas (Times New Roman, título 19, encabezado ~15, celdas ~13), en
# estilo "booktabs" (solo líneas horizontales), lista para pegar en el
# documento de tesis. Números en formato es-CO ("." miles, "," decimales),
# igual que en el resto de las gráficas de code1.R/code2.R.
# -----------------------------------------------------------------------------

fmt_miles_es <- scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
fmt_dec_es   <- function(x, accuracy = 0.1) scales::label_number(accuracy = accuracy, decimal.mark = ",", big.mark = ".")(x)
fmt_pct_es   <- function(x, accuracy = 0.1) paste0(fmt_dec_es(x, accuracy), "%")

# Dibuja un data frame (columnas ya formateadas como texto, ver fmt_*_es)
# como tabla tipo booktabs y la exporta con exportar_grafica(). No formatea
# números por sí misma: los valores deben llegar ya listos como texto.
graficar_tabla_metrica <- function(df, titulo, subtitulo = NULL, nombre_archivo,
                                   alineacion = NULL, ancho = 9, alto = NULL) {
  stopifnot(is.data.frame(df), ncol(df) >= 1, nrow(df) >= 1)
  n_col <- ncol(df)
  n_fil <- nrow(df)
  if (is.null(alineacion)) alineacion <- c("left", rep("center", n_col - 1))
  stopifnot(length(alineacion) == n_col)
  if (is.null(alto)) alto <- 1.7 + 0.55 * (n_fil + 1)

  encabezados <- names(df)
  y_header <- n_fil + 1

  df_idx <- df
  names(df_idx) <- as.character(seq_len(n_col))
  df_idx$fila <- seq_len(n_fil)

  celdas <- tidyr::pivot_longer(
    df_idx, cols = as.character(seq_len(n_col)),
    names_to = "col", values_to = "texto"
  ) %>%
    dplyr::mutate(
      col = as.integer(col), y = n_fil + 1 - fila,
      texto = as.character(texto), negrita = FALSE
    ) %>%
    dplyr::select(col, y, texto, negrita)

  encab_df <- tibble::tibble(
    col = seq_len(n_col), y = y_header, texto = encabezados, negrita = TRUE
  )

  datos_tabla <- dplyr::bind_rows(encab_df, celdas) %>%
    dplyr::mutate(
      hjust_x = dplyr::if_else(alineacion[col] == "left", 0, 0.5),
      x       = col + dplyr::if_else(alineacion[col] == "left", -0.42, 0)
    )

  y_toprule <- y_header + 0.65
  y_midrule <- y_header - 0.5
  y_botrule <- 0.5

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0.55, xend = n_col + 0.45, y = y_toprule, yend = y_toprule),
      linewidth = 0.9, colour = "#000000"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0.55, xend = n_col + 0.45, y = y_midrule, yend = y_midrule),
      linewidth = 0.6, colour = "#000000"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0.55, xend = n_col + 0.45, y = y_botrule, yend = y_botrule),
      linewidth = 0.9, colour = "#000000"
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(datos_tabla, negrita),
      ggplot2::aes(x = x, y = y, label = texto, hjust = hjust_x),
      family = "times_new_roman", fontface = "bold", size = 5.2, colour = "#000000"
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(datos_tabla, !negrita),
      ggplot2::aes(x = x, y = y, label = texto, hjust = hjust_x),
      family = "times_new_roman", size = 4.6, colour = "#1a1a1a"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0.3, n_col + 0.7), ylim = c(y_botrule - 0.5, y_toprule + 0.5), clip = "off"
    ) +
    ggplot2::labs(title = titulo, subtitle = subtitulo) +
    tema_grafica +
    ggplot2::theme(
      axis.text  = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 14, r = 24, b = 14, l = 24)
    )

  exportar_grafica(p, nombre_archivo, ancho = ancho, alto = alto)
  invisible(p)
}

# -----------------------------------------------------------------------------
# BLOQUE 2 — Helpers de winsorización  [BLOQUE ORIGINAL 2 — CONSERVADO]
#
# Se conservan las tres funciones tal cual (misma lógica). En este script
# se usan ÚNICAMENTE con fines de DIAGNÓSTICO (reportar el cap de Tukey y
# el % de observaciones que quedarían por encima), NUNCA para modificar
# V_raw / W_raw / Y_amount. La winsorización APLICADA a los datos se
# traslada a code3.R (ver diagnostico_outliers.csv más abajo).
# -----------------------------------------------------------------------------

tukey_cap_upper <- function(x, k = 3) {
  x_num <- suppressWarnings(as.numeric(x))
  if (all(is.na(x_num))) return(list(x = x_num, cap = NA_real_, q3 = NA_real_, iqr = NA_real_))
  q   <- stats::quantile(x_num, probs = c(0.25, 0.75), na.rm = TRUE, type = 7)
  iqr <- as.numeric(q[2] - q[1])
  q3  <- as.numeric(q[2])
  cap <- q3 + k * iqr
  x_w <- x_num
  x_w[!is.na(x_w) & x_w > cap] <- cap
  list(x = x_w, cap = cap, q3 = q3, iqr = iqr)
}

apply_upper_cap <- function(x, cap) {
  x_num <- suppressWarnings(as.numeric(x))
  if (!is.finite(cap)) return(x_num)
  x_num[!is.na(x_num) & x_num > cap] <- cap
  x_num
}

winsorize_df_numeric_upper_trainonly <- function(df_train, df_test,
                                                 id_col = "response_id",
                                                 k = 3, cols = NULL) {
  stopifnot(id_col %in% names(df_train), id_col %in% names(df_test))
  if (is.null(cols)) {
    cols <- names(df_train)[sapply(df_train, is.numeric)]
    cols <- setdiff(cols, id_col)
  } else {
    cols <- intersect(cols, names(df_train))
    cols <- setdiff(cols, id_col)
  }
  out_tr <- df_train; out_te <- df_test; caps <- list()
  for (cc in cols) {
    tk <- tukey_cap_upper(out_tr[[cc]], k = k)
    caps[[cc]] <- tk$cap
    out_tr[[cc]] <- tk$x
    out_te[[cc]] <- apply_upper_cap(out_te[[cc]], tk$cap)
  }
  list(train = out_tr, test = out_te, caps = caps)
}

# -----------------------------------------------------------------------------
# BLOQUE 3 — Helper de alineación de IDs  [BLOQUE ORIGINAL 3 — CONSERVADO]
# Sin cambios respecto al original. Utilidad general, se conserva como tal.
# -----------------------------------------------------------------------------

align_master_ids <- function(V, W, var_2023, id_col = "response_id",
                             master = c("W","V","var")) {
  master <- match.arg(master)
  V        <- V        %>% distinct(.data[[id_col]], .keep_all = TRUE) %>% mutate(!!id_col := as.character(.data[[id_col]]))
  W        <- W        %>% distinct(.data[[id_col]], .keep_all = TRUE) %>% mutate(!!id_col := as.character(.data[[id_col]]))
  var_2023 <- var_2023 %>% distinct(.data[[id_col]], .keep_all = TRUE) %>% mutate(!!id_col := as.character(.data[[id_col]]))
  ids_common <- Reduce(intersect, list(V[[id_col]], W[[id_col]], var_2023[[id_col]]))
  stopifnot(length(ids_common) > 0)
  ids_master <- switch(master,
                       W   = W[[id_col]][W[[id_col]] %in% ids_common],
                       V   = V[[id_col]][V[[id_col]] %in% ids_common],
                       var = var_2023[[id_col]][var_2023[[id_col]] %in% ids_common]
  )
  V_a   <- V[match(ids_master, V[[id_col]]), , drop = FALSE]
  W_a   <- W[match(ids_master, W[[id_col]]), , drop = FALSE]
  var_a <- var_2023[match(ids_master, var_2023[[id_col]]), , drop = FALSE]
  stopifnot(identical(V_a[[id_col]], W_a[[id_col]]))
  stopifnot(identical(V_a[[id_col]], var_a[[id_col]]))
  list(V = V_a, W = W_a, var_2023 = var_a, ids = ids_master)
}

# -----------------------------------------------------------------------------
# BLOQUE 4 — Carga de datos de compras Amazon  [BLOQUE ORIGINAL 4 — MODIFICADO]
# PROBLEMA DETECTADO -> EXPLICACIÓN -> PROPUESTA: ver BLOQUE 1 (here::here()).
# El resto de la lógica (limpieza de nombres, detección de columna de ID,
# renombrado a response_id, tipos, amount = precio * cantidad) es idéntica.
# -----------------------------------------------------------------------------

purchases <- readr::read_csv(ruta_purchases, show_col_types = FALSE) |>
  janitor::clean_names()

resp_col <- names(purchases)[stringr::str_detect(names(purchases), "response")]
stopifnot(length(resp_col) == 1)

purchases0 <- purchases %>%
  dplyr::rename(response_id = !!resp_col) %>%
  dplyr::mutate(
    response_id = as.character(response_id),
    order_date  = lubridate::as_date(order_date),
    dplyr::across(c(purchase_price_per_unit, quantity), as.numeric),
    amount      = purchase_price_per_unit * quantity
  )

# -----------------------------------------------------------------------------
# BLOQUE 5 — Clasificación en macro-categorías  [BLOQUE ORIGINAL 5 — CONSERVADO]
# Sin cambios. Se necesita para todas las categorías de compra (no solo Adidas).
# -----------------------------------------------------------------------------

electronics_keywords <- c(
  "electronics?","computer","laptop","notebook","desktop",
  "tablet","smartphone","\\bphone\\b","\\btv\\b","television",
  "monitor","projector","printer","scanner","camera","webcam",
  "speaker","audio","soundbar","headphones?","headset","earbuds?",
  "microphone","charger","charging","battery","power( bank| supply)?",
  "adapter","cable","dock(ing)?( station)?","\\bmouse\\b","keyboard",
  "trackpad","controller","stylus","\\bssd\\b","(hard ?drive|hdd)",
  "(flash ?drive|thumb ?drive)","\\b(memory|ram)\\b","\\bgpu\\b",
  "graphics card","\\busb\\b","bluetooth","hdmi","\\bethernet\\b",
  "\\bwi[- ]?fi\\b|wifi","smartwatch","wearable","router","modem",
  "mesh( wifi)?","toner","\\bink\\b","(vr headset|virtual reality)"
)
electronics_pattern <- paste(electronics_keywords, collapse = "|")

classify_category <- function(cat) {
  dplyr::case_when(
    stringr::str_detect(cat, "(?i)book|ebook|dvd|blu[-_ ]?ray|video_games?")          ~ "Media",
    stringr::str_detect(cat, "(?i)apparel|clothing|hosiery|bra|coat|shoe|fashion")    ~ "Apparel",
    stringr::str_detect(cat, "(?i)beauty|cosmetic|hair_|skin|lotion|fragrance")       ~ "Beauty_PersonalCare",
    stringr::str_detect(cat, "(?i)grocery|food|snack|beverage|coffee|tea|candy")      ~ "Grocery",
    stringr::str_detect(cat, "(?i)kitchen|cook|bake|dinnerware|utensil|cookware")     ~ "Kitchen_Dining",
    stringr::str_detect(cat, "(?i)home_|furniture|decor|bedding|lamp|curtain|rug")    ~ "Home_Living",
    stringr::str_detect(cat, stringr::regex(electronics_pattern, ignore_case = TRUE)) ~ "Electronics",
    stringr::str_detect(cat, "(?i)sport|fitness|exercise|camping|cycling|golf|ball")  ~ "Sports_Outdoors",
    stringr::str_detect(cat, "(?i)toy|game|lego|puzzle|doll|costume")                 ~ "Toys_Games",
    stringr::str_detect(cat, "(?i)pet_|animal")                                       ~ "Pet_Supplies",
    stringr::str_detect(cat, "(?i)baby_")                                             ~ "Baby",
    stringr::str_detect(cat, "(?i)auto|car|vehicle|engine|tire|battery")              ~ "Automotive",
    TRUE                                                                               ~ "Other"
  )
}

purchases_cat <- purchases0 %>%
  dplyr::mutate(
    broad_cat = classify_category(category) |> factor(),
    title_lc  = stringr::str_squish(stringr::str_to_lower(dplyr::coalesce(title, "")))
  )

# -----------------------------------------------------------------------------
# BLOQUE 6 — Detección de marca Adidas  [BLOQUE ORIGINAL 6 — CONSERVADO]
# Sin cambios. Detección de "adidas" en el título vía regex.
# -----------------------------------------------------------------------------

ADIDAS_PATTERN <- "\\badidas\\b"

purchases_cat <- purchases_cat %>%
  dplyr::mutate(
    is_adidas = stringr::str_detect(
      title_lc,
      stringr::regex(ADIDAS_PATTERN, ignore_case = TRUE)
    )
  )

stopifnot(!any(is.na(purchases_cat$is_adidas)))

purchases_adidas <- purchases_cat %>%
  dplyr::filter(is_adidas) %>%
  dplyr::mutate(
    response_id = as.character(response_id),
    unit_price  = purchase_price_per_unit,
    total_price = amount
  )

# -----------------------------------------------------------------------------
# BLOQUE 7 — Sub-macro-categorías Adidas  [BLOQUE ORIGINAL 7 — CONSERVADO]
# Sin cambios.
# -----------------------------------------------------------------------------

footwear_adidas_pattern    <- "SHOES|SANDAL|\\bBOOT\\b|TECHNICAL_SPORT_SHOE|SLIPPER"
bottoms_adidas_pattern     <- "PANTS|SHORTS|TIGHTS|LEOTARD|TRACK_SUIT|SKIRT|OVERALLS"
tops_adidas_pattern        <- "\\bSHIRT\\b|SWEATSHIRT|\\bCOAT\\b|SWEATER|TUNIC|OUTERWEAR"
underwear_adidas_pattern   <- "UNDERPANTS|\\bBRA\\b|\\bUNDERWEAR\\b|\\bSOCKS\\b"
accessories_adidas_pattern <- "BACKPACK|DUFFEL_BAG|WAIST_PACK|\\bHAT\\b|\\bBAG\\b|HANDBAG|SCARF|APPAREL_HEAD"
sportsequip_adidas_pattern <- "RECREATION_BALL|SHIN_GUARD|SPORT_ACTIVITY_GLOVE|SPORT_EQUIPMENT_BAG_CASE|KNEE_PAD|SWEATBAND"

classify_adidas_macro <- function(cat) {
  dplyr::case_when(
    stringr::str_detect(cat, stringr::regex(footwear_adidas_pattern,    ignore_case = TRUE)) ~ "Footwear",
    stringr::str_detect(cat, stringr::regex(bottoms_adidas_pattern,     ignore_case = TRUE)) ~ "Bottoms",
    stringr::str_detect(cat, stringr::regex(tops_adidas_pattern,        ignore_case = TRUE)) ~ "Tops",
    stringr::str_detect(cat, stringr::regex(underwear_adidas_pattern,   ignore_case = TRUE)) ~ "Underwear_Socks",
    stringr::str_detect(cat, stringr::regex(accessories_adidas_pattern, ignore_case = TRUE)) ~ "Accessories",
    stringr::str_detect(cat, stringr::regex(sportsequip_adidas_pattern, ignore_case = TRUE)) ~ "Sports_Equip",
    TRUE                                                                                      ~ "Other_Adidas"
  )
}

purchases_adidas <- purchases_adidas %>%
  dplyr::mutate(adidas_macro = classify_adidas_macro(category) |> factor())

# -----------------------------------------------------------------------------
# BLOQUE 8 — Ventanas temporales T1 y T2  [BLOQUE ORIGINAL 8 — CONSERVADO]
# Sin cambios.
# -----------------------------------------------------------------------------

ref_date1a <- lubridate::ymd("2018-01-01")
ref_date2a <- lubridate::ymd("2024-12-31")

date_range <- purchases_adidas %>%
  dplyr::filter(
    is_adidas == TRUE,
    dplyr::between(order_date, ref_date1a, ref_date2a),
    total_price > 0
  ) %>%
  dplyr::summarise(
    min_date = min(order_date, na.rm = TRUE),
    max_date = max(order_date, na.rm = TRUE),
    .groups  = "drop"
  )

min_date <- date_range$min_date[[1]]
max_date <- date_range$max_date[[1]]
stopifnot(!is.na(min_date), !is.na(max_date))

ref_date1 <- min_date
ref_date2 <- lubridate::ymd("2021-10-31")   # fin T1
ref_date3 <- lubridate::ymd("2021-11-01")   # inicio T2
ref_date4 <- max_date

has_order_id <- "order_id" %in% names(purchases_adidas)

# -----------------------------------------------------------------------------
# BLOQUE 9 — RFM de Adidas en T1  [BLOQUE ORIGINAL 9 — CONSERVADO]
# Sin cambios.
# -----------------------------------------------------------------------------

rfm_raw_2018 <- purchases_adidas %>%
  dplyr::filter(
    is_adidas == TRUE,
    dplyr::between(order_date, ref_date1, ref_date2),
    total_price > 0
  ) %>%
  dplyr::group_by(response_id) %>%
  dplyr::summarise(
    recency_days_2018_Adidas = as.numeric(ref_date2 - max(order_date, na.rm = TRUE)),
    frequency_2018_Adidas    = if (has_order_id) dplyr::n_distinct(order_id) else dplyr::n(),
    monetary_2018_Adidas     = sum(total_price, na.rm = TRUE),
    .groups = "drop"
  )

adidas_flags <- purchases_adidas %>%
  dplyr::filter(
    is_adidas == TRUE,
    dplyr::between(order_date, ref_date1, ref_date2)
  ) %>%
  dplyr::select(response_id, adidas_macro) %>%
  dplyr::distinct() %>%
  dplyr::mutate(flag = 1L) %>%
  tidyr::pivot_wider(
    names_from   = adidas_macro,
    values_from  = flag,
    values_fill  = list(flag = 0L),
    names_prefix = "2018_Adidas_macro_",
    names_expand = TRUE
  ) %>%
  dplyr::right_join(dplyr::select(rfm_raw_2018, response_id), by = "response_id") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("2018_Adidas_macro_"),
                              \(x) dplyr::coalesce(x, 0L)))

# -----------------------------------------------------------------------------
# BLOQUE 10 — Y cruda: amount_2023_Adidas  [BLOQUE ORIGINAL 10 — CONSERVADO]
# Esta ES la fuente de Y_amount (monto real en dólares). Aún no se
# winsoriza ni se discretiza: eso pertenece al modelo específico (code3.R).
#
# DISCREPANCIA DE CATEGORÍA DETECTADA (Y_amount vs. SioW — solo se
# documenta aquí, NO se modifica nada todavía; ver también BLOQUE 23):
# Y_amount (amount_2023_Adidas) se calcula sobre `purchases_adidas`
# filtrado ÚNICAMENTE por `is_adidas == TRUE` y la ventana T2 — es decir,
# TODO el gasto en productos Adidas en T2, sin restringir por
# `broad_cat`. `purchases_adidas` no filtra por categoría amplia en
# ningún punto previo.
# Más abajo (BLOQUE 23), `amount_adidas` que alimenta SioW_amount/SoW/
# PoW_amount se calcula sobre `period_df`, que SÍ restringe a
# `broad_cat %in% c("Apparel", "Sports_Outdoors")`. Como `broad_cat` sale
# de `classify_category()` (clasificador GENÉRICO aplicado a la columna
# `category` de Amazon, no específico de Adidas), es posible que exista
# gasto en productos Adidas cuyo `category` no caiga en Apparel ni en
# Sports_Outdoors (p. ej. si Amazon etiquetó el producto en otra
# categoría del catálogo). Ese gasto SÍ cuenta en Y_amount pero NO
# cuenta en amount_adidas/SioW_amount/SoW.
# CONSECUENCIA: Y_amount (universo = todo Adidas en T2) y amount_adidas
# dentro de SioW (universo = Adidas en T2 ∩ {Apparel, Sports_Outdoors})
# no están garantizados a ser iguales para el mismo response_id, aunque
# ambos midan "gasto en Adidas". Esta discrepancia YA EXISTÍA en el
# script original (Code2_survey_MODIFICADO.R) de forma idéntica; no es
# introducida por esta reorganización.
# DECISIÓN PENDIENTE DEL USUARIO: definir si la categoría de análisis
# para SioW/SoW/PoW debe (a) ampliarse para incluir todo `broad_cat`
# donde haya gasto Adidas detectado (alineando con Y_amount), o (b)
# mantenerse en Apparel + Sports_Outdoors y, en ese caso, redefinir
# Y_amount para que use el mismo universo de categoría que SioW. No se
# aplica ningún cambio hasta que se indique cuál opción corresponde.
# -----------------------------------------------------------------------------

var_2023 <- purchases_adidas %>%
  dplyr::filter(
    is_adidas == TRUE,
    dplyr::between(order_date, ref_date3, ref_date4)
  ) %>%
  dplyr::group_by(response_id) %>%
  dplyr::summarise(
    amount_2023_Adidas = sum(total_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(amount_2023_Adidas = tidyr::replace_na(amount_2023_Adidas, 0))

# -----------------------------------------------------------------------------
# BLOQUE 11 — Identificación de montos por encima de SEVERE_THRESHOLD
# [BLOQUE ORIGINAL 11 — MODIFICADO: de ELIMINACIÓN a DIAGNÓSTICO]
#
# PROBLEMA DETECTADO: el bloque original ELIMINA del universo de datos
# (var_2023, purchases_adidas, purchases_cat, purchases0, y más abajo
# también W_pre y V_pre) a todo response_id con amount_2023_Adidas >
# 5000, antes del split TRAIN/TEST.
# EXPLICACIÓN: eliminar filas es una transformación irreversible de los
# datos, no un diagnóstico. Este script (code2.R) debe limitarse a
# CONSTRUIR y OBSERVAR V_raw/W_raw/Y_amount, dejando cualquier decisión
# de tratamiento de casos extremos (eliminar, capar, ponderar, o no
# hacer nada) para una etapa posterior explícita y deliberada. No hay
# evidencia en este script de que estos casos sean errores de captura;
# esa era una interpretación que no corresponde afirmar aquí sin
# evidencia adicional (p. ej. revisión manual de los registros).
# PROPUESTA: se conserva la IDENTIFICACIÓN de los response_id con
# amount_2023_Adidas > SEVERE_THRESHOLD, y se EXPORTA a un CSV de
# diagnóstico para que puedan revisarse manualmente. NINGÚN dato se
# elimina en code2.R: var_2023, purchases_adidas, purchases_cat y
# purchases0 conservan TODOS los registros. Los filtros correspondientes
# que existían más abajo sobre W_pre (bloque 16) y V_pre (bloque 17)
# también se retiraron por la misma razón (ver notas en esos bloques).
# -----------------------------------------------------------------------------

SEVERE_THRESHOLD <- 5000

diag_montos_severos <- var_2023 %>%
  dplyr::filter(amount_2023_Adidas > SEVERE_THRESHOLD) %>%
  dplyr::select(response_id, amount_2023_Adidas) %>%
  dplyr::arrange(dplyr::desc(amount_2023_Adidas))

message("Montos > ", SEVERE_THRESHOLD, " identificados para revisión (NO eliminados de Code2): ",
        nrow(diag_montos_severos))

readr::write_csv(diag_montos_severos, file.path(ruta_salidas, "diagnostico_montos_severos.csv"))

# NOTA: anomalous_ids_severe se conserva como vector (vacío de efecto en
# los datos) únicamente porque BLOQUE 16 y BLOQUE 17 hacían referencia a
# él; se documenta ahí mismo que ya no filtra nada.
anomalous_ids_severe <- character(0)

var_2023 <- var_2023 %>% dplyr::semi_join(rfm_raw_2018, by = "response_id")

# -----------------------------------------------------------------------------
# BLOQUE 12 — Merge RFM + Y  [BLOQUE ORIGINAL 12 — CONSERVADO]
# Sin cambios.
# -----------------------------------------------------------------------------

rfm_amount <- rfm_raw_2018 %>%
  dplyr::left_join(var_2023, by = "response_id") %>%
  tidyr::replace_na(list(amount_2023_Adidas = 0))

# -----------------------------------------------------------------------------
# BLOQUE 13 — Features multimarca y comportamiento en T2  [BLOQUE ORIGINAL 13 —
# CONSERVADO]
# Sin cambios: diccionario de 17 marcas deportivas competidoras,
# broadcat_summary y state_summ.
# -----------------------------------------------------------------------------

sport_brand_dict <- tibble::tibble(
  pattern = c(
    "\\badidas\\b","\\bnike\\b","\\bpuma\\b",
    "under armour|\\bua\\b","new balance","\\breebok\\b",
    "\\bchampion\\b","\\bfila\\b","\\bconverse\\b",
    "\\bvans\\b","\\bskechers\\b","\\bcolumbia\\b",
    "\\bnorth face\\b|the north face","\\bpatagonia\\b",
    "\\blululemon\\b","\\bnewline\\b","\\bhummel\\b"
  ),
  brand = c(
    "Adidas","Nike","Puma","UnderArmour","NewBalance","Reebok",
    "Champion","Fila","Converse","Vans","Skechers","Columbia",
    "NorthFace","Patagonia","Lululemon","Newline","Hummel"
  )
)

extract_sport_brand <- function(title_lc) {
  vapply(title_lc, function(t) {
    idx <- which(stringr::str_detect(
      t, stringr::regex(sport_brand_dict$pattern, ignore_case = TRUE)
    ))
    if (length(idx) == 0) NA_character_ else sport_brand_dict$brand[idx[1]]
  }, character(1))
}

purchases_sport_period <- purchases_cat %>%
  dplyr::filter(
    dplyr::between(order_date, ref_date3, ref_date4),
    broad_cat %in% c("Apparel", "Sports_Outdoors")
  ) %>%
  dplyr::mutate(
    sport_brand = extract_sport_brand(title_lc),
    sport_brand = dplyr::if_else(is.na(sport_brand), "Generic", sport_brand)
  )

brand_count <- purchases_sport_period %>%
  dplyr::filter(sport_brand != "Generic") %>%
  dplyr::group_by(response_id) %>%
  dplyr::summarise(
    `2023_n_sport_brand` = dplyr::n_distinct(sport_brand),
    .groups = "drop"
  )

brand_items <- purchases_sport_period %>%
  dplyr::filter(sport_brand != "Generic") %>%
  dplyr::group_by(response_id, sport_brand) %>%
  dplyr::summarise(n_items = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from   = sport_brand,
    values_from  = n_items,
    values_fill  = list(n_items = 0),
    names_prefix = "2023_sportbrand_"
  )

rfm_amount_2018_2023 <- rfm_amount %>%
  dplyr::left_join(adidas_flags, by = "response_id") %>%
  dplyr::mutate(dplyr::across(starts_with("2018_Adidas_macro_"),
                              ~ tidyr::replace_na(.x, 0)))

rfm_amount_2018_2023b <- rfm_amount_2018_2023 %>%
  dplyr::left_join(brand_count, by = "response_id") %>%
  dplyr::left_join(brand_items, by = "response_id") %>%
  dplyr::mutate(
    `2023_n_sport_brand` = dplyr::coalesce(`2023_n_sport_brand`, 0),
    dplyr::across(starts_with("2023_sportbrand_"), ~ dplyr::coalesce(.x, 0))
  )

broadcat_summary <- purchases_cat %>%
  dplyr::filter(dplyr::between(order_date, ref_date1, ref_date2)) %>%
  dplyr::group_by(response_id, broad_cat) %>%
  dplyr::summarise(
    units_cat  = sum(quantity, na.rm = TRUE),
    amount_cat = sum(amount,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = broad_cat,
    values_from = c(units_cat, amount_cat),
    values_fill = list(units_cat = 0, amount_cat = 0),
    names_glue  = "2018_{.value}_{broad_cat}"
  )

rfm_amount_2018_2023b <- rfm_amount_2018_2023b %>%
  dplyr::left_join(broadcat_summary, by = "response_id") %>%
  dplyr::mutate(
    dplyr::across(starts_with("2018_units_cat_"),  ~ dplyr::coalesce(.x, 0)),
    dplyr::across(starts_with("2018_amount_cat_"), ~ dplyr::coalesce(.x, 0))
  )

mode_safe2 <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

state_summ <- purchases0 %>%
  dplyr::filter(dplyr::between(order_date, ref_date1, ref_date2)) %>%
  dplyr::mutate(
    shipping_address_state = shipping_address_state %>%
      stringr::str_trim() %>% stringr::str_to_upper(),
    shipping_address_state = dplyr::if_else(
      is.na(shipping_address_state) | shipping_address_state == "",
      "DIGITAL/LOCKER", shipping_address_state
    )
  ) %>%
  dplyr::group_by(response_id) %>%
  dplyr::summarise(
    n_states  = dplyr::n_distinct(shipping_address_state[shipping_address_state != "DIGITAL/LOCKER"]),
    top_state = mode_safe2(shipping_address_state),
    .groups   = "drop"
  )

rfm_amount_2018_2023b <- rfm_amount_2018_2023b %>%
  dplyr::left_join(state_summ, by = "response_id")

# -----------------------------------------------------------------------------
# BLOQUE 14 — Merge con survey_es  [BLOQUE ORIGINAL 14 — MODIFICADO]
#
# PROBLEMA DETECTADO (1): el original asumía que `survey_enriched` ya
# existía en el entorno de R (por haber corrido code1.R antes en la misma
# sesión). Esto no es reproducible: si se ejecuta code2.R en una sesión
# nueva, o en otra máquina, el script falla.
# EXPLICACIÓN: para que code2.R sea reproducible de forma independiente,
# debe leer la base desde disco.
# PROPUESTA: leer `survey_es.csv` (la base oficial ENRIQUECIDA y
# TRADUCIDA que produce code1.R) directamente desde
# data/survey-data/survey_es.csv vía here::here(). Se usa survey_es
# (no `survey`/`survey_enriched`, que quedan como referencia cruda en
# inglés y no deben usarse para más trabajo, según lo acordado).
#
# PROBLEMA DETECTADO (2): en code1.R, la columna de ID fue renombrada de
# `response_id` a `id_respuesta` como parte de la traducción de nombres
# de columnas de survey_es. Todo el pipeline de compras de este script
# (purchases, RFM, W_pre, V_pre, etc.) usa `response_id` de forma extensa.
# EXPLICACIÓN: reescribir todo el pipeline de compras a `id_respuesta`
# sería un cambio invasivo y de alto riesgo de introducir errores, sin
# beneficio real (es solo un identificador técnico interno).
# PROPUESTA: renombrar `id_respuesta` -> `response_id` inmediatamente
# después de leer survey_es, y dejarlo así documentado. `response_id` se
# trata como nombre técnico interno (permitido en inglés según lo
# indicado por el usuario para nombres técnicos).
#
# PROBLEMA DETECTADO (3): `cols_drop` en el original usa nombres de
# columna en inglés (duration_in_seconds, q_prolific_mturk, q_demos_race,
# q_life_changes, q_demos_state, recorded_date). Como code1.R ya tradujo
# los nombres de esas columnas concretas al español (duracion_segundos,
# plataforma_participacion, raza_combinada, cambios_vida_combinado,
# estado, fecha_registro), usar los nombres viejos haría que
# dplyr::select(-all_of(cols_drop)) fallara (columna inexistente).
# Las ~17 columnas de la "batería adicional" (q_control, q_altruism,
# q_bonus_*, q_data_value_*, q_sell_*, q_small_biz_use, q_census_use,
# q_research_society, q_attn_check, showdata, incentive, connect) NO
# fueron traducidas en code1.R (quedaron pendientes por falta de
# confirmación de significado) y por lo tanto conservan su nombre en
# inglés en survey_es.
# PROPUESTA: actualizar cols_drop con los nombres realmente presentes en
# survey_es (mezcla de español para las columnas ya traducidas + inglés
# para la batería adicional aún pendiente).
# -----------------------------------------------------------------------------

stopifnot(file.exists(ruta_survey_es))

survey_es <- readr::read_csv(ruta_survey_es, show_col_types = FALSE)

stopifnot("id_respuesta" %in% names(survey_es))
survey_es <- survey_es %>%
  dplyr::rename(response_id = id_respuesta) %>%
  dplyr::mutate(response_id = as.character(response_id))

# survey_enriched (nombre usado en el resto del pipeline heredado de la
# versión Adidas original) apunta ahora a la base oficial traducida.
survey_enriched <- survey_es

cols_drop <- c(
  "duracion_segundos","plataforma_participacion","q_control","q_altruism",
  "q_bonus_05","q_bonus_20","q_bonus_50",
  "q_data_value_100","q_data_value_any","q_data_value_any_1_text",
  "q_sell_your_data","q_sell_consumer_data","q_small_biz_use","q_census_use",
  "q_research_society","q_attn_check","showdata","incentive","connect",
  "raza_combinada","cambios_vida_combinado","estado","fecha_registro"
)
cols_drop <- intersect(cols_drop, names(survey_enriched))

valid_ids <- rfm_amount_2018_2023b %>%
  dplyr::select(response_id) %>%
  dplyr::distinct()

survey_enriched2 <- survey_enriched %>%
  dplyr::distinct(response_id, .keep_all = TRUE) %>%
  dplyr::semi_join(valid_ids, by = "response_id") %>%
  dplyr::left_join(rfm_amount_2018_2023b, by = "response_id",
                   relationship = "one-to-one") %>%
  dplyr::select(-dplyr::all_of(cols_drop))

# -----------------------------------------------------------------------------
# BLOQUE NUEVO — Missingness ORIGINAL de las variables fuente de V/W
# (AGREGADO, se calcula ANTES de dummy_cols()/replace_na())
#
# PROBLEMA DETECTADO: el diagnóstico de missingness que ya existía en
# este script (más abajo, después del split) se calculaba sobre
# V_train/W_train, que para ese punto YA habían pasado por
# fastDummies::dummy_cols(ignore_na = TRUE) y por
# dplyr::mutate(across(where(is.numeric), ~ replace_na(.x, 0))) en el
# BLOQUE 15. Es decir, medía missingness DESPUÉS de que los NA ya habían
# sido tratados (convertidos a 0 o absorbidos por ignore_na), por lo que
# ese diagnóstico prácticamente siempre reporta 0% NA — no refleja el
# missingness real de la encuesta.
# PROPUESTA: calcular aquí, sobre survey_enriched2 (justo después del
# merge del BLOQUE 14 y ANTES de cualquier dummy_cols()/replace_na()),
# el missingness ORIGINAL de las variables categóricas que alimentan las
# dummies de V (cols_dummy_es, raza_*, cambio_*) y de las variables RFM
# que alimentan W (recency/frequency/monetary), además de n_states.
# El diagnóstico post-tratamiento (más abajo) se conserva pero se
# documenta explícitamente como "post-tratamiento", no se elimina.
# -----------------------------------------------------------------------------

vars_v_w_fuente <- c(
  "frecuencia_compra_amazon","grupo_edad","origen_hispano",
  "nivel_educativo","rango_ingresos","genero",
  "orientacion_sexual","consumo_cigarrillos","consumo_marihuana",
  "consumo_alcohol","diabetes_hogar","silla_ruedas_hogar",
  "personas_comparten_cuenta","tamano_hogar",
  "recency_days_2018_Adidas","frequency_2018_Adidas","monetary_2018_Adidas",
  "n_states","top_state"
)
vars_v_w_fuente <- c(
  vars_v_w_fuente,
  grep("^raza_",   names(survey_enriched2), value = TRUE),
  grep("^cambio_", names(survey_enriched2), value = TRUE)
)
vars_v_w_fuente <- intersect(unique(vars_v_w_fuente), names(survey_enriched2))

diagnostico_missing_original <- tibble::tibble(
  variable = vars_v_w_fuente,
  n        = nrow(survey_enriched2),
  n_na     = sapply(vars_v_w_fuente, function(v) sum(is.na(survey_enriched2[[v]]))),
  pct_na   = round(100 * sapply(vars_v_w_fuente, function(v) sum(is.na(survey_enriched2[[v]]))) / nrow(survey_enriched2), 2)
)

readr::write_csv(diagnostico_missing_original, file.path(ruta_salidas, "diagnostico_missing_original.csv"))
message("Missingness ORIGINAL (antes de dummy_cols/replace_na) exportado: diagnostico_missing_original.csv")

# -----------------------------------------------------------------------------
# BLOQUE 15 — Creación de dummies  [BLOQUE ORIGINAL 15 — MODIFICADO]
#
# PROBLEMA DETECTADO: el original fija explícitamente los niveles del
# factor (levels = c("18 - 24 years", ...)) para controlar la categoría
# de referencia de las dummies. Como survey_es ya tiene los VALORES
# traducidos al español (grupo_edad, personas_comparten_cuenta), no
# podemos reutilizar los niveles en inglés: si el texto exacto no
# coincide carácter por carácter con el valor traducido real, todas las
# filas quedarían como NA en el factor y la dummy se perdería en
# silencio (justo el tipo de error que motivó el bloque de
# verificación de traducción de code1.R).
# EXPLICACIÓN: no se puede adivinar con certeza la cadena española exacta
# sin volver a mirar verificacion_traduccion.csv de code1.R.
# PROPUESTA (segura, no destructiva): NO fijar niveles a mano. Se usa
# factor(as.character(x)) dejando que R ordene alfabéticamente. Esto
# preserva TODAS las categorías (ninguna se pierde), pero la categoría de
# referencia (la que se elimina con remove_first_dummy = TRUE) puede
# diferir de la del script original. Se deja esto documentado; si el
# usuario confirma las cadenas exactas via verificacion_traduccion.csv,
# se puede fijar el orden deseado explícitamente en una siguiente
# iteración.
# -----------------------------------------------------------------------------

if ("grupo_edad" %in% names(survey_enriched2)) {
  survey_enriched2$grupo_edad <- factor(as.character(survey_enriched2$grupo_edad))
}
if ("personas_comparten_cuenta" %in% names(survey_enriched2)) {
  survey_enriched2$personas_comparten_cuenta <- factor(as.character(survey_enriched2$personas_comparten_cuenta))
}

cols_dummy_es <- c(
  "frecuencia_compra_amazon","grupo_edad","origen_hispano",
  "nivel_educativo","rango_ingresos","genero",
  "orientacion_sexual","consumo_cigarrillos","consumo_marihuana",
  "consumo_alcohol","diabetes_hogar","silla_ruedas_hogar",
  "personas_comparten_cuenta","tamano_hogar"
)
cols_dummy_es <- intersect(cols_dummy_es, names(survey_enriched2))

data_dum <- survey_enriched2 %>%
  fastDummies::dummy_cols(
    select_columns = cols_dummy_es,
    remove_first_dummy      = TRUE,
    remove_selected_columns = TRUE,
    ignore_na               = TRUE
  ) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)))

# -----------------------------------------------------------------------------
# BLOQUE 16 — Construcción de W_pre + features mensuales 12m
# [BLOQUE ORIGINAL 16 — CONSERVADO]
# Sin cambios (los nombres RFM son técnicos internos: se conservan en
# inglés, p. ej. recency_days_2018_Adidas, según lo permitido para
# nombres técnicos).
# -----------------------------------------------------------------------------

W_pre <- data_dum %>%
  dplyr::select(
    response_id,
    recency_days_2018_Adidas,
    frequency_2018_Adidas,
    monetary_2018_Adidas
  ) %>%
  dplyr::distinct(response_id, .keep_all = TRUE) %>%
  dplyr::mutate(response_id = as.character(response_id))

# [BLOQUE 11] anomalous_ids_severe ya no elimina registros (ver BLOQUE 11):
# se dejó este bloque intacto en su forma condicional; con
# anomalous_ids_severe = character(0) la condición nunca se cumple y
# W_pre conserva TODOS los response_id, incluidos los de montos severos.
if (length(anomalous_ids_severe) > 0) {
  W_pre <- W_pre %>% dplyr::filter(!response_id %in% anomalous_ids_severe)
}

ids_w <- unique(W_pre$response_id)

anchor_month <- lubridate::floor_date(ref_date2, "month")
months_12    <- seq(anchor_month %m-% months(11), anchor_month, by = "month")

adidas_monthly <- purchases_adidas %>%
  dplyr::filter(
    is_adidas == TRUE,
    dplyr::between(order_date, ref_date1, ref_date2),
    response_id %in% ids_w
  ) %>%
  dplyr::mutate(ym = lubridate::floor_date(order_date, "month")) %>%
  dplyr::group_by(response_id, ym) %>%
  dplyr::summarise(
    dollar_m   = sum(total_price, na.rm = TRUE),
    products_m = sum(quantity,    na.rm = TRUE),
    .groups = "drop"
  )

grid_12 <- tidyr::expand_grid(response_id = ids_w, ym = months_12) %>%
  dplyr::left_join(adidas_monthly, by = c("response_id", "ym")) %>%
  dplyr::mutate(
    dollar_m   = tidyr::replace_na(dollar_m, 0),
    products_m = tidyr::replace_na(products_m, 0),
    mnum       = lubridate::month(ym)
  )

safe_div <- function(num, den) ifelse(den == 0, 0, num / den)
topk_avg <- function(x, k) mean(utils::head(sort(x, decreasing = TRUE), k))

feat_12 <- grid_12 %>%
  dplyr::group_by(response_id) %>%
  dplyr::summarise(
    dollar_avg_12m       = mean(dollar_m),
    products_avg_12m     = mean(products_m),
    dollar_std_12m       = stats::sd(dollar_m),
    products_std_12m     = stats::sd(products_m),
    dollar_top1_12m      = max(dollar_m),
    dollar_top3_avg_12m  = topk_avg(dollar_m, 3),
    dollar_top6_avg_12m  = topk_avg(dollar_m, 6),
    products_top1_12m    = max(products_m),
    products_top3_avg_12m = topk_avg(products_m, 3),
    products_top6_avg_12m = topk_avg(products_m, 6),
    dollar_spring_avg_12m   = mean(dollar_m[mnum %in% c(3,4,5)]),
    dollar_summer_avg_12m   = mean(dollar_m[mnum %in% c(6,7,8)]),
    dollar_autumn_avg_12m   = mean(dollar_m[mnum %in% c(9,10,11)]),
    dollar_winter_avg_12m   = mean(dollar_m[mnum %in% c(12,1,2)]),
    products_spring_avg_12m = mean(products_m[mnum %in% c(3,4,5)]),
    products_summer_avg_12m = mean(products_m[mnum %in% c(6,7,8)]),
    products_autumn_avg_12m = mean(products_m[mnum %in% c(9,10,11)]),
    products_winter_avg_12m = mean(products_m[mnum %in% c(12,1,2)]),
    months_with_purchase_12m = sum(products_m > 0),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dollar_std_12m   = tidyr::replace_na(dollar_std_12m, 0),
    products_std_12m = tidyr::replace_na(products_std_12m, 0)
  )

feat_ratios_12 <- feat_12 %>%
  dplyr::transmute(
    response_id,
    dollar_avg12_top1_ratio   = safe_div(dollar_avg_12m, dollar_top1_12m),
    dollar_avg12_top3_ratio   = safe_div(dollar_avg_12m, dollar_top3_avg_12m),
    dollar_avg12_top6_ratio   = safe_div(dollar_avg_12m, dollar_top6_avg_12m),
    products_avg12_top1_ratio = safe_div(products_avg_12m, products_top1_12m),
    products_avg12_top3_ratio = safe_div(products_avg_12m, products_top3_avg_12m),
    products_avg12_top6_ratio = safe_div(products_avg_12m, products_top6_avg_12m)
  )

adidas_feats_12 <- feat_12 %>% dplyr::left_join(feat_ratios_12, by = "response_id")

cols_new <- setdiff(names(adidas_feats_12), "response_id")
W_pre    <- W_pre %>% dplyr::select(-dplyr::any_of(cols_new))
W_pre    <- W_pre %>% dplyr::left_join(adidas_feats_12, by = "response_id")

# -----------------------------------------------------------------------------
# BLOQUE 17 — Construcción de V_pre  [BLOQUE ORIGINAL 17 — MODIFICADO]
#
# PROBLEMA DETECTADO: el original selecciona starts_with("race_") y
# starts_with("life_"). Como code1.R renombró esas dummies a español
# (raza_negra_afroamericana, ..., cambio_perdida_empleo, ...), esos
# prefijos ya no existen en survey_es y V_pre quedaría vacío de esas
# variables (fallo silencioso, sin error de R).
# EXPLICACIÓN: dummy_cols() de este mismo script (BLOQUE 15) genera,
# además, dummies con prefijo grupo_edad_, rango_ingresos_ y
# personas_comparten_cuenta_ en lugar de q_demos_age_, q_demos_income_ y
# q_amazon_use_howmany_.
# PROPUESTA: actualizar los prefijos de starts_with() a los nombres
# españoles reales: "raza_", "cambio_", "grupo_edad_", "rango_ingresos_",
# "personas_comparten_cuenta_". n_states se conserva igual (se genera
# dentro de este mismo script, no viene de survey_es).
# -----------------------------------------------------------------------------

V_pre <- data_dum %>%
  dplyr::select(
    response_id,
    dplyr::starts_with("raza_"),
    dplyr::starts_with("cambio_"),
    dplyr::starts_with("grupo_edad_"),
    dplyr::starts_with("rango_ingresos_"),
    dplyr::starts_with("personas_comparten_cuenta_"),
    n_states
  ) %>%
  dplyr::distinct(response_id, .keep_all = TRUE) %>%
  dplyr::mutate(response_id = as.character(response_id))

# [BLOQUE 11] Igual que en W_pre: anomalous_ids_severe = character(0),
# por lo que esta condición nunca se cumple y V_pre conserva TODOS los
# response_id.
if (length(anomalous_ids_severe) > 0) {
  V_pre <- V_pre %>% dplyr::filter(!response_id %in% anomalous_ids_severe)
}

# -----------------------------------------------------------------------------
# BLOQUE 18 — Universo maestro y alineación V/W/var_2023
# [BLOQUE ORIGINAL 18 — CONSERVADO]
# Sin cambios.
# -----------------------------------------------------------------------------

ids_master <- intersect(V_pre$response_id, W_pre$response_id)
stopifnot(length(ids_master) > 0)
ids_master <- W_pre$response_id[W_pre$response_id %in% ids_master]

var_2023_full <- tibble::tibble(response_id = ids_master) %>%
  dplyr::left_join(var_2023, by = "response_id") %>%
  dplyr::mutate(amount_2023_Adidas = tidyr::replace_na(amount_2023_Adidas, 0))

V_full <- V_pre[match(ids_master, V_pre$response_id), , drop = FALSE]
W_full <- W_pre[match(ids_master, W_pre$response_id), , drop = FALSE]

stopifnot(identical(V_full$response_id, ids_master))
stopifnot(identical(W_full$response_id, ids_master))
stopifnot(identical(var_2023_full$response_id, ids_master))
stopifnot(!any(is.na(var_2023_full$amount_2023_Adidas)))

V_aligned        <- V_full
W_aligned        <- W_full
var_2023_aligned <- var_2023_full

# -----------------------------------------------------------------------------
# BLOQUE 19 — Split TRAIN/TEST 80/20  [BLOQUE ORIGINAL 19 — MODIFICADO
# (agregado: verificación explícita de intersección vacía)]
#
# PROBLEMA DETECTADO: el original no verificaba explícitamente que
# TRAIN y TEST no compartan IDs (aunque por construcción con sample()/
# setdiff() no debería ocurrir).
# EXPLICACIÓN: dado que este split es ahora la base común para TODOS los
# scripts de modelado futuros (code3 en adelante), es crítico blindarlo
# con una verificación explícita, no solo confiar en que la lógica de
# muestreo sea correcta.
# PROPUESTA: agregar stopifnot(length(intersect(train_ids, test_ids)) == 0)
# inmediatamente después del split.
# -----------------------------------------------------------------------------

set.seed(123)
ids <- V_aligned$response_id
n   <- length(ids)

train_ids <- sample(ids, size = floor(0.8 * n), replace = FALSE)
test_ids  <- setdiff(ids, train_ids)

stopifnot(length(intersect(train_ids, test_ids)) == 0)   # AGREGADO

train_idx <- match(train_ids, ids)
test_idx  <- match(test_ids,  ids)

V_train <- V_aligned[train_idx, , drop = FALSE]
V_test  <- V_aligned[test_idx,  , drop = FALSE]
W_train <- W_aligned[train_idx, , drop = FALSE]
W_test  <- W_aligned[test_idx,  , drop = FALSE]

var_train <- var_2023_aligned[train_idx, , drop = FALSE]
var_test  <- var_2023_aligned[test_idx,  , drop = FALSE]

message("Split sizes: train=", nrow(V_train), " | test=", nrow(V_test))

# =============================================================================
# BLOQUE 20 [ORIGINAL] — Winsorización post-split -> AGREGADO como
# DIAGNÓSTICO (no se aplica a los datos)
#
# PROBLEMA DETECTADO: el bloque 20 original APLICA winsorización Tukey
# (k=3) directamente sobre Y, W numéricas y n_states, modificando los
# valores antes de exportarlos.
# EXPLICACIÓN: según la nueva arquitectura, la winsorización aplicada es
# una decisión de preparación específica del modelo POGIT/conteo
# (necesaria para estabilizar la relación varianza/media que define
# c_train), no una regla universal de "construcción común". Aplicarla
# aquí destruiría información (el monto real) que otro modelo futuro
# (p. ej. un modelo robusto a outliers, o un análisis puramente
# descriptivo) podría necesitar sin capar.
# PROPUESTA: en code2.R se calculan los caps de Tukey y el % de
# observaciones que superarían el cap SOLO A MODO DE DIAGNÓSTICO
# (exportado a diagnostico_outliers.csv), sin modificar V_raw / W_raw /
# Y_amount. code3.R decidirá si aplica esos caps antes de construir
# c_train / Y_count.
# =============================================================================

diagnosticar_outliers <- function(x, nombre, k = 3) {
  x_num <- suppressWarnings(as.numeric(x))
  tk    <- tukey_cap_upper(x_num, k = k)
  n_val   <- sum(!is.na(x_num))
  n_sobre <- sum(!is.na(x_num) & x_num > tk$cap)
  tibble::tibble(
    variable      = nombre,
    n             = n_val,
    q3            = tk$q3,
    iqr           = tk$iqr,
    cap_tukey_k3  = tk$cap,
    n_sobre_cap   = n_sobre,
    pct_sobre_cap = if (n_val > 0) round(100 * n_sobre / n_val, 2) else NA_real_
  )
}

diag_outliers <- dplyr::bind_rows(
  diagnosticar_outliers(var_train$amount_2023_Adidas, "Y_amount (amount_2023_Adidas) - TRAIN"),
  purrr::map_dfr(
    setdiff(names(W_train)[sapply(W_train, is.numeric)], "response_id"),
    ~ diagnosticar_outliers(W_train[[.x]], paste0("W_raw: ", .x, " - TRAIN"))
  ),
  if ("n_states" %in% names(V_train) && is.numeric(V_train$n_states))
    diagnosticar_outliers(V_train$n_states, "V_raw: n_states - TRAIN") else NULL
)

readr::write_csv(diag_outliers, file.path(ruta_salidas, "diagnostico_outliers.csv"))

tabla_outliers <- diag_outliers %>%
  dplyr::transmute(
    Variable            = variable,
    n                   = fmt_miles_es(n),
    Q3                  = fmt_dec_es(q3, 2),
    IQR                 = fmt_dec_es(iqr, 2),
    `Cap Tukey (k=3)`   = fmt_dec_es(cap_tukey_k3, 2),
    `N sobre cap`       = fmt_miles_es(n_sobre_cap),
    `% sobre cap`       = fmt_pct_es(pct_sobre_cap, 0.1)
  )
graficar_tabla_metrica(
  tabla_outliers,
  titulo = "Diagnostico de outliers (Tukey, k=3) - Adidas",
  subtitulo = "Solo diagnostico: NO se aplica sobre V_raw/W_raw/Y_amount en code2.R",
  nombre_archivo = "tabla_01_diagnostico_outliers"
)

# =============================================================================
# BLOQUE 21 [ORIGINAL] — Construcción de Y -> MODIFICADO: Y_amount cruda,
# SIN c_train / Y_count
#
# PROBLEMA DETECTADO: el bloque 21 original divide el monto por
# c_train = Var(Y)/Media(Y) y redondea a entero, produciendo Y_train/
# Y_test como CONTEOS discretizados — una transformación específica para
# el modelo POGIT.
# EXPLICACIÓN: este script debe entregar Y_amount = el monto real en
# dólares, sin discretizar, para que sea reutilizable por cualquier
# modelo futuro (conteo, regresión continua, etc.).
# PROPUESTA: Y_amount_train / Y_amount_test = amount_2023_Adidas crudo
# (ya winsorizado NO, ver bloque 20). El cálculo de c_train y Y_count
# se TRASLADA A CODE3.R.
# =============================================================================

Y_amount_train <- var_train$amount_2023_Adidas
Y_amount_test  <- var_test$amount_2023_Adidas

message("Y_amount (dolares) -> train: n=", length(Y_amount_train),
        " media=", round(mean(Y_amount_train), 2),
        " | test: n=", length(Y_amount_test),
        " media=", round(mean(Y_amount_test), 2))

# =============================================================================
# BLOQUE NUEVO — Diagnósticos EDA (AGREGADO)
# Missingness, correlación y VIF, calculados solo con fines de
# diagnóstico exploratorio, sin transformar V_raw / W_raw / Y_amount.
# =============================================================================

# --- Missingness (POST-tratamiento) ------------------------------------------
# NOTA (documentación del tratamiento aplicado, sin volver a calcularlo
# aquí): este diagnóstico se calcula sobre V_train/W_train/V_test/W_test,
# es decir DESPUÉS de que el BLOQUE 15 aplicó
# fastDummies::dummy_cols(ignore_na = TRUE) y
# dplyr::mutate(across(where(is.numeric), ~ replace_na(.x, 0))). Por
# construcción, casi siempre reportará 0% NA. El missingness ORIGINAL
# (antes de ese tratamiento) está en diagnostico_missing_original.csv,
# exportado justo después del BLOQUE 14.

calcular_missing <- function(df, nombre_base) {
  tibble::tibble(
    base     = nombre_base,
    variable = names(df),
    n        = nrow(df),
    n_na     = sapply(df, function(x) sum(is.na(x))),
    pct_na   = round(100 * sapply(df, function(x) sum(is.na(x))) / nrow(df), 2)
  )
}

diag_missing <- dplyr::bind_rows(
  calcular_missing(V_train, "V_raw_train"),
  calcular_missing(V_test,  "V_raw_test"),
  calcular_missing(W_train, "W_raw_train"),
  calcular_missing(W_test,  "W_raw_test"),
  tibble::tibble(base = "Y_amount_train", variable = "amount_2023_Adidas",
                 n = length(Y_amount_train), n_na = sum(is.na(Y_amount_train)),
                 pct_na = round(100 * sum(is.na(Y_amount_train)) / length(Y_amount_train), 2)),
  tibble::tibble(base = "Y_amount_test", variable = "amount_2023_Adidas",
                 n = length(Y_amount_test), n_na = sum(is.na(Y_amount_test)),
                 pct_na = round(100 * sum(is.na(Y_amount_test)) / length(Y_amount_test), 2))
)

readr::write_csv(diag_missing, file.path(ruta_salidas, "diagnostico_missing.csv"))

# --- Correlación (Spearman) sobre W numéricas en TRAIN, SOLO diagnóstico ----

W_train_num <- W_train %>%
  dplyr::select(-response_id) %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0)))

diag_correlacion <- tibble::tibble(var1 = character(), var2 = character(), spearman = double())

if (ncol(W_train_num) >= 2) {
  cor_mat <- suppressWarnings(stats::cor(W_train_num, method = "spearman",
                                         use = "pairwise.complete.obs"))
  cor_mat[upper.tri(cor_mat, diag = TRUE)] <- NA
  diag_correlacion <- as.data.frame(as.table(cor_mat)) %>%
    stats::setNames(c("var1", "var2", "spearman")) %>%
    dplyr::filter(!is.na(spearman)) %>%
    dplyr::mutate(spearman = round(spearman, 4)) %>%
    dplyr::arrange(dplyr::desc(abs(spearman))) %>%
    tibble::as_tibble()
}

readr::write_csv(diag_correlacion, file.path(ruta_salidas, "diagnostico_correlacion.csv"))

# --- VIF manual (sin depender del paquete 'car') -----------------------------
# VIF_j = 1 / (1 - R^2_j), donde R^2_j sale de regresionar la variable j
# contra todas las demás variables numéricas de W (solo TRAIN).

calcular_vif_manual <- function(df_numeric) {
  df_numeric <- df_numeric %>% dplyr::select(where(~ stats::sd(.x, na.rm = TRUE) > 0))
  nombres <- names(df_numeric)
  if (length(nombres) < 2) return(tibble::tibble(variable = character(), VIF = double()))
  vif_out <- vapply(nombres, function(v) {
    otras <- setdiff(nombres, v)
    formula_str <- paste0("`", v, "` ~ ", paste0("`", otras, "`", collapse = " + "))
    modelo <- tryCatch(stats::lm(stats::as.formula(formula_str), data = df_numeric),
                       error = function(e) NULL)
    if (is.null(modelo)) return(NA_real_)
    r2 <- summary(modelo)$r.squared
    if (is.finite(r2) && r2 < 1) 1 / (1 - r2) else Inf
  }, numeric(1))
  tibble::tibble(variable = names(vif_out), VIF = round(unname(vif_out), 3)) %>%
    dplyr::arrange(dplyr::desc(VIF))
}

diag_vif <- calcular_vif_manual(W_train_num)
readr::write_csv(diag_vif, file.path(ruta_salidas, "diagnostico_vif.csv"))

if (nrow(diag_vif) > 0) {
  tabla_vif <- diag_vif %>% dplyr::transmute(Variable = variable, VIF = fmt_dec_es(VIF, 3))
  graficar_tabla_metrica(
    tabla_vif,
    titulo = "Factor de inflacion de varianza (VIF) - W_train (Adidas)",
    subtitulo = "Calculado manualmente (regresion de cada variable contra las demas), solo TRAIN",
    nombre_archivo = "tabla_02_diagnostico_vif"
  )
} else {
  message("BLOQUE VIF: menos de 2 variables numericas validas; no se genera tabla_02_diagnostico_vif.")
}

# --- Resumen general de variables (medias/medianas/sd/min/max) --------------

resumir_numericas <- function(df, nombre_base) {
  df_num <- df %>% dplyr::select(where(is.numeric))
  if (ncol(df_num) == 0) return(tibble::tibble())
  purrr::map_dfr(names(df_num), function(v) {
    x <- df_num[[v]]
    tibble::tibble(
      base = nombre_base, variable = v, n = sum(!is.na(x)),
      media = round(mean(x, na.rm = TRUE), 3), mediana = round(stats::median(x, na.rm = TRUE), 3),
      sd = round(stats::sd(x, na.rm = TRUE), 3),
      min = round(min(x, na.rm = TRUE), 3), max = round(max(x, na.rm = TRUE), 3)
    )
  })
}

resumen_variables <- dplyr::bind_rows(
  resumir_numericas(V_train, "V_raw_train"),
  resumir_numericas(V_test,  "V_raw_test"),
  resumir_numericas(W_train, "W_raw_train"),
  resumir_numericas(W_test,  "W_raw_test"),
  tibble::tibble(base = "Y_amount_train", variable = "amount_2023_Adidas", n = length(Y_amount_train),
                 media = round(mean(Y_amount_train), 3), mediana = round(stats::median(Y_amount_train), 3),
                 sd = round(stats::sd(Y_amount_train), 3), min = round(min(Y_amount_train), 3),
                 max = round(max(Y_amount_train), 3)),
  tibble::tibble(base = "Y_amount_test", variable = "amount_2023_Adidas", n = length(Y_amount_test),
                 media = round(mean(Y_amount_test), 3), mediana = round(stats::median(Y_amount_test), 3),
                 sd = round(stats::sd(Y_amount_test), 3), min = round(min(Y_amount_test), 3),
                 max = round(max(Y_amount_test), 3))
)

readr::write_csv(resumen_variables, file.path(ruta_salidas, "resumen_variables.csv"))

tabla_resumen_variables <- resumen_variables %>%
  dplyr::transmute(
    Base              = base,
    Variable          = variable,
    n                 = fmt_miles_es(n),
    Media             = fmt_dec_es(media, 3),
    Mediana           = fmt_dec_es(mediana, 3),
    `Desv. estandar`  = fmt_dec_es(sd, 3),
    Minimo            = fmt_dec_es(min, 3),
    Maximo            = fmt_dec_es(max, 3)
  )
# PROBLEMA DETECTADO -> EXPLICACION -> PROPUESTA: esta tabla resume TODAS
# las variables numericas de V/W/Y (decenas de filas), por lo que la
# imagen resultante es larga (una tabla de una sola pagina de tesis no le
# alcanza). Se exporta igual, completa, para no truncar informacion en
# silencio; el usuario decide si la recorta o la divide al pegarla.
graficar_tabla_metrica(
  tabla_resumen_variables,
  titulo = "Resumen de variables numericas (V_raw, W_raw, Y_amount) - Adidas",
  subtitulo = "Media, mediana, desviacion estandar, minimo y maximo por base y conjunto (train/test)",
  nombre_archivo = "tabla_03_resumen_variables",
  alineacion = c("left", "left", "center", "center", "center", "center", "center", "center")
)

# =============================================================================
# BLOQUE 22 [ORIGINAL] — Gráfico de distribución de Y -> MODIFICADO:
# histograma + boxplot de Y_amount CONTINUO (dólares), en español, estilo
# code1.R (título 19 / ejes 15 / texto 13), colores multi-barra, sin el
# tema "poster" de 44-54pt del original.
# =============================================================================

graficar_distribucion_y <- function(vec, titulo, subtitulo, nombre_archivo) {
  df_y <- tibble::tibble(monto = vec)
  media   <- round(mean(vec, na.rm = TRUE), 2)
  mediana <- round(stats::median(vec, na.rm = TRUE), 2)
  n_ceros <- sum(vec == 0, na.rm = TRUE)
  pct_ceros <- round(100 * n_ceros / length(vec), 1)

  p <- ggplot2::ggplot(df_y, ggplot2::aes(x = monto)) +
    ggplot2::geom_histogram(bins = 30, fill = pal[1], color = "white", alpha = 0.9) +
    ggplot2::geom_vline(xintercept = media, linetype = "dashed", color = "#222222", linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = mediana, linetype = "dotted", color = "#222222", linewidth = 0.9) +
    ggplot2::annotate("label", x = Inf, y = Inf, hjust = 1.02, vjust = 1.05,
                      label = paste0("n = ", scales::comma(length(vec)), "\n",
                                     "Media = $", scales::comma(media), "\n",
                                     "Mediana = $", scales::comma(mediana), "\n",
                                     "Y = 0: ", scales::comma(n_ceros), " (", pct_ceros, "%)"),
                      family = "times_new_roman", size = 4, fill = "white", color = "#1a1a1a") +
    ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = titulo, subtitle = subtitulo,
                  x = "Monto gastado en Adidas en T2 (Y_amount, USD)",
                  y = "Frecuencia absoluta") +
    tema_grafica

  exportar_grafica(p, nombre_archivo, ancho = 10, alto = 6.5)
  invisible(p)
}

graficar_distribucion_y(
  Y_amount_train,
  "Distribucion de Y_amount - Conjunto de entrenamiento (Adidas)",
  "Monto real en dolares (sin discretizar) | Division 80/20 con set.seed(123)",
  "grafica_01_distribucion_y_amount_entrenamiento"
)

graficar_distribucion_y(
  Y_amount_test,
  "Distribucion de Y_amount - Conjunto de prueba (Adidas)",
  "Monto real en dolares (sin discretizar) | Division 80/20 con set.seed(123)",
  "grafica_02_distribucion_y_amount_prueba"
)

df_box <- dplyr::bind_rows(
  tibble::tibble(monto = Y_amount_train, conjunto = "Entrenamiento"),
  tibble::tibble(monto = Y_amount_test,  conjunto = "Prueba")
)

p_box <- ggplot2::ggplot(df_box, ggplot2::aes(x = conjunto, y = monto, fill = conjunto)) +
  ggplot2::geom_boxplot(alpha = 0.85, outlier.color = pal[2]) +
  ggplot2::scale_fill_manual(values = pal[1:2], guide = "none") +
  ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
  ggplot2::labs(title = "Y_amount por conjunto - Adidas",
                subtitle = "Comparacion de distribucion entre entrenamiento y prueba",
                x = NULL, y = "Monto gastado en Adidas en T2 (USD)") +
  tema_grafica

exportar_grafica(p_box, "grafica_03_boxplot_y_amount_train_test", ancho = 8, alto = 6.5)

# =============================================================================
# BLOQUE 23 [ORIGINAL] — SoW empirico en T2 -> MODIFICADO: SioW ahora es el
# tamaño de billetera CONTINUO en dolares (sin dividir por c_train, que
# ya no existe en este script).
#
# PROBLEMA DETECTADO: el original define siow = round((amount_adidas +
# amount_otherbrands) / c_train, 0), es decir, un tamaño de billetera
# discretizado en unidades de c_train (concepto específico de POGIT).
# EXPLICACIÓN: sin c_train en este script, no existe una forma no
# arbitraria de discretizar. Ademas, para el diagnostico exploratorio
# general, el tamaño de billetera en dolares es mas interpretable.
# PROPUESTA: SioW = amount_total (dolares), continuo. El calculo de una
# version discretizada especifica para POGIT se TRASLADA A CODE3.R.
# =============================================================================

ids_sow_test <- Reduce(intersect, list(W_test$response_id, V_test$response_id))
ids_sow_test <- sort(as.character(ids_sow_test))
stopifnot(length(ids_sow_test) > 0)

reorder_by_ids <- function(df, ids, id_col = "response_id", keep_all_ids = TRUE) {
  stopifnot(id_col %in% names(df))
  ids <- as.character(ids)
  df[[id_col]] <- as.character(df[[id_col]])
  if (keep_all_ids) {
    miss <- setdiff(ids, df[[id_col]])
    if (length(miss) > 0)
      df <- dplyr::bind_rows(df, tibble::tibble(!!id_col := miss))
  }
  out <- df[match(ids, df[[id_col]]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# DISCREPANCIA DE CATEGORÍA DETECTADA (ver también BLOQUE 10, donde se
# documenta en detalle): `period_df` restringe a
# `broad_cat %in% c("Apparel", "Sports_Outdoors")`, mientras que
# Y_amount (BLOQUE 10) no aplica ningún filtro de `broad_cat` sobre el
# gasto Adidas. Por lo tanto `amount_adidas` (y en consecuencia
# SioW_amount, SoW y PoW_amount) puede no coincidir con Y_amount para el
# mismo cliente si existe gasto Adidas fuera de esas dos broad_cat. NO
# se modifica el filtro aquí; queda pendiente de decisión metodológica
# (ver detalle y opciones en BLOQUE 10).

period_df <- purchases_cat %>%
  dplyr::filter(
    dplyr::between(order_date, ref_date3, ref_date4),
    response_id %in% ids_sow_test,
    broad_cat %in% c("Apparel", "Sports_Outdoors")
  )

adidas_vs_others <- period_df %>%
  dplyr::group_by(response_id) %>%
  dplyr::summarise(
    amount_adidas      = sum(dplyr::if_else(is_adidas == TRUE,  amount, 0), na.rm = TRUE),
    amount_otherbrands = sum(dplyr::if_else(is_adidas == FALSE, amount, 0), na.rm = TRUE),
    n_total            = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::right_join(tibble::tibble(response_id = ids_sow_test), by = "response_id") %>%
  dplyr::mutate(
    amount_adidas      = tidyr::replace_na(amount_adidas, 0),
    amount_otherbrands = tidyr::replace_na(amount_otherbrands, 0),
    n_total            = tidyr::replace_na(n_total, 0),
    # CORRECCION METODOLOGICA (definicion del anteproyecto, sin c_train):
    #   SioW_amount = gasto total observado del cliente en la categoria de
    #                 analisis (Adidas + otras marcas). Escala monetaria
    #                 original: NO se divide, NO se discretiza, NO se
    #                 redondea, NO depende de c_train.
    #   SoW         = proporcion del gasto de la categoria que es Adidas
    #                 (amount_adidas / SioW_amount; 0 si SioW_amount = 0).
    #   PoW_amount  = parte del gasto de la categoria que Adidas todavia
    #                 no capta (potencial de billetera), en dolares.
    amount_total  = amount_adidas + amount_otherbrands,
    SioW_amount   = amount_total,
    SoW           = dplyr::if_else(SioW_amount > 0, amount_adidas / SioW_amount, 0),
    PoW_amount    = SioW_amount - amount_adidas
  ) %>%
  dplyr::select(response_id, amount_adidas, amount_otherbrands, amount_total,
               SioW_amount, SoW, PoW_amount)

adidas_vs_others2 <- reorder_by_ids(adidas_vs_others, ids_sow_test, keep_all_ids = TRUE)
stopifnot(nrow(adidas_vs_others2) == length(ids_sow_test))

# -----------------------------------------------------------------------------
# Validaciones obligatorias de SioW_amount / SoW / PoW_amount (AGREGADO)
# Cualquier problema de calidad de dato se reporta explicitamente (stop/
# warning); nunca se corrige u oculta en silencio.
# -----------------------------------------------------------------------------

TOL_NUMERICA <- 1e-6

stopifnot(all(adidas_vs_others2$SioW_amount      >= 0, na.rm = TRUE))
stopifnot(all(adidas_vs_others2$amount_adidas    >= 0, na.rm = TRUE))
stopifnot(all(adidas_vs_others2$amount_otherbrands >= 0, na.rm = TRUE))

chequeo_suma <- abs(
  adidas_vs_others2$SioW_amount -
    (adidas_vs_others2$amount_adidas + adidas_vs_others2$amount_otherbrands)
)
if (any(chequeo_suma > TOL_NUMERICA, na.rm = TRUE)) {
  stop("SioW_amount no coincide con amount_adidas + amount_otherbrands para ",
       sum(chequeo_suma > TOL_NUMERICA, na.rm = TRUE),
       " cliente(s). Revisar calidad de dato antes de continuar.")
}

chequeo_pow <- abs(
  adidas_vs_others2$PoW_amount -
    (adidas_vs_others2$SioW_amount - adidas_vs_others2$amount_adidas)
)
if (any(chequeo_pow > TOL_NUMERICA, na.rm = TRUE)) {
  stop("PoW_amount no coincide con SioW_amount - amount_adidas para ",
       sum(chequeo_pow > TOL_NUMERICA, na.rm = TRUE),
       " cliente(s). Revisar calidad de dato antes de continuar.")
}

if (any(adidas_vs_others2$PoW_amount < -TOL_NUMERICA, na.rm = TRUE)) {
  n_pow_neg <- sum(adidas_vs_others2$PoW_amount < -TOL_NUMERICA, na.rm = TRUE)
  warning("PoW_amount es negativo para ", n_pow_neg, " cliente(s) (deberia ser ",
          ">= 0 si SioW_amount >= amount_adidas). Se reporta explicitamente; ",
          "revisar posible problema de calidad de dato en purchases_cat/",
          "purchases_adidas. No se corrige automaticamente.")
}

stopifnot(all(adidas_vs_others2$SoW >= 0 - TOL_NUMERICA, na.rm = TRUE))
stopifnot(all(adidas_vs_others2$SoW <= 1 + TOL_NUMERICA, na.rm = TRUE))

# =============================================================================
# BLOQUE 24 [ORIGINAL] — Gráficos SoW -> MODIFICADO: estilo code1.R
# NOTA: solo se actualizo la referencia de columna sow -> SoW (renombrada
# en el bloque anterior). El diseno, tamanos y estilo de la grafica NO
# se modificaron.
# =============================================================================

df_sow <- adidas_vs_others2 %>%
  dplyr::transmute(share = pmin(pmax(SoW, 0), 1)) %>%
  dplyr::filter(!is.na(share))

m_all  <- mean(df_sow$share,   na.rm = TRUE)
md_all <- stats::median(df_sow$share, na.rm = TRUE)

p_all <- ggplot2::ggplot(df_sow, ggplot2::aes(x = share)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                          binwidth = 0.01, boundary = 0, closed = "right",
                          fill = pal[3], color = "white", alpha = 0.9) +
  ggplot2::geom_vline(xintercept = m_all,  linetype = "dashed", color = "#222222", linewidth = 0.9) +
  ggplot2::geom_vline(xintercept = md_all, linetype = "dotted", color = "#222222", linewidth = 0.9) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
  ggplot2::coord_cartesian(xlim = c(0, 1)) +
  ggplot2::labs(title = "Participacion de Adidas en el gasto total (T2) - con ceros",
                subtitle = paste0("Conjunto de prueba | Media = ", scales::percent(m_all, accuracy = 0.1),
                                  " | Mediana = ", scales::percent(md_all, accuracy = 0.1)),
                x = "Participacion de Adidas en el gasto (SoW)", y = "Densidad") +
  tema_grafica

exportar_grafica(p_all, "grafica_04_sow_adidas_completo_con_ceros", ancho = 10, alto = 6.5)

tot_n <- nrow(df_sow)
counts_sow <- df_sow %>%
  dplyr::summarise(`SoW = 0` = sum(share == 0), `SoW > 0` = sum(share > 0)) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to = "Grupo", values_to = "n") %>%
  dplyr::mutate(pct = n / tot_n)

p_counts <- ggplot2::ggplot(counts_sow, ggplot2::aes(x = Grupo, y = n, fill = Grupo)) +
  ggplot2::geom_col(width = 0.5, color = "white") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(scales::comma(n), " (", scales::percent(pct, accuracy = 0.1), ")")),
                     vjust = -0.4, family = "times_new_roman", size = 4.5, color = "#1a1a1a") +
  ggplot2::scale_fill_manual(values = pal[4:5], guide = "none") +
  ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::labs(title = "Clientes sin y con gasto en Adidas en T2", subtitle = "Conjunto de prueba",
                x = NULL, y = "Numero de clientes") +
  tema_grafica

exportar_grafica(p_counts, "grafica_05_sow_adidas_ceros_vs_positivos", ancho = 8, alto = 6.5)

pos_sow <- df_sow %>% dplyr::filter(share > 0)
m_pos   <- mean(pos_sow$share,   na.rm = TRUE)
md_pos  <- stats::median(pos_sow$share, na.rm = TRUE)

p_pos <- ggplot2::ggplot(pos_sow, ggplot2::aes(x = share)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                          binwidth = 0.02, boundary = 0, closed = "right",
                          fill = pal[5], color = "white", alpha = 0.9) +
  ggplot2::geom_vline(xintercept = m_pos,  linetype = "dashed", color = "#222222", linewidth = 0.9) +
  ggplot2::geom_vline(xintercept = md_pos, linetype = "dotted", color = "#222222", linewidth = 0.9) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, 0.1)) +
  ggplot2::coord_cartesian(xlim = c(0, 1)) +
  ggplot2::labs(title = "Participacion de Adidas en el gasto total (T2) - solo compradores activos",
                subtitle = paste0("Conjunto de prueba, SoW > 0 | Media = ", scales::percent(m_pos, accuracy = 0.1),
                                  " | Mediana = ", scales::percent(md_pos, accuracy = 0.1)),
                x = "Participacion de Adidas en el gasto (SoW)", y = "Densidad") +
  tema_grafica

exportar_grafica(p_pos, "grafica_06_sow_adidas_solo_positivos", ancho = 10, alto = 6.5)

g_panel <- (p_all + ggplot2::labs(subtitle = NULL)) /
  (p_counts + ggplot2::labs(subtitle = NULL)) /
  (p_pos + ggplot2::labs(subtitle = NULL)) +
  patchwork::plot_layout(heights = c(0.4, 0.25, 0.35)) +
  patchwork::plot_annotation(
    title = "Vision integrada del SoW de Adidas - Conjunto de prueba",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(family = "times_new_roman", size = 19,
                                                               face = "bold", color = "#000000"))
  )

exportar_grafica(g_panel, "grafica_07_sow_adidas_panel_combinado", ancho = 10, alto = 14)

# =============================================================================
# BLOQUE 25 [ORIGINAL] — Exportacion empirical_sow.xlsx -> MODIFICADO:
# se consolida en una sola hoja con response_id, amount_adidas,
# amount_otherbrands, SioW_amount, SoW y PoW_amount para poder verificar
# los calculos directamente en el Excel. Ruta ya estaba actualizada a
# salidas/codigo_de_salida2/.
# =============================================================================

validacion_empirica_sow <- adidas_vs_others2 %>%
  dplyr::select(response_id, amount_adidas, amount_otherbrands, amount_total,
               SioW_amount, SoW, PoW_amount)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "validacion_sow")
openxlsx::writeData(wb, "validacion_sow", as.data.frame(validacion_empirica_sow))
openxlsx::saveWorkbook(wb, file = file.path(ruta_salidas, "empirical_sow.xlsx"), overwrite = TRUE)

# =============================================================================
# BLOQUE 26 [ORIGINAL] — Bubble plot SoW x SioW -> MODIFICADO: SioW en
# quintiles de dolares (continuo), estilo code1.R.
# NOTA: solo se actualizo la referencia de columnas sow -> SoW y
# siow -> SioW_amount (renombradas en BLOQUE 23). El diseno, tamanos y
# estilo de la grafica NO se modificaron.
# =============================================================================

breaks_sow <- c(0, .2, .4, .6, .8, 1)
labs_sow   <- c("[0, .2]","(.2, .4]","(.4, .6]","(.6, .8]","(.8, 1]")

plot_df <- adidas_vs_others2 %>%
  dplyr::transmute(sow = pmin(pmax(SoW, 0), 1), tw = SioW_amount) %>%
  dplyr::mutate(
    sow_bin = cut(sow, breaks = breaks_sow, include.lowest = TRUE, right = TRUE, labels = labs_sow),
    tw_q    = dplyr::ntile(tw, 5)
  ) %>%
  dplyr::filter(!is.na(sow_bin)) %>%
  dplyr::group_by(tw_q, sow_bin) %>%
  dplyr::summarise(n = dplyr::n(), avg_usd = mean(tw, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    tw_lab = factor(
      dplyr::case_when(
        tw_q == 1 ~ "Quintil inferior", tw_q == 2 ~ "2.o quintil",
        tw_q == 3 ~ "3.er quintil",     tw_q == 4 ~ "4.o quintil",
        tw_q == 5 ~ "Quintil superior"
      ),
      levels = c("Quintil superior","4.o quintil","3.er quintil","2.o quintil","Quintil inferior")
    ),
    sow_lab   = factor(sow_bin, levels = labs_sow),
    label_txt = scales::dollar(avg_usd, accuracy = 0.01)
  )

p_bubble <- ggplot2::ggplot(plot_df, ggplot2::aes(x = sow_lab, y = tw_lab)) +
  ggplot2::geom_point(ggplot2::aes(size = n), shape = 21, fill = pal[6], color = "white",
                      stroke = 0.7, alpha = 0.88) +
  ggplot2::geom_text(ggplot2::aes(label = label_txt), size = 4, family = "times_new_roman",
                     color = "#000000", fontface = "bold") +
  ggplot2::scale_size_continuous(name = "N clientes", range = c(4, 16)) +
  ggplot2::labs(title = "Clientes por participacion de marca (SoW) y tamano de billetera (SioW)",
                subtitle = "Adidas - conjunto de prueba - SioW en dolares (monto total T2)",
                x = "SoW - intervalos de 0.2", y = "SioW - quintiles (USD)") +
  tema_grafica +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                axis.line  = ggplot2::element_line(color = "#555555", linewidth = 0.5))

exportar_grafica(p_bubble, "grafica_08_bubble_sow_siow_adidas", ancho = 10, alto = 6.5)

# =============================================================================
# BLOQUE 27 [ORIGINAL] — Firmas de marca -> MODIFICADO: estilo code1.R,
# exportacion individual PDF+SVG+PNG (sin PDF combinado adicional).
# =============================================================================

ids_w_train <- W_aligned$response_id[train_idx]
ids_w_test  <- W_aligned$response_id[test_idx]

data1_marcas_2023 <- survey_enriched2 %>%
  dplyr::select(response_id, dplyr::starts_with("2023_sportbrand_")) %>%
  dplyr::filter(response_id %in% ids_w_test)

if ("2023_sportbrand_Adidas" %in% names(data1_marcas_2023)) {
  pos1 <- which(data1_marcas_2023$`2023_sportbrand_Adidas` != 0)
} else {
  pos1 <- integer(0)
}

brand_cols <- grep("^2023_sportbrand_", names(survey_enriched2), value = TRUE)

if (length(pos1) > 0 && length(brand_cols) > 0) {

  ids_interes <- data1_marcas_2023$response_id[pos1]

  df_counts <- survey_enriched2 %>%
    dplyr::select(response_id, dplyr::all_of(brand_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(brand_cols), ~ tidyr::replace_na(as.numeric(.x), 0))) %>%
    dplyr::mutate(total_units = rowSums(dplyr::across(dplyr::all_of(brand_cols)), na.rm = TRUE))

  df_long <- df_counts %>%
    tidyr::pivot_longer(cols = dplyr::all_of(brand_cols), names_to = "brand", values_to = "units") %>%
    dplyr::mutate(
      brand      = gsub("^2023_sportbrand_", "", brand),
      proportion = dplyr::if_else(total_units > 0, units / total_units, 0)
    ) %>%
    dplyr::filter(response_id %in% ids_interes, units > 0)

  sig_tbl <- df_long %>%
    dplyr::filter(brand != "Adidas") %>%
    dplyr::group_by(response_id) %>%
    dplyr::summarise(
      signature = {
        br <- sort(unique(brand[units > 0]))
        if (length(br) == 0) "NONE" else paste(br, collapse = " + ")
      }, .groups = "drop"
    )

  df_long <- df_long %>%
    dplyr::left_join(sig_tbl, by = "response_id") %>%
    dplyr::filter(signature != "NONE")

  users_by_sig <- df_long %>%
    dplyr::distinct(response_id, signature) %>%
    dplyr::arrange(signature, response_id)

  panel_groups <- list()
  for (sig in unique(users_by_sig$signature)) {
    ids_sig <- users_by_sig %>% dplyr::filter(signature == sig) %>% dplyr::pull(response_id)
    if (length(ids_sig) == 0) next
    chunks <- split(ids_sig, ceiling(seq_along(ids_sig) / 3))
    for (ch in chunks) panel_groups <- append(panel_groups, list(list(ids = ch, signature = sig)))
  }

  for (i in seq_along(panel_groups)) {
    meta    <- panel_groups[[i]]
    df_plot <- df_long %>% dplyr::filter(response_id %in% meta$ids, units > 0)
    if (nrow(df_plot) == 0) next

    id_levels <- unique(meta$ids)
    id_labels <- setNames(paste0("Cliente ", seq_along(id_levels)), id_levels)
    df_plot   <- df_plot %>% dplyr::mutate(cliente_label = id_labels[response_id])

    p_sig <- ggplot2::ggplot(df_plot, ggplot2::aes(x = reorder(brand, -proportion), y = proportion, fill = brand)) +
      ggplot2::geom_col(width = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(scales::percent(proportion, accuracy = 1), " (", units, ")")),
                         hjust = -0.1, family = "times_new_roman", size = 3.8, color = "#000000") +
      ggplot2::scale_fill_manual(values = pal) +
      ggplot2::facet_wrap(~ cliente_label, scales = "free_x", nrow = 1) +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::labs(title = paste0("Clientes agrupados por firma de marca no-Adidas: ", meta$signature),
                    x = "Marca", y = "Proporcion de compra", fill = "Marca") +
      tema_grafica +
      ggplot2::theme(legend.position = "bottom")

    exportar_grafica(p_sig, sprintf("grafica_%02d_firma_marca_pagina_%02d", 8 + i, i), ancho = 10, alto = 6.5)
  }

} else {
  message("BLOQUE 27: no hay compradores de Adidas en TEST con marcas competidoras registradas; se omite el analisis de firmas de marca.")
}

# =============================================================================
# BLOQUES 28-30 [ORIGINAL] — TRASLADADOS A CODE3.R (no implementados aqui)
#
# Las siguientes piezas son preparacion ESPECIFICA del modelo POGIT/conteo
# y quedan fuera de code2.R, tal como lo establece la arquitectura de dos
# niveles acordada:
#   - as_numeric_matrix(), rank_report(), drop_allzero_constant_nzv()
#   - prep_V_lastcol_scale()            (estandarizacion de la ultima
#     columna de V, aplicada TRAIN->TEST)
#   - scale_W_cols_train_only()         (estandarizacion de W, TRAIN->TEST)
#   - select_W_from5_spearman()         (seleccion de variables de W por
#     correlacion de Spearman, cutoff 0.90)
#   - make_fullrank_VW()                (orquestador de rango completo)
#   - Adicion de columna Intercept a V_train/V_test/W_train/W_test
#   - c_train = Var(Y_amount_train) / Media(Y_amount_train)
#   - Y_count = round(Y_amount / c_train, 0)  (version discretizada de Y)
#   - Exportacion final a matrices_data.xlsx con matrices ya listas para
#     el modelo POGIT (V_train2, V_test2, W_train2, W_test2, Y_train,
#     Y_test)
# =============================================================================

# =============================================================================
# BLOQUE NUEVO — Exportacion de las bases crudas (AGREGADO)
# Estas son las matrices de la "construccion comun": V_raw, W_raw y
# Y_amount, TRAIN y TEST, SIN winsorizar, SIN estandarizar y SIN reducir
# columnas por correlacion. code3.R las tomara como punto de partida.
# =============================================================================

readr::write_csv(V_train, file.path(ruta_salidas, "V_train_raw.csv"))
readr::write_csv(V_test,  file.path(ruta_salidas, "V_test_raw.csv"))
readr::write_csv(W_train, file.path(ruta_salidas, "W_train_raw.csv"))
readr::write_csv(W_test,  file.path(ruta_salidas, "W_test_raw.csv"))

readr::write_csv(tibble::tibble(response_id = V_train$response_id, Y_amount = Y_amount_train),
                 file.path(ruta_salidas, "Y_amount_train.csv"))
readr::write_csv(tibble::tibble(response_id = V_test$response_id, Y_amount = Y_amount_test),
                 file.path(ruta_salidas, "Y_amount_test.csv"))

readr::write_csv(
  dplyr::bind_rows(
    tibble::tibble(response_id = train_ids, conjunto = "train"),
    tibble::tibble(response_id = test_ids,  conjunto = "test")
  ),
  file.path(ruta_salidas, "ids_train_test.csv")
)

message("\nPipeline comun (Script 2) completo - Adidas.")
message("  Bases crudas V/W/Y:       ", ruta_salidas)
message("  Graficas individuales:    ", ruta_graficas)
message("  Diagnosticos EDA:         diagnostico_outliers.csv, diagnostico_montos_severos.csv,")
message("                             diagnostico_missing_original.csv, diagnostico_missing.csv,")
message("                             diagnostico_correlacion.csv, diagnostico_vif.csv, resumen_variables.csv")
message("  Tablas de metricas (img): tabla_01_diagnostico_outliers, tabla_02_diagnostico_vif,")
message("                             tabla_03_resumen_variables (en graficas_individuales/)")
message("  SoW empirico:             empirical_sow.xlsx")
