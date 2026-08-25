# =============================================================================
# code3.R — PREPARACIÓN ESPECÍFICA PARA EL MODELO POGIT (ADIDAS)
#
# PROYECTO: Predicción SoW/SioW Adidas — Maestría
#
# ROL DE ESTE SCRIPT (arquitectura de dos niveles):
#   Script 2 (code2.R) = construcción "agnóstica de método de modelado":
#   V_raw, W_raw, Y_amount (monto real en dólares, SIN discretizar), y
#   diagnóstico exploratorio. NO aplica winsorización destructiva,
#   estandarización, selección de variables por Spearman, ni discretiza Y.
#
#   Script 3 (este archivo) = TRANSFORMA esas variables para que sean
#   compatibles con el modelo POGIT: winsorización APLICADA (Tukey k=3,
#   caps estimados SOLO con TRAIN), discretización de Y (c_train, Y_count),
#   diagnóstico de rango completo, selección de columnas de W por
#   correlación de Spearman, estandarización TRAIN->TEST, adición de
#   columna Intercept, y exportación final de las matrices listas para
#   el modelo (matrices_data.xlsx).
#
#   Este script NO estima el modelo POGIT (parámetros, log-verosimilitud,
#   coeficientes) ni calcula métricas de desempeño out-of-sample (MAE,
#   AIC/BIC, etc.). Esa pieza no existe en el material de referencia
#   disponible (ver nota de trazabilidad más abajo) y el usuario confirmó
#   explícitamente que la estimación/evaluación del modelo nunca se hizo
#   en el script original — por lo tanto NO se inventa aquí.
#
# BASE FUENTE / TRAZABILIDAD:
#   Todo lo que este script hace está tomado, bloque por bloque, del
#   archivo "code2_survey_purchase.R" (versión Apple, subido por el
#   usuario, 2175 líneas) — específicamente de la sección que en la
#   arquitectura Adidas quedó documentada en code2.R como "BLOQUES 28-30
#   [ORIGINAL] — TRASLADADOS A CODE3.R" (winsorización post-split,
#   construcción de c_train/Y_count, as_numeric_matrix/rank_report/
#   drop_allzero_constant_nzv/prep_V_lastcol_scale/scale_W_cols_train_only/
#   select_W_from5_spearman/make_fullrank_VW, adición de Intercept, y
#   exportación a matrices_data.xlsx). Cada bloque de abajo indica el
#   rango de líneas exacto de code2_survey_purchase.R del que proviene.
#
#   Las gráficas de validación del SoW empírico (histogramas con/sin
#   ceros, barras SoW=0 vs SoW>0, bubble plot SoW×SioW, firmas de marca)
#   YA fueron implementadas en code2.R de Adidas (BLOQUES 24, 26, 27),
#   en su versión continua (dólares, sin discretizar). code3.R NO las
#   repite — sería trabajo duplicado y no forma parte de lo que quedó
#   pendiente en "TRASLADADOS A CODE3.R". Ver BLOQUE 7 más abajo para la
#   única pieza relacionada que sí es nueva aquí: el SioW/SoW del test
#   set discretizado a la escala de conteos de c_train, para uso futuro
#   en validación del modelo (esto sí es una pieza distinta del original
#   que no está duplicada en code2.R).
#
#   ENTRADA: lee los CSV crudos que exporta code2.R (V_train_raw.csv,
#   V_test_raw.csv, W_train_raw.csv, W_test_raw.csv, Y_amount_train.csv,
#   Y_amount_test.csv) desde salidas/codigo_de_salida2/. Este script NO
#   modifica code1.R ni code2.R, ni sus salidas.
#
# NORMAS VISUALES: idénticas a code1.R y code2.R — Times New Roman real
# (fonts/times.ttf con fallback a Tinos + warning() explícito), tamaños
# título 19 / ejes 15 / texto 13, formato numérico es-CO en TODAS las
# gráficas ("." separador de miles, "," separador decimal), traducción
# completa al español, exportación individual PDF + SVG + PNG por
# gráfica (nunca un PDF combinado).
#
# TRAZABILIDAD: cada bloque indica su origen entre corchetes
# [BLOQUE ORIGINAL: líneas n-m de code2_survey_purchase.R]. Todo cambio
# de fondo (traducción, rediseño visual, decisión no evidente en el
# original) está marcado con el patrón:
#   # PROBLEMA DETECTADO -> EXPLICACIÓN -> PROPUESTA
# =============================================================================

# -----------------------------------------------------------------------------
# BLOQUE 1 — Carga de paquetes, rutas reproducibles, tipografía y tema
# [NUEVO — mismo mecanismo que code1.R / code2.R]
#
# PROBLEMA DETECTADO: el original ("code2_survey_purchase.R") no usa
# here::here() y no incluye tidymodels en uso real (solo se menciona en
# un comentario como "usada en code3.R", pero ningún código de ese
# archivo llega a usarla: no hay estimación de modelo en el material de
# referencia).
# EXPLICACIÓN: mantener el mismo mecanismo de rutas reproducibles que
# code1.R/code2.R evita el error de here::here() ya reportado por el
# usuario. tidymodels se omite del listado de paquetes porque no se usa
# en ninguna línea de este script (no se está estimando el POGIT aquí).
# PROPUESTA: here::here() para todas las rutas; paquetes limitados a los
# que efectivamente se usan (winsorización, selección de columnas,
# gráficas, exportación a Excel).
# -----------------------------------------------------------------------------

packages <- c(
  "dplyr","tidyr","stringr","tibble",
  "readr","ggplot2","scales","patchwork",
  "openxlsx","caret","here","AER"
)
invisible(lapply(packages, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  require(p, character.only = TRUE)
}))

# Dependencias para exportación de gráficas individuales PDF + SVG + PNG,
# igual que en code1.R / code2.R.
for (p in c("showtext","sysfonts","svglite","ragg")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(showtext)
library(sysfonts)
library(svglite)

# -----------------------------------------------------------------------------
# Rutas reproducibles (here::here)
# -----------------------------------------------------------------------------

ruta_entrada  <- here::here("salidas", "codigo_de_salida2")
ruta_salidas  <- here::here("salidas", "codigo_de_salida3")
ruta_graficas <- here::here("salidas", "codigo_de_salida3", "graficas_individuales")
ruta_fuente   <- here::here("fonts", "times.ttf")

dir.create(ruta_salidas,  showWarnings = FALSE, recursive = TRUE)
dir.create(ruta_graficas, showWarnings = FALSE, recursive = TRUE)

stopifnot(dir.exists(ruta_entrada))

# -----------------------------------------------------------------------------
# Tipografía Times New Roman real — MISMO MECANISMO que code1.R / code2.R
# -----------------------------------------------------------------------------

if (file.exists(ruta_fuente)) {
  sysfonts::font_add("times_new_roman", regular = ruta_fuente)
} else {
  warning(
    "No se encontro '", ruta_fuente, "'. Usando 'Tinos' (Google Fonts) como ",
    "sustituto temporal de Times New Roman en las graficas de code3.R. ",
    "Para usar la tipografia real, coloca el archivo times.ttf en la ",
    "carpeta 'fonts/' en la raiz del proyecto."
  )
  sysfonts::font_add_google("Tinos", "times_new_roman")
}
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# -----------------------------------------------------------------------------
# Tema y paleta estándar de gráficas — IDÉNTICOS a code1.R / code2.R
# (título 19, ejes 15, texto 13)
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

# Formateador de números es-CO reutilizable: "." miles, "," decimales.
# Ver correccion aplicada en code1.R (bar_plot): sprintf() SIEMPRE usa "."
# como decimal sin importar el locale, por eso se usa scales::label_number()
# de forma explicita en toda grafica que muestre numeros.
fmt_miles_es <- scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
fmt_dec_es   <- function(x, accuracy = 0.1) scales::label_number(accuracy = accuracy, decimal.mark = ",", big.mark = ".")(x)
fmt_pct_es   <- function(x, accuracy = 0.1) paste0(fmt_dec_es(100 * x, accuracy), "%")

# Helper de exportación: cada gráfica se guarda como PDF + SVG + PNG
# independientes, igual que en code1.R / code2.R (no un PDF combinado).
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
# Tablas de métricas/pruebas como gráfica — NUEVO, a petición del usuario:
# las tablas compactas de indicadores/pruebas (caps de winsorización, rango
# antes/después de selección, columnas eliminadas de W, prueba de
# Cameron y Trivedi) se exportan ADEMÁS del CSV como imagen PDF+SVG+PNG
# con la MISMA tipografía y tamaños que las gráficas (Times New Roman,
# título 19, encabezado ~15, celdas ~13), en estilo "booktabs" (solo
# líneas horizontales), lista para pegar en el documento de tesis. No
# formatea números por sí misma: los valores deben llegar ya formateados
# como texto (usar fmt_miles_es/fmt_dec_es/fmt_pct_es antes de llamarla).
# -----------------------------------------------------------------------------

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
# BLOQUE 2 — Helpers de winsorización
# [BLOQUE ORIGINAL: líneas 81-128 de code2_survey_purchase.R — CONSERVADO]
#
# Idénticas a las de code2.R de Adidas (donde se usaban solo con fines de
# diagnóstico). Aquí SÍ se usan para aplicar la winsorización real, tal
# como el original lo hacía después del split train/test.
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
# BLOQUE 3 — Carga de V_raw / W_raw / Y_amount (TRAIN/TEST) desde code2.R
# [NUEVO — code3.R es independiente, no depende del entorno de code2.R]
#
# PROBLEMA DETECTADO: si code3.R reutilizara objetos en memoria de una
# sesión donde ya se corrió code2.R, no sería reproducible por sí solo.
# EXPLICACIÓN: para que code3.R pueda ejecutarse de forma independiente
# (igual que code2.R lee survey_es.csv en vez de depender de code1.R en
# memoria), debe leer los CSV que code2.R ya exporta.
# PROPUESTA: leer V_train_raw.csv, V_test_raw.csv, W_train_raw.csv,
# W_test_raw.csv, Y_amount_train.csv, Y_amount_test.csv desde
# salidas/codigo_de_salida2/, y verificar alineación de IDs antes de
# continuar.
# -----------------------------------------------------------------------------

V_train <- readr::read_csv(file.path(ruta_entrada, "V_train_raw.csv"), show_col_types = FALSE) %>%
  dplyr::mutate(response_id = as.character(response_id))
V_test  <- readr::read_csv(file.path(ruta_entrada, "V_test_raw.csv"),  show_col_types = FALSE) %>%
  dplyr::mutate(response_id = as.character(response_id))
W_train <- readr::read_csv(file.path(ruta_entrada, "W_train_raw.csv"), show_col_types = FALSE) %>%
  dplyr::mutate(response_id = as.character(response_id))
W_test  <- readr::read_csv(file.path(ruta_entrada, "W_test_raw.csv"),  show_col_types = FALSE) %>%
  dplyr::mutate(response_id = as.character(response_id))
Y_amount_train_df <- readr::read_csv(file.path(ruta_entrada, "Y_amount_train.csv"), show_col_types = FALSE) %>%
  dplyr::mutate(response_id = as.character(response_id))
Y_amount_test_df  <- readr::read_csv(file.path(ruta_entrada, "Y_amount_test.csv"),  show_col_types = FALSE) %>%
  dplyr::mutate(response_id = as.character(response_id))

# Verificación de alineación: V, W y Y_amount deben tener exactamente los
# mismos response_id, en el mismo orden, dentro de cada conjunto (TRAIN/TEST).
# Si code2.R exportó todo correctamente, esto ya debería cumplirse; se
# reordena por response_id como salvaguarda no destructiva antes de stopifnot.
alinear_por_id <- function(df, ids_ref, id_col = "response_id") {
  df[[id_col]] <- as.character(df[[id_col]])
  df[match(ids_ref, df[[id_col]]), , drop = FALSE]
}

ids_train <- V_train$response_id
ids_test  <- V_test$response_id

W_train           <- alinear_por_id(W_train,           ids_train)
Y_amount_train_df <- alinear_por_id(Y_amount_train_df,  ids_train)
W_test            <- alinear_por_id(W_test,             ids_test)
Y_amount_test_df  <- alinear_por_id(Y_amount_test_df,   ids_test)

stopifnot(identical(V_train$response_id, W_train$response_id))
stopifnot(identical(V_train$response_id, Y_amount_train_df$response_id))
stopifnot(identical(V_test$response_id,  W_test$response_id))
stopifnot(identical(V_test$response_id,  Y_amount_test_df$response_id))
stopifnot(!any(is.na(V_train$response_id)), !any(is.na(V_test$response_id)))

Y_amount_train <- Y_amount_train_df$Y_amount
Y_amount_test  <- Y_amount_test_df$Y_amount

message("code3.R — cargado desde code2.R: train=", nrow(V_train), " | test=", nrow(V_test))

# -----------------------------------------------------------------------------
# BLOQUE 4 — Winsorización APLICADA post-split (Tukey k=3, caps SOLO TRAIN)
# [BLOQUE ORIGINAL: líneas 1049-1102 de code2_survey_purchase.R — CONSERVADO]
#
# A diferencia de code2.R (donde esto era solo diagnóstico), aquí SÍ se
# aplica: los caps se calculan EXCLUSIVAMENTE con TRAIN y se aplican a
# TEST, para no filtrar información de test hacia el preprocesamiento
# (misma regla de "no data leakage" del original, líneas 15-20).
#
# Tres sub-bloques, igual que el original:
#   (a) Cap de Y_amount
#   (b) Cap de todas las columnas numéricas de W
#   (c) Cap de n_states en V (única columna continua; las dummies 0/1 no
#       se ven afectadas por construcción)
# -----------------------------------------------------------------------------

# (a) Cap de Y_amount estimado en TRAIN
tk_y_tr <- tukey_cap_upper(Y_amount_train, k = 3)
cap_y_train <- tk_y_tr$cap
message("Cap Tukey (k=3) TRAIN para Y_amount: ", round(cap_y_train, 4))

Y_amount_train_wins <- tk_y_tr$x
Y_amount_test_wins  <- apply_upper_cap(Y_amount_test, cap_y_train)

# (b) Cap de W numéricas estimado en TRAIN
W_wins <- winsorize_df_numeric_upper_trainonly(
  df_train = W_train, df_test = W_test, id_col = "response_id", k = 3, cols = NULL
)
W_train_wins <- W_wins$train
W_test_wins  <- W_wins$test

# (c) Cap de n_states en V estimado en TRAIN
V_train_wins <- V_train
V_test_wins  <- V_test
cap_ns_train <- NA_real_
if ("n_states" %in% names(V_train) && is.numeric(V_train$n_states)) {
  tk_ns_tr <- tukey_cap_upper(V_train$n_states, k = 3)
  cap_ns_train <- tk_ns_tr$cap
  message("Cap Tukey (k=3) TRAIN para n_states: ", round(cap_ns_train, 4))
  V_train_wins$n_states <- tk_ns_tr$x
  V_test_wins$n_states  <- apply_upper_cap(V_test$n_states, cap_ns_train)
}

# Diagnóstico exportable de todos los caps aplicados (Y_amount, n_states,
# y cada columna numérica de W), para auditoría.
diag_caps <- dplyr::bind_rows(
  tibble::tibble(variable = "Y_amount", cap_tukey_k3 = cap_y_train),
  if (!is.na(cap_ns_train)) tibble::tibble(variable = "V_raw: n_states", cap_tukey_k3 = cap_ns_train) else NULL,
  tibble::tibble(
    variable     = paste0("W_raw: ", names(W_wins$caps)),
    cap_tukey_k3 = unlist(W_wins$caps, use.names = FALSE)
  )
)
readr::write_csv(diag_caps, file.path(ruta_salidas, "diagnostico_caps_winsorizacion.csv"))

tabla_caps <- diag_caps %>%
  dplyr::transmute(Variable = variable, `Cap Tukey (k=3), solo TRAIN` = fmt_dec_es(cap_tukey_k3, 2))
graficar_tabla_metrica(
  tabla_caps,
  titulo = "Caps de winsorizacion aplicados (Tukey, k=3) - Adidas",
  subtitulo = "Caps estimados exclusivamente con TRAIN, aplicados a TEST (sin fuga de datos)",
  nombre_archivo = "tabla_01_caps_winsorizacion"
)

# -----------------------------------------------------------------------------
# BLOQUE 5 — Construcción de Y discretizada: c_train, Y_train, Y_test
# [BLOQUE ORIGINAL: líneas 1106-1144 de code2_survey_purchase.R — CONSERVADO]
#
# Y = round(monto_winsorizado / c_train), donde
#   c_train = Var(Y_amount_train_wins) / Media(Y_amount_train_wins)
# (estimador de momentos para la constante de escala, calculado SOLO con
# TRAIN, ya winsorizado; se aplica la misma constante a TEST).
# pmax(0L, ...) garantiza no negatividad; as.integer() para las
# distribuciones discretas del modelo POGIT.
# -----------------------------------------------------------------------------

mean_train <- mean(Y_amount_train_wins, na.rm = TRUE)
var_train_ <- stats::var(Y_amount_train_wins, na.rm = TRUE)

c_train <- round(var_train_ / mean_train, 0)
if (!is.finite(c_train) || c_train <= 0) {
  stop("c_train no es valido (", c_train, "). Revisar media/varianza de Y_amount en TRAIN.")
}

Y_train <- pmax(0L, as.integer(round(Y_amount_train_wins / c_train, 0)))
Y_test  <- pmax(0L, as.integer(round(Y_amount_test_wins  / c_train, 0)))

stopifnot(all(Y_train >= 0), all(Y_test >= 0))
stopifnot(length(Y_train) == nrow(V_train_wins))
stopifnot(length(Y_test)  == nrow(V_test_wins))

message("c_train = ", c_train,
        " | Y_train: n=", length(Y_train), " media=", round(mean(Y_train), 3),
        " | Y_test: n=", length(Y_test),  " media=", round(mean(Y_test), 3))

# -----------------------------------------------------------------------------
# BLOQUE 6 — Gráficas de distribución de Y (Y_train, Y_test)
# [BLOQUE ORIGINAL: líneas 1150-1211 de code2_survey_purchase.R — TRADUCIDO
# Y REDISEÑADO]
#
# PROBLEMA DETECTADO: el original dibuja estas gráficas con barplot() base
# R, en inglés ("Training set" / "Test set", ejes "Y" / "Frequency"),
# paleta de grises fija, sin tipografía Times New Roman, y sin exportación
# individual PDF+SVG+PNG.
# EXPLICACIÓN: el usuario exigió explícitamente que TODAS las gráficas de
# code3.R sigan las mismas normas visuales de code1.R (tamaño, tipografía,
# formato de números es-CO, traducción al español).
# PROPUESTA: reconstruir esta gráfica en ggplot2, con el mismo tema_grafica
# de code1.R/code2.R, títulos y ejes en español, formato es-CO en las
# etiquetas de frecuencia, y exportación individual PDF+SVG+PNG. La lógica
# de fondo (tabular Y por valor entero y graficar frecuencias) es idéntica
# al original; solo cambia la implementación visual.
# -----------------------------------------------------------------------------

graficar_distribucion_Y_discreta <- function(vec, titulo, subtitulo, nombre_archivo) {
  conteos <- tibble::tibble(valor_y = vec) %>%
    dplyr::count(valor_y, name = "n") %>%
    dplyr::arrange(valor_y) %>%
    dplyr::mutate(valor_y_chr = factor(valor_y, levels = sort(unique(valor_y))))

  ggplot2::ggplot(conteos, ggplot2::aes(x = valor_y_chr, y = n)) +
    ggplot2::geom_col(fill = pal[1], color = "white", width = 0.75) +
    ggplot2::geom_text(
      ggplot2::aes(label = fmt_miles_es(n)),
      vjust = -0.4, size = 4, colour = "grey20", family = "times_new_roman"
    ) +
    ggplot2::scale_y_continuous(
      labels = fmt_miles_es,
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(
      title = titulo, subtitle = subtitulo,
      x = "Y (conteo discretizado, unidades de c_train)",
      y = "Frecuencia absoluta"
    ) +
    tema_grafica
}

subtitulo_c <- paste0("c_train = ", fmt_miles_es(c_train),
                      " | Division 80/20 con set.seed(123) (heredado de code2.R)")

p_y_train <- graficar_distribucion_Y_discreta(
  Y_train,
  "Distribucion de Y - Conjunto de entrenamiento (Adidas)",
  subtitulo_c,
  "grafica_01_distribucion_Y_entrenamiento"
)
exportar_grafica(p_y_train, "grafica_01_distribucion_Y_entrenamiento", ancho = 10, alto = 6.5)

p_y_test <- graficar_distribucion_Y_discreta(
  Y_test,
  "Distribucion de Y - Conjunto de prueba (Adidas)",
  subtitulo_c,
  "grafica_02_distribucion_Y_prueba"
)
exportar_grafica(p_y_test, "grafica_02_distribucion_Y_prueba", ancho = 10, alto = 6.5)

# -----------------------------------------------------------------------------
# BLOQUE 7 — SioW/SoW discretizados (escala de conteos) para el TEST set
# [BLOQUE ORIGINAL: líneas 1214-1298 de code2_survey_purchase.R — CONSERVADO
# (adaptado a Adidas), SIN duplicar graficas ya hechas en code2.R]
#
# PROBLEMA DETECTADO: el original calcula, ademas del SoW continuo, una
# version discretizada de SioW en la MISMA escala de conteos que Y
# (dividiendo por c_train), especificamente para poder comparar mas
# adelante las predicciones del modelo POGIT (que trabaja en conteos)
# contra un SioW/SoW observado, sin tener que reconvertir unidades.
# EXPLICACION: code2.R de Adidas ya exporta un SoW/SioW/PoW CONTINUO (en
# dolares) en empirical_sow.xlsx, pero esa version no esta en la escala de
# conteos que usa el modelo POGIT (Y_train/Y_test). Como code3.R es quien
# calcula c_train, esta es la unica etapa del pipeline donde se puede
# construir la version discretizada.
# PROPUESTA: recalcular sobre el TEST set (mismo universo ids_sow_test que
# usaba el original) un SioW_count = round(amount_total/c_train) y un
# SoW_test (identico en definicion al SoW continuo, pero re-derivado aqui
# para exportarlo junto con SioW_count), y exportarlos a un archivo Excel
# SEPARADO (empirical_sow_discretizado.xlsx) sin tocar el
# empirical_sow.xlsx continuo que ya genera code2.R. No se generan graficas
# nuevas aqui: serian practicamente identicas (en forma) a las graficas 04-07
# de code2.R, que ya existen en escala continua.
# -----------------------------------------------------------------------------

ruta_purchases <- here::here("data", "survey-data", "amazon-purchases.csv")
stopifnot(file.exists(ruta_purchases))

# Se recarga el universo de compras Adidas vs. otras marcas en T2, con la
# MISMA logica de clasificacion de code2.R (BLOQUES 4-8), unicamente para
# poder recomponer amount_adidas / amount_otherbrands sobre ids_sow_test.
# Esto se hace por separado (no se reimporta code2.R) para que code3.R siga
# siendo ejecutable de forma independiente, igual que code2.R lo es de code1.R.

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

classify_category <- function(cat) {
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
ADIDAS_PATTERN <- "\\badidas\\b"
purchases_cat <- purchases_cat %>%
  dplyr::mutate(is_adidas = stringr::str_detect(title_lc, stringr::regex(ADIDAS_PATTERN, ignore_case = TRUE)))

ref_date3 <- lubridate::ymd("2021-11-01")
ref_date4 <- max(purchases_cat$order_date[purchases_cat$is_adidas], na.rm = TRUE)

# PROBLEMA DETECTADO -> EXPLICACION -> PROPUESTA: igual que en code2.R
# (BLOQUE 23), Y_amount/c_train se calculan sobre TODAS las categorias de
# Adidas, mientras que este SioW/SoW discretizado, para ser consistente con
# el SoW continuo ya calculado y documentado en code2.R, se restringe aqui
# tambien a broad_cat %in% c("Apparel","Sports_Outdoors"). Esta es la MISMA
# discrepancia de universos ya documentada y pendiente de decision en
# code2.R; no se resuelve aqui, solo se hereda la misma definicion usada
# alli para no introducir una tercera definicion distinta sin evidencia.

ids_sow_test <- sort(as.character(V_test$response_id))
stopifnot(length(ids_sow_test) > 0)

period_df3 <- purchases_cat %>%
  dplyr::filter(
    dplyr::between(order_date, ref_date3, ref_date4),
    response_id %in% ids_sow_test,
    broad_cat %in% c("Apparel", "Sports_Outdoors")
  )

adidas_vs_others_discreto <- period_df3 %>%
  dplyr::group_by(response_id) %>%
  dplyr::summarise(
    amount_adidas      = sum(dplyr::if_else(is_adidas == TRUE,  amount, 0), na.rm = TRUE),
    amount_otherbrands = sum(dplyr::if_else(is_adidas == FALSE, amount, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::right_join(tibble::tibble(response_id = ids_sow_test), by = "response_id") %>%
  dplyr::mutate(
    amount_adidas      = tidyr::replace_na(amount_adidas, 0),
    amount_otherbrands = tidyr::replace_na(amount_otherbrands, 0),
    amount_total       = amount_adidas + amount_otherbrands,
    SioW_count          = round(amount_total / c_train, 0),
    SoW_test            = dplyr::if_else(SioW_count == 0, 0, amount_adidas / amount_total)
  ) %>%
  dplyr::select(response_id, amount_adidas, amount_otherbrands, amount_total, SioW_count, SoW_test)

adidas_vs_others_discreto <- adidas_vs_others_discreto[match(ids_sow_test, adidas_vs_others_discreto$response_id), , drop = FALSE]
stopifnot(nrow(adidas_vs_others_discreto) == length(ids_sow_test))

wb_sow_disc <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_sow_disc, "sow_siow_discretizado")
openxlsx::writeData(wb_sow_disc, "sow_siow_discretizado", as.data.frame(adidas_vs_others_discreto))
openxlsx::saveWorkbook(wb_sow_disc, file = file.path(ruta_salidas, "empirical_sow_discretizado.xlsx"), overwrite = TRUE)

# -----------------------------------------------------------------------------
# BLOQUE 8 — Diagnóstico de rango completo ANTES de la selección de columnas
# [BLOQUE ORIGINAL: líneas 1742-1796 de code2_survey_purchase.R — CONSERVADO]
# -----------------------------------------------------------------------------

as_numeric_matrix <- function(df, id_col = "response_id") {
  stopifnot(id_col %in% names(df))
  X <- df %>% dplyr::select(-dplyr::all_of(id_col))
  X <- X %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))
  as.matrix(X)
}

rank_report <- function(df, id_col = "response_id", tol = 1e-10) {
  X <- as_numeric_matrix(df, id_col)
  X[is.na(X)] <- 0
  q <- qr(X, tol = tol)
  r <- q$rank
  list(
    n = nrow(X), p = ncol(X),
    rank = r,
    full_col_rank = (r == ncol(X)),
    deficiency = ncol(X) - r,
    kappa = tryCatch(kappa(X), error = function(e) NA_real_)
  )
}

rank_report_to_row <- function(rep_list, etiqueta) {
  tibble::tibble(
    matriz = etiqueta, n = rep_list$n, p = rep_list$p, rango = rep_list$rank,
    rango_completo = rep_list$full_col_rank, deficiencia = rep_list$deficiency,
    numero_condicion_kappa = round(rep_list$kappa, 3)
  )
}

diag_rango_antes <- dplyr::bind_rows(
  rank_report_to_row(rank_report(V_train_wins), "V_train (antes de seleccion)"),
  rank_report_to_row(rank_report(V_test_wins),  "V_test (antes de seleccion)"),
  rank_report_to_row(rank_report(W_train_wins), "W_train (antes de seleccion)"),
  rank_report_to_row(rank_report(W_test_wins),  "W_test (antes de seleccion)")
)
readr::write_csv(diag_rango_antes, file.path(ruta_salidas, "diagnostico_rango_antes_seleccion.csv"))

tabla_rango_antes <- diag_rango_antes %>%
  dplyr::transmute(
    Matriz              = matriz,
    n                   = fmt_miles_es(n),
    p                   = fmt_miles_es(p),
    Rango               = fmt_miles_es(rango),
    `Rango completo`    = dplyr::if_else(rango_completo, "Si", "No"),
    Deficiencia         = fmt_miles_es(deficiencia),
    `Numero de condicion (kappa)` = fmt_dec_es(numero_condicion_kappa, 2)
  )
graficar_tabla_metrica(
  tabla_rango_antes,
  titulo = "Diagnostico de rango completo - antes de seleccion de columnas (Adidas)",
  subtitulo = "V/W winsorizadas, sin limpieza NZV ni seleccion por correlacion de Spearman",
  nombre_archivo = "tabla_02_rango_antes_seleccion"
)

# -----------------------------------------------------------------------------
# BLOQUE 9 — Selección de columnas y estandarización TRAIN->TEST
# [BLOQUE ORIGINAL: líneas 1798-2046 de code2_survey_purchase.R — CONSERVADO]
#
# drop_allzero_constant_nzv(): elimina columnas all-zero, constantes y de
# varianza casi nula (caret::nearZeroVar()).
# prep_V_lastcol_scale(): estandariza SOLO la ultima columna continua de V
# (n_states), sin eliminar ninguna dummy.
# scale_W_cols_train_only(): estandariza columnas de W con media/sd de TRAIN.
# select_W_from5_spearman(): protege las primeras 4 columnas de W (RFM +
# una mas, tal como en el original — ver comentario original: "columnas
# 1-4 (RFM: recency, frequency, monetary + una mas) son FIJAS"), limpia
# NZV en las candidatas restantes, y elimina redundancias por correlacion
# de Spearman (cutoff 0.90) usando caret::findCorrelation().
# make_fullrank_VW(): orquesta todo lo anterior y reporta rango final.
# -----------------------------------------------------------------------------

drop_allzero_constant_nzv <- function(df, id_col = "response_id", nzv = TRUE) {
  X <- df %>% dplyr::select(-dplyr::all_of(id_col))
  X <- X %>% dplyr::select(where(is.numeric))
  X0 <- X; X0[is.na(X0)] <- 0

  all_zero <- names(X0)[vapply(X0, function(z) all(z == 0), logical(1))]
  constant <- names(X0)[vapply(X0, function(z) length(unique(z)) == 1, logical(1))]

  keep <- setdiff(names(X0), union(all_zero, constant))
  X1 <- X0[, keep, drop = FALSE]

  nzv_cols <- character(0)
  if (nzv && ncol(X1) > 0) {
    nzv_idx <- caret::nearZeroVar(X1)
    if (length(nzv_idx) > 0) nzv_cols <- colnames(X1)[nzv_idx]
    keep2 <- setdiff(colnames(X1), nzv_cols)
    X1 <- X1[, keep2, drop = FALSE]
  }

  list(
    kept = colnames(X1),
    removed = list(
      all_zero = all_zero,
      constant = setdiff(constant, all_zero),
      nzv = nzv_cols
    )
  )
}

scale_W_cols_train_only <- function(W_train, W_test, cols_to_scale, id_col = "response_id") {
  stopifnot(id_col %in% names(W_train), id_col %in% names(W_test))
  cols_to_scale <- intersect(cols_to_scale, setdiff(names(W_train), id_col))

  if (length(cols_to_scale) == 0) {
    return(list(W_train_sc = W_train, W_test_sc = W_test,
                mu = numeric(0), sd = numeric(0), scaled_cols = character(0)))
  }

  Wtr <- W_train; Wte <- W_test
  Wtr[cols_to_scale] <- lapply(Wtr[cols_to_scale], function(x) suppressWarnings(as.numeric(x)))
  Wte[cols_to_scale] <- lapply(Wte[cols_to_scale], function(x) suppressWarnings(as.numeric(x)))

  mu  <- vapply(Wtr[cols_to_scale], function(x) mean(x, na.rm = TRUE), numeric(1))
  sdv <- vapply(Wtr[cols_to_scale], function(x) stats::sd(x, na.rm = TRUE), numeric(1))

  ok <- is.finite(sdv) & sdv > 0
  scaled_cols <- cols_to_scale[ok]

  for (cc in scaled_cols) {
    Wtr[[cc]] <- (Wtr[[cc]] - mu[cc]) / sdv[cc]
    Wte[[cc]] <- (Wte[[cc]] - mu[cc]) / sdv[cc]
  }

  list(W_train_sc = Wtr, W_test_sc = Wte, mu = mu, sd = sdv, scaled_cols = scaled_cols)
}

select_W_from5_spearman <- function(W_train, W_test,
                                    id_col = "response_id",
                                    keep_first_k = 4,
                                    cutoff = 0.90,
                                    nzv = TRUE,
                                    scale_selected = TRUE) {
  stopifnot(id_col %in% names(W_train), id_col %in% names(W_test))

  feats <- setdiff(names(W_train), id_col)
  stopifnot(setequal(feats, setdiff(names(W_test), id_col)))

  # PROBLEMA DETECTADO -> EXPLICACION -> PROPUESTA: el original protege las
  # primeras 4 columnas de W como "fijas", aunque solo 3 son RFM en sentido
  # estricto (recency, frequency, monetary); la 4a queda fija por posicion,
  # no por definicion metodologica explicita ("+ una mas", segun el propio
  # comentario del original). Se conserva identico aqui (keep_first_k = 4)
  # porque asi esta evidenciado en el material de referencia, sin agregar
  # una regla nueva no evidenciada.
  fixed <- feats[seq_len(min(keep_first_k, length(feats)))]
  cand  <- setdiff(feats, fixed)

  cand_df  <- W_train %>% dplyr::select(all_of(id_col), all_of(cand))
  keep_inf <- drop_allzero_constant_nzv(cand_df, id_col = id_col, nzv = nzv)
  cand_kept <- keep_inf$kept

  if (length(cand_kept) <= 1) {
    selected <- c(fixed, cand_kept)
    Wtr_sel <- W_train %>% dplyr::select(all_of(id_col), all_of(selected))
    Wte_sel <- W_test  %>% dplyr::select(all_of(id_col), all_of(selected))
    sc <- if (scale_selected) scale_W_cols_train_only(Wtr_sel, Wte_sel, selected, id_col) else
      list(W_train_sc = Wtr_sel, W_test_sc = Wte_sel, mu = numeric(0), sd = numeric(0), scaled_cols = character(0))
    return(list(
      W_train_sel = sc$W_train_sc, W_test_sel = sc$W_test_sc,
      fixed = fixed, kept_from5 = cand_kept, dropped_bad = keep_inf$removed,
      dropped_corr = character(0), scaled_cols = sc$scaled_cols,
      scale_params = list(mu = sc$mu, sd = sc$sd)
    ))
  }

  X <- W_train %>%
    dplyr::select(all_of(cand_kept)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
    as.matrix()
  X[is.na(X)] <- 0

  cor_mat <- suppressWarnings(stats::cor(X, method = "spearman", use = "pairwise.complete.obs"))
  diag(cor_mat) <- 1

  drop_idx <- caret::findCorrelation(cor_mat, cutoff = cutoff, names = FALSE, exact = TRUE)
  dropped_corr <- if (length(drop_idx) > 0) colnames(cor_mat)[drop_idx] else character(0)

  kept_from5 <- setdiff(cand_kept, dropped_corr)
  selected <- c(fixed, kept_from5)

  Wtr_sel <- W_train %>% dplyr::select(all_of(id_col), all_of(selected))
  Wte_sel <- W_test  %>% dplyr::select(all_of(id_col), all_of(selected))

  sc <- if (scale_selected) scale_W_cols_train_only(Wtr_sel, Wte_sel, selected, id_col) else
    list(W_train_sc = Wtr_sel, W_test_sc = Wte_sel, mu = numeric(0), sd = numeric(0), scaled_cols = character(0))

  list(
    W_train_sel = sc$W_train_sc, W_test_sel = sc$W_test_sc,
    fixed = fixed, kept_from5 = kept_from5, dropped_bad = keep_inf$removed,
    dropped_corr = dropped_corr, cutoff = cutoff, scaled_cols = sc$scaled_cols,
    scale_params = list(mu = sc$mu, sd = sc$sd)
  )
}

make_fullrank_VW <- function(V_train, V_test, W_train, W_test,
                             id_col = "response_id", V_last_col = NULL,
                             W_keep_first_k = 4, W_cutoff = 0.90,
                             nzv = TRUE, scale_W = TRUE) {

  stopifnot(identical(as.character(V_train[[id_col]]), as.character(W_train[[id_col]])))
  stopifnot(identical(as.character(V_test[[id_col]]),  as.character(W_test[[id_col]])))

  Vtr <- V_train; Vte <- V_test
  feats_V <- setdiff(names(Vtr), id_col)
  if (is.null(V_last_col)) V_last_col <- tail(feats_V, 1)
  stopifnot(V_last_col %in% names(Vtr))

  mu_v <- NA_real_; sd_v <- NA_real_
  if (is.numeric(Vtr[[V_last_col]])) {
    mu_v <- mean(Vtr[[V_last_col]], na.rm = TRUE)
    sd_v <- stats::sd(Vtr[[V_last_col]], na.rm = TRUE)
    if (is.finite(sd_v) && sd_v > 0) {
      Vtr[[V_last_col]] <- (Vtr[[V_last_col]] - mu_v) / sd_v
      Vte[[V_last_col]] <- (Vte[[V_last_col]] - mu_v) / sd_v
    } else {
      warning("V_last_col tiene sd=0 en TRAIN; no se estandariza.")
    }
  } else {
    warning("V_last_col no es numerica; no se estandariza.")
  }

  Wres <- select_W_from5_spearman(
    W_train, W_test, id_col = id_col, keep_first_k = W_keep_first_k,
    cutoff = W_cutoff, nzv = nzv, scale_selected = scale_W
  )

  rep_final <- list(
    V_train = rank_report(Vtr, id_col), V_test = rank_report(Vte, id_col),
    W_train = rank_report(Wres$W_train_sel, id_col), W_test = rank_report(Wres$W_test_sel, id_col)
  )

  list(
    V_train2 = Vtr, V_test2 = Vte,
    W_train2 = Wres$W_train_sel, W_test2 = Wres$W_test_sel,
    report_rank = rep_final,
    V_last_col = V_last_col, V_scale_params = list(mu = mu_v, sd = sd_v),
    removed_W_bad = Wres$dropped_bad, dropped_W_corr = Wres$dropped_corr,
    kept_W_from5 = Wres$kept_from5, fixed_W_1to4 = Wres$fixed,
    W_scaled_cols = Wres$scaled_cols, W_scale_params = Wres$scale_params
  )
}

# -----------------------------------------------------------------------------
# BLOQUE 10 — Ejecución de make_fullrank_VW + diagnóstico de columnas
# eliminadas y rango final
# [BLOQUE ORIGINAL: líneas 2050-2092 de code2_survey_purchase.R — CONSERVADO]
# -----------------------------------------------------------------------------

res <- make_fullrank_VW(
  V_train = V_train_wins, V_test = V_test_wins,
  W_train = W_train_wins, W_test = W_test_wins,
  id_col = "response_id",
  V_last_col     = if ("n_states" %in% names(V_train_wins)) "n_states" else NULL,
  W_keep_first_k = 4, W_cutoff = 0.90, nzv = TRUE, scale_W = TRUE
)

V_train2 <- res$V_train2
V_test2  <- res$V_test2
W_train2 <- res$W_train2
W_test2  <- res$W_test2

message("V_train full col rank: ", res$report_rank$V_train$full_col_rank,
        " | deficiencia: ", res$report_rank$V_train$deficiency)
message("W_train full col rank: ", res$report_rank$W_train$full_col_rank,
        " | deficiencia: ", res$report_rank$W_train$deficiency)
message("Columnas de W estandarizadas (TRAIN->TEST): ", length(res$W_scaled_cols))

diag_rango_despues <- dplyr::bind_rows(
  rank_report_to_row(res$report_rank$V_train, "V_train2 (despues de seleccion)"),
  rank_report_to_row(res$report_rank$V_test,  "V_test2 (despues de seleccion)"),
  rank_report_to_row(res$report_rank$W_train, "W_train2 (despues de seleccion)"),
  rank_report_to_row(res$report_rank$W_test,  "W_test2 (despues de seleccion)")
)
readr::write_csv(diag_rango_despues, file.path(ruta_salidas, "diagnostico_rango_despues_seleccion.csv"))

tabla_rango_despues <- diag_rango_despues %>%
  dplyr::transmute(
    Matriz              = matriz,
    n                   = fmt_miles_es(n),
    p                   = fmt_miles_es(p),
    Rango               = fmt_miles_es(rango),
    `Rango completo`    = dplyr::if_else(rango_completo, "Si", "No"),
    Deficiencia         = fmt_miles_es(deficiencia),
    `Numero de condicion (kappa)` = fmt_dec_es(numero_condicion_kappa, 2)
  )
graficar_tabla_metrica(
  tabla_rango_despues,
  titulo = "Diagnostico de rango completo - despues de seleccion de columnas (Adidas)",
  subtitulo = "V: solo n_states estandarizado. W: RFM fijas + seleccion Spearman (cutoff 0.90), estandarizada",
  nombre_archivo = "tabla_03_rango_despues_seleccion"
)

diag_columnas_w <- dplyr::bind_rows(
  if (length(res$removed_W_bad$all_zero) > 0) tibble::tibble(columna = res$removed_W_bad$all_zero, motivo = "all_zero") else NULL,
  if (length(res$removed_W_bad$constant) > 0) tibble::tibble(columna = res$removed_W_bad$constant, motivo = "constante") else NULL,
  if (length(res$removed_W_bad$nzv)      > 0) tibble::tibble(columna = res$removed_W_bad$nzv,      motivo = "near_zero_variance") else NULL,
  if (length(res$dropped_W_corr)         > 0) tibble::tibble(columna = res$dropped_W_corr,          motivo = paste0("correlacion_spearman > ", 0.90)) else NULL
)
if (nrow(diag_columnas_w) == 0) diag_columnas_w <- tibble::tibble(columna = character(0), motivo = character(0))
readr::write_csv(diag_columnas_w, file.path(ruta_salidas, "diagnostico_columnas_eliminadas_W.csv"))

if (nrow(diag_columnas_w) > 0) {
  tabla_columnas_w <- diag_columnas_w %>% dplyr::transmute(Columna = columna, `Motivo de eliminacion` = motivo)
} else {
  tabla_columnas_w <- tibble::tibble(Columna = "(ninguna)", `Motivo de eliminacion` = "No se elimino ninguna columna de W en esta etapa")
}
graficar_tabla_metrica(
  tabla_columnas_w,
  titulo = "Columnas de W eliminadas antes de estimar el POGIT (Adidas)",
  subtitulo = "All-zero, constantes, near-zero-variance, o correlacion de Spearman > 0.90 (solo TRAIN)",
  nombre_archivo = "tabla_04_columnas_eliminadas_W"
)

# -----------------------------------------------------------------------------
# BLOQUE 11 — Matrices finales con Intercepto + exportación a matrices_data.xlsx
# [BLOQUE ORIGINAL: líneas 2095-2175 de code2_survey_purchase.R — CONSERVADO]
# -----------------------------------------------------------------------------

V_train_mat <- V_train2 %>% dplyr::select(-response_id)
V_test_mat  <- V_test2  %>% dplyr::select(-response_id)
W_train_mat <- W_train2 %>% dplyr::select(-response_id)
W_test_mat  <- W_test2  %>% dplyr::select(-response_id)

to_num_mat <- function(df) {
  out <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))
  out %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0)))
}

V_train_mat <- to_num_mat(V_train_mat)
V_test_mat  <- to_num_mat(V_test_mat)
W_train_mat <- to_num_mat(W_train_mat)
W_test_mat  <- to_num_mat(W_test_mat)

add_intercept <- function(df, name = "Intercept") {
  df <- dplyr::relocate(df, dplyr::everything())
  cbind(setNames(data.frame(rep(1, nrow(df))), name), df)
}

V_train_mat <- add_intercept(V_train_mat, "Intercept")
V_test_mat  <- add_intercept(V_test_mat,  "Intercept")
W_train_mat <- add_intercept(W_train_mat, "Intercept")
W_test_mat  <- add_intercept(W_test_mat,  "Intercept")

stopifnot(ncol(V_train_mat) == ncol(V_test_mat))
stopifnot(ncol(W_train_mat) == ncol(W_test_mat))
stopifnot(all(V_train_mat$Intercept == 1), all(V_test_mat$Intercept == 1))
stopifnot(all(W_train_mat$Intercept == 1), all(W_test_mat$Intercept == 1))

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "V_train")
openxlsx::writeData(wb, "V_train", as.data.frame(V_train_mat))
openxlsx::addWorksheet(wb, "V_test")
openxlsx::writeData(wb, "V_test", as.data.frame(V_test_mat))
openxlsx::addWorksheet(wb, "W_train")
openxlsx::writeData(wb, "W_train", as.data.frame(W_train_mat))
openxlsx::addWorksheet(wb, "W_test")
openxlsx::writeData(wb, "W_test", as.data.frame(W_test_mat))
openxlsx::addWorksheet(wb, "Y_train")
openxlsx::writeData(wb, "Y_train", data.frame(response_id = V_train2$response_id, Y_train = Y_train))
openxlsx::addWorksheet(wb, "Y_test")
openxlsx::writeData(wb, "Y_test", data.frame(response_id = V_test2$response_id, Y_test = Y_test))

openxlsx::saveWorkbook(wb, file = file.path(ruta_salidas, "matrices_data.xlsx"), overwrite = TRUE)

# -----------------------------------------------------------------------------
# BLOQUE 12 — Prueba formal de sobredispersion (Cameron y Trivedi, 1990)
# [NUEVO — AGREGADO A PETICION EXPLICITA DEL USUARIO, NO proviene del
# material de referencia "code2_survey_purchase.R"]
#
# PROBLEMA DETECTADO: la prueba de Cameron y Trivedi necesita un modelo
# Poisson YA AJUSTADO para poder comparar sus residuales contra los
# valores ajustados (regresion auxiliar de (y - mu)^2 - y sobre mu). Este
# script, hasta el BLOQUE 11, nunca ajusta ningun modelo (ni el POGIT
# real ni ningun otro) — se detiene en la construccion de matrices, tal
# como confirmo el usuario que hacia el "Apple original".
# EXPLICACION: para poder correr la prueba es indispensable ajustar,
# como minimo, un GLM Poisson auxiliar. Este NO es el modelo POGIT del
# proyecto (que tiene dos componentes: V/x1 con enlace logit para la
# asignacion SoW, y W/x2 con enlace log para la intensidad SioW/lambda);
# es solo un modelo de diagnostico para poder ejecutar la prueba de
# sobredispersion que el usuario pidio explicitamente agregar.
# PROPUESTA: ajustar el GLM Poisson auxiliar usando Y_train como
# respuesta y W_train2 (la matriz de intensidad ya seleccionada,
# estandarizada y con Intercept) como predictores — es la eleccion mas
# defendible porque W es, por definicion en este proyecto, el componente
# que modela la intensidad de conteo (log(lambda_i)) del POGIT. Se
# entrena SOLO con TRAIN (misma regla de no fuga de datos usada en todo
# el pipeline). El resultado se exporta a un CSV de diagnostico y se
# imprime un mensaje con la interpretacion (evidencia o no de
# sobredispersion, y por tanto si conviene una Binomial Negativa en vez
# de Poisson puro para el componente de intensidad cuando se estime el
# POGIT real). Esto NO reemplaza ni adelanta la estimacion del modelo
# POGIT: sigue sin existir en este script, tal como se acordo.
# -----------------------------------------------------------------------------

W_train_diag <- W_train_mat  # ya tiene Intercept + columnas seleccionadas/escaladas (BLOQUE 11)

modelo_poisson_auxiliar <- stats::glm(
  Y_train ~ . - 1,
  data   = cbind(Y_train = Y_train, W_train_diag),
  family = stats::poisson(link = "log")
)

prueba_sobredispersion <- AER::dispersiontest(modelo_poisson_auxiliar, alternative = "greater")

diag_sobredispersion <- tibble::tibble(
  prueba              = "Cameron y Trivedi (1990) - sobredispersion",
  modelo_auxiliar      = "GLM Poisson: Y_train ~ W_train2 (Intercept + variables de intensidad)",
  estadistico_z        = round(unname(prueba_sobredispersion$statistic), 4),
  valor_p              = signif(prueba_sobredispersion$p.value, 4),
  dispersion_estimada  = round(unname(prueba_sobredispersion$estimate), 4),
  alternativa          = "sobredispersion (dispersion > 1)",
  n_observaciones      = length(Y_train),
  n_predictores        = ncol(W_train_diag),
  interpretacion       = dplyr::if_else(
    prueba_sobredispersion$p.value < 0.05,
    "Se rechaza equidispersion (p < 0.05): hay evidencia de sobredispersion. Considerar Binomial Negativa en vez de Poisson puro para el componente de intensidad del POGIT.",
    "No se rechaza equidispersion (p >= 0.05): no hay evidencia suficiente de sobredispersion en este modelo auxiliar."
  )
)
readr::write_csv(diag_sobredispersion, file.path(ruta_salidas, "diagnostico_sobredispersion_cameron_trivedi.csv"))

tabla_sobredispersion <- diag_sobredispersion %>%
  dplyr::transmute(
    Prueba                  = prueba,
    `Estadistico z`         = fmt_dec_es(estadistico_z, 3),
    `Valor p`                = fmt_dec_es(valor_p, 4),
    `Dispersion estimada`    = fmt_dec_es(dispersion_estimada, 3),
    `N observaciones`        = fmt_miles_es(n_observaciones),
    `N predictores`          = fmt_miles_es(n_predictores)
  )
graficar_tabla_metrica(
  tabla_sobredispersion,
  titulo = "Prueba de sobredispersion (Cameron y Trivedi, 1990) - Adidas",
  subtitulo = "Modelo auxiliar de diagnostico: GLM Poisson, Y_train ~ W_train2 (Intercept + intensidad)",
  nombre_archivo = "tabla_05_sobredispersion_cameron_trivedi",
  ancho = 11
)

message("Prueba de Cameron y Trivedi (sobredispersion) — estadistico z: ",
        round(unname(prueba_sobredispersion$statistic), 4),
        " | valor p: ", signif(prueba_sobredispersion$p.value, 4),
        " | dispersion estimada: ", round(unname(prueba_sobredispersion$estimate), 4))
message(diag_sobredispersion$interpretacion)

# -----------------------------------------------------------------------------
# Resumen final
# -----------------------------------------------------------------------------

message("\nPipeline especifico POGIT (Script 3) completo - Adidas.")
message("  c_train:                  ", c_train)
message("  Matrices finales:         ", file.path(ruta_salidas, "matrices_data.xlsx"))
message("  SioW/SoW discretizado:    ", file.path(ruta_salidas, "empirical_sow_discretizado.xlsx"))
message("  Graficas individuales:    ", ruta_graficas)
message("  Diagnosticos:             diagnostico_caps_winsorizacion.csv,")
message("                             diagnostico_rango_antes_seleccion.csv,")
message("                             diagnostico_rango_despues_seleccion.csv,")
message("                             diagnostico_columnas_eliminadas_W.csv,")
message("                             diagnostico_sobredispersion_cameron_trivedi.csv")
message("  Tablas de metricas (img): tabla_01_caps_winsorizacion, tabla_02_rango_antes_seleccion,")
message("                             tabla_03_rango_despues_seleccion, tabla_04_columnas_eliminadas_W,")
message("                             tabla_05_sobredispersion_cameron_trivedi (en graficas_individuales/)")
message("  NOTA: este script NO estima el modelo POGIT real ni calcula sus")
message("        metricas de desempeno (no existian en el material de")
message("        referencia). El GLM Poisson del BLOQUE 12 es SOLO un modelo")
message("        auxiliar de diagnostico para la prueba de Cameron y Trivedi,")
message("        agregada a peticion del usuario.")
