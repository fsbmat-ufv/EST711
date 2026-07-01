# ==============================================================================
# EST711 - Inferência Estatística II
# Descoberta, validação e geração automática do banco R/exams
# ==============================================================================

library(exams)

## Diretório do próprio script -------------------------------------------------
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(path)) return(dirname(normalizePath(path)))
  }
  normalizePath(getwd())
}

setwd(script_dir())
dir.create("output_moodle", showWarnings = FALSE)
dir.create("output_html", showWarnings = FALSE)

## Descoberta automática, com ordenação numérica Cap/S/Q ----------------------
exercise_key <- function(path) {
  name <- basename(path)
  hit <- regexec("^Cap([0-9]+)S([0-9]+)Q([0-9]+)\\.Rmd$", name)
  part <- regmatches(name, hit)[[1]]
  if (length(part) != 4L) {
    stop("Nome de exercício inválido: ", path)
  }
  as.integer(part[2:4])
}

cap_dirs <- list.dirs(".", full.names = FALSE, recursive = FALSE)
cap_dirs <- cap_dirs[grepl("^Cap[0-9]+$", cap_dirs)]
cap_dirs <- cap_dirs[order(as.integer(sub("^Cap", "", cap_dirs)))]
if (!length(cap_dirs)) stop("Nenhum diretório CapN foi encontrado.")

discover_chapter <- function(cap_dir) {
  files <- list.files(cap_dir, pattern = "\\.Rmd$", full.names = TRUE)
  if (!length(files)) stop("Nenhum .Rmd em ", cap_dir)
  keys <- t(vapply(files, exercise_key, integer(3)))
  expected <- as.integer(sub("^Cap", "", cap_dir))
  if (any(keys[, 1] != expected)) {
    stop("Há arquivo com capítulo incompatível dentro de ", cap_dir)
  }
  files[order(keys[, 2], keys[, 3])]
}

capitulos <- setNames(lapply(cap_dirs, discover_chapter), cap_dirs)
todos <- unlist(capitulos, use.names = FALSE)
if (anyDuplicated(normalizePath(todos))) stop("Há arquivos duplicados na descoberta.")

cat("Banco descoberto automaticamente:\n")
for (cap in names(capitulos)) {
  cat(sprintf("  %s: %d questões\n", cap, length(capitulos[[cap]])))
}
cat(sprintf("  Total: %d questões\n", length(todos)))

## Configuração ----------------------------------------------------------------
n_versoes <- suppressWarnings(as.integer(Sys.getenv("EST711_N_VERSOES", "2")))
if (is.na(n_versoes) || n_versoes < 1L) {
  stop("EST711_N_VERSOES deve ser um inteiro positivo.")
}
skip_html <- identical(tolower(Sys.getenv("EST711_SKIP_HTML", "false")), "true")

falhas <- character()

## XML por capítulo ------------------------------------------------------------
for (cap in names(capitulos)) {
  cat(sprintf("\nGerando XML de %s (%d questões)...\n",
              cap, length(capitulos[[cap]])))
  tryCatch(
    exams2moodle(
      file = capitulos[[cap]],
      n = n_versoes,
      name = paste0("EST711_", cap),
      dir = "output_moodle",
      converter = "pandoc-mathjax",
      encoding = "UTF-8"
    ),
    error = function(e) {
      falhas <<- c(falhas, sprintf("XML %s: %s", cap, conditionMessage(e)))
    }
  )
}

## XML combinado ---------------------------------------------------------------
cat("\nGerando XML combinado...\n")
tryCatch(
  exams2moodle(
    file = todos,
    n = n_versoes,
    name = "EST711_completo",
    dir = "output_moodle",
    converter = "pandoc-mathjax",
    encoding = "UTF-8"
  ),
  error = function(e) {
    falhas <<- c(falhas, sprintf("XML completo: %s", conditionMessage(e)))
  }
)

## Prévia HTML: primeira questão de cada seção ---------------------------------
if (!skip_html) {
  for (cap in names(capitulos)) {
    keys <- t(vapply(capitulos[[cap]], exercise_key, integer(3)))
    preview <- capitulos[[cap]][!duplicated(keys[, 2])]
    cat(sprintf("Gerando prévia HTML de %s (%d questões)...\n",
                cap, length(preview)))
    tryCatch(
      exams2html(
        file = preview,
        n = 1,
        name = paste0("EST711_", cap, "_preview"),
        dir = "output_html",
        converter = "pandoc-mathjax",
        encoding = "UTF-8"
      ),
      error = function(e) {
        falhas <<- c(falhas, sprintf("HTML %s: %s", cap, conditionMessage(e)))
      }
    )
  }
}

if (length(falhas)) {
  stop("Falhas na geração:\n- ", paste(falhas, collapse = "\n- "))
}

cat(sprintf("\nGeração concluída: %d questões, %d versão(ões) por questão.\n",
            length(todos), n_versoes))
