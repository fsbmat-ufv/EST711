# =====================================================================
# EST 150 - Prova 2 - Versoes PARAMETRIZADAS (R/exams -> Moodle)
# Prof. Fernando de Souza Bastos - UFV
# ---------------------------------------------------------------------
# As questoes em 'parametrizadas/' sorteiam valores, cenarios e a ordem
# das alternativas a cada geracao. Assim, n = 20 produz 20 versoes
# REALMENTE diferentes de cada questao.
#
# Este script:
#   1) gera 20 XML individuais (PR2_1.xml ... PR2_20.xml), cada um com
#      n = 20 versoes da respectiva questao;
#   2) gera 1 XML combinado (PR2.xml) com 20 versoes de cada questao.
#
# Pre-requisitos: install.packages("exams") e pandoc (vem com o RStudio).
# =====================================================================

library(exams)

## Diretorio de trabalho = pasta deste script -------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

## Reprodutibilidade da geracao (mude/retire para sortear diferente) ---
set.seed(150)

## Lista das 20 questoes parametrizadas -------------------------------
questoes <- c(
  "q01_amostra_vies_selecao.Rmd",
  "q02_media_mediana.Rmd",
  "q03_grafico_enganoso.Rmd",
  "q04_correlacao_causalidade.Rmd",
  "q05_reprodutibilidade.Rmd",
  "q06_auditabilidade.Rmd",
  "q07_datasheet_modelcard.Rmd",
  "q08_tipos_vies.Rmd",
  "q09_acuracia_subgrupo.Rmd",
  "q10_falso_positivo_negativo.Rmd",
  "q11_feedback_loop_accountability.Rmd",
  "q12_triade_cia.Rmd",
  "q13_autenticacao_autorizacao.Rmd",
  "q14_hash_criptografia.Rmd",
  "q15_sal_senha.Rmd",
  "q16_ameacas_dataleakage.Rmd",
  "q17_qualidade_dados.Rmd",
  "q18_tidy_joins.Rmd",
  "q19_governanca_fair.Rmd",
  "q20_git_github_r.Rmd"
)

arquivos <- file.path("parametrizadas", questoes)
nomes    <- paste0("PR2_", seq_along(questoes))   # PR2_1 ... PR2_20

dir.create("output_parametrizadas", showWarnings = FALSE)

## ---------------------------------------------------------------------
## 1) Um XML por questao: PR2_1.xml ... PR2_20.xml (n = n versoes cada)
## ---------------------------------------------------------------------
for (i in seq_along(arquivos)) {
  exams::exams2moodle(
    file     = arquivos[i],
    n        = 10,
    verbose  = TRUE,
    name     = nomes[i],
    dir      = "output_parametrizadas",
    encoding = "UTF-8"
  )
}

## ---------------------------------------------------------------------
## 2) Um unico XML com as 20 questoes, n versoes de cada: PR2.xml
## ---------------------------------------------------------------------
exams::exams2moodle(
  file     = arquivos,
  n        = 10,
  verbose  = TRUE,
  name     = "PR2",
  dir      = "output_parametrizadas",
  encoding = "UTF-8"
)

cat("\nConcluido. XML em: ", normalizePath("output_parametrizadas"), "\n", sep = "")

## ---------------------------------------------------------------------
## (Opcional) Conferir uma questao em PDF antes de exportar ------------
## ---------------------------------------------------------------------
# exams2pdf(file = "parametrizadas/q09_acuracia_subgrupo.Rmd", n = 3,
#           dir = "output_parametrizadas", encoding = "UTF-8")
