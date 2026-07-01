# ==============================================================================
# leitor_xml_moodle.R
# App Shiny: Leitor e Simulador de Questões do Moodle (XML gerado pelo R/exams)
# Disciplina: EST711 - Inferência Estatística II
#
# Uso:      shiny::runApp("leitor_xml_moodle.R")
# Depende:  shiny, xml2, bslib
#
# O app lê um XML no formato Moodle (exams2moodle), apresenta as questões com
# visual semelhante ao do Moodle (MathJax para as fórmulas), permite escolher
# alternativas, verificar a resposta e acompanhar a pontuação.
# ==============================================================================

pacotes  <- c("shiny", "xml2", "bslib")
faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) install.packages(faltando)

library(shiny)
library(xml2)
library(bslib)

# ==============================================================================
# PARSING DO XML DO MOODLE
# ==============================================================================

# Texto de um nó XML ("" se ausente)
texto_no <- function(node) {
  if (length(node) == 0) return("")
  txt <- xml_text(node)
  if (length(txt) == 0 || is.na(txt)) return("")
  txt
}

# Remove blocos <style>...</style> e <script>...</script> (o exams2moodle
# embute um <style> de tabelas em todo enunciado) e tags soltas de container
limpar_html_enunciado <- function(html) {
  html <- gsub("(?is)<style[^>]*>.*?</style>",   "", html, perl = TRUE)
  html <- gsub("(?is)<script[^>]*>.*?</script>", "", html, perl = TRUE)
  trimws(html)
}

# Versão texto-plano (usada apenas para checagens de conteúdo não vazio)
para_texto_plano <- function(html) {
  txt <- limpar_html_enunciado(html)
  txt <- gsub("<[^>]+>", " ", txt)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&lt;",  "<", txt, fixed = TRUE)
  txt <- gsub("&gt;",  ">", txt, fixed = TRUE)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", txt))
}

# Converte arquivos embutidos (base64) em data-URIs e substitui as
# referências @@PLUGINFILE@@/nome no HTML (necessário para questões com figuras)
embutir_arquivos <- function(html, nos_file) {
  if (length(nos_file) == 0) return(html)
  for (f in nos_file) {
    nome <- xml_attr(f, "name")
    enc  <- xml_attr(f, "encoding")
    if (is.na(nome) || is.na(enc) || enc != "base64") next
    ext  <- tolower(sub(".*\\.", "", nome))
    mime <- switch(ext,
      png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
      gif = "image/gif", svg = "image/svg+xml", "application/octet-stream")
    conteudo <- gsub("[[:space:]]", "", xml_text(f))
    uri      <- paste0("data:", mime, ";base64,", conteudo)
    html <- gsub(paste0("@@PLUGINFILE@@/", nome), uri, html, fixed = TRUE)
  }
  html
}

# Lê um XML do Moodle e devolve uma lista de questões estruturadas
parse_moodle_xml <- function(caminho_xml) {
  doc <- tryCatch(read_xml(caminho_xml), error = function(e) NULL)
  if (is.null(doc)) return(NULL)

  nos <- xml_find_all(doc, "//question")
  # Descarta nós de categoria e tipos sem alternativas
  tipos <- xml_attr(nos, "type")
  nos   <- nos[!is.na(tipos) & tipos %in% c("multichoice", "truefalse")]

  questoes <- lapply(nos, function(q) {
    nome <- trimws(texto_no(xml_find_first(q, "./name/text")))

    qt_no      <- xml_find_first(q, "./questiontext")
    enun_html  <- texto_no(xml_find_first(q, "./questiontext/text"))
    enun_html  <- limpar_html_enunciado(enun_html)
    enun_html  <- embutir_arquivos(enun_html, xml_find_all(qt_no, "./file"))

    fb_html <- texto_no(xml_find_first(q, "./generalfeedback/text"))
    fb_html <- limpar_html_enunciado(fb_html)

    single_txt <- tolower(trimws(texto_no(xml_find_first(q, "./single"))))
    unica      <- !identical(single_txt, "false")   # padrão: escolha única

    nos_resp <- xml_find_all(q, "./answer")
    if (length(nos_resp) == 0) return(NULL)

    alternativas <- lapply(nos_resp, function(a) {
      frac <- suppressWarnings(as.numeric(xml_attr(a, "fraction")))
      if (length(frac) == 0 || is.na(frac)) frac <- 0
      alt_html <- limpar_html_enunciado(texto_no(xml_find_first(a, "./text")))
      alt_fb   <- limpar_html_enunciado(texto_no(xml_find_first(a, "./feedback/text")))
      list(html = alt_html, fracao = frac, feedback = alt_fb)
    })

    # Remove alternativas sem conteúdo
    tem_txt <- vapply(alternativas,
                      function(a) nchar(para_texto_plano(a$html)) > 0, logical(1))
    alternativas <- alternativas[tem_txt]
    if (length(alternativas) == 0) return(NULL)

    fracoes <- vapply(alternativas, function(a) a$fracao, numeric(1))
    # Corretas: fração positiva (schoice: uma com 100; mchoice: várias positivas)
    corretas <- which(fracoes > 0)
    if (length(corretas) == 0) corretas <- which.max(fracoes)

    list(
      nome         = nome,
      enunciado    = enun_html,
      feedback     = fb_html,
      alternativas = alternativas,
      corretas     = corretas,
      unica        = unica || length(corretas) == 1
    )
  })

  questoes <- Filter(Negate(is.null), questoes)
  questoes <- Filter(function(q) nchar(para_texto_plano(q$enunciado)) > 0, questoes)
  if (length(questoes) == 0) return(NULL)
  questoes
}

# Procura XMLs em pastas usuais para oferecer no seletor local
listar_xmls_locais <- function() {
  dirs <- c("output_moodle", ".", file.path("Rexams", "output_moodle"))
  achados <- character(0)
  for (d in dirs) {
    if (!dir.exists(d)) next
    fs <- list.files(d, pattern = "\\.xml$", ignore.case = TRUE, full.names = TRUE)
    achados <- c(achados, fs)
  }
  achados <- normalizePath(unique(achados), winslash = "/", mustWork = FALSE)
  stats::setNames(achados, basename(achados))
}

# ==============================================================================
# CSS (estilo Moodle)
# ==============================================================================
css_moodle <- "
  body { background-color: #f5f5f5; }

  .moodle-header {
    background-color: #0066cc; color: white;
    padding: 12px 20px; border-radius: 6px 6px 0 0;
  }
  .moodle-header h4 { margin: 0; font-size: 1.05em; }

  .questao-box {
    background: #fff; border: 1px solid #dee2e6; border-top: none;
    border-radius: 0 0 6px 6px; padding: 20px; margin-bottom: 16px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  }

  .enunciado {
    font-size: 1.05em; line-height: 1.6; margin-bottom: 18px;
    border-left: 4px solid #0066cc; padding-left: 14px; color: #212529;
  }
  .enunciado p:last-child { margin-bottom: 0; }

  .alternativas-titulo {
    font-weight: 600; color: #495057; margin-bottom: 10px;
    font-size: 0.9em; text-transform: uppercase; letter-spacing: 0.05em;
  }

  /* Alternativas: cada opção como um cartão clicável */
  #resposta_escolha .form-check, #resposta_escolha .radio, #resposta_escolha .checkbox {
    padding: 10px 14px 10px 38px; margin-bottom: 8px;
    border: 1px solid #dee2e6; border-radius: 4px;
    background: #fafafa; transition: background .15s, border-color .15s;
  }
  #resposta_escolha .form-check:hover, #resposta_escolha .radio:hover,
  #resposta_escolha .checkbox:hover { background: #e8f0fe; border-color: #0066cc; }
  #resposta_escolha label { cursor: pointer; width: 100%; margin-bottom: 0; }
  #resposta_escolha label p { display: inline; margin: 0; }
  .letra-alt { font-weight: 600; color: #0066cc; margin-right: 6px; }

  .feedback-correto {
    background: #d4edda; border: 1px solid #28a745; border-radius: 4px;
    padding: 12px 16px; color: #155724; margin-bottom: 10px;
  }
  .feedback-incorreto {
    background: #f8d7da; border: 1px solid #dc3545; border-radius: 4px;
    padding: 12px 16px; color: #721c24; margin-bottom: 10px;
  }
  .feedback-solucao {
    background: #fff; border: 1px dashed #6c757d; border-radius: 4px;
    padding: 10px 14px; color: #383d41; margin-top: 8px; font-size: 0.97em;
  }
  .feedback-explicacao {
    background: #fff3cd; border: 1px solid #ffc107; border-radius: 4px;
    padding: 12px 16px; color: #664d03; margin-bottom: 16px;
  }
  .feedback-explicacao p:last-child { margin-bottom: 0; }

  .pontuacao-box {
    background: linear-gradient(135deg, #0066cc, #004499); color: white;
    border-radius: 8px; padding: 14px; text-align: center; margin-bottom: 14px;
  }
  .pontuacao-num { font-size: 2em; font-weight: bold; line-height: 1.1; }
  .pontuacao-txt { font-size: 0.85em; opacity: 0.9; }

  .mapa-botoes { display: flex; flex-wrap: wrap; gap: 4px; }
  .mapa-botoes .btn { width: 38px; height: 34px; padding: 2px 0; font-size: .85em; }

  .info-arquivo {
    background: #e9ecef; border-radius: 6px; padding: 8px 12px;
    margin-top: 8px; font-size: 0.88em; color: #495057;
  }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#0066cc"),
  tags$head(
    tags$style(HTML(css_moodle)),
    # Configuração do MathJax ANTES de carregar o script
    tags$script(HTML("
      window.MathJax = {
        tex: {
          inlineMath:  [['\\\\(', '\\\\)'], ['$', '$']],
          displayMath: [['\\\\[', '\\\\]'], ['$$', '$$']],
          processEscapes: true
        },
        options: { skipHtmlTags: ['script','noscript','style','textarea','pre'] }
      };
    ")),
    tags$script(
      src   = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js",
      async = NA
    ),
    # Re-renderiza as fórmulas sempre que o Shiny atualiza um output
    tags$script(HTML("
      $(document).on('shiny:value shiny:inputchanged', function() {
        clearTimeout(window._mjTimer);
        window._mjTimer = setTimeout(function() {
          if (window.MathJax && MathJax.typesetPromise) {
            MathJax.typesetPromise().catch(function(e) {});
          }
        }, 80);
      });
    "))
  ),

  div(
    style = "background:#0066cc; color:white; padding:12px 20px; border-radius:6px; margin:12px 0 16px 0;",
    h3(style = "margin:0;", "EST711 — Simulador de Questões Moodle"),
    p(style  = "margin:0; font-size:0.85em; opacity:0.85;",
      "Inferência Estatística II | Departamento de Estatística | UFV")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "card mb-3",
        div(class = "card-header fw-bold", "Carregar questões"),
        div(class = "card-body",
          uiOutput("ui_xml_local"),
          fileInput("arquivo_xml", "…ou envie um arquivo XML:",
                    accept = ".xml", buttonLabel = "Procurar…",
                    placeholder = "Nenhum arquivo"),
          uiOutput("ui_info_arquivo")
        )
      ),
      uiOutput("ui_pontuacao"),
      uiOutput("ui_navegacao"),
      uiOutput("ui_mapa_questoes")
    ),
    mainPanel(
      width = 9,
      uiOutput("ui_questao_atual"),
      uiOutput("ui_feedback")
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(
    questoes     = NULL,   # lista de questões parseadas
    idx_atual    = 1L,
    respostas    = list(), # respostas selecionadas (character; NULL = em branco)
    resultados   = logical(0), # TRUE/FALSE/NA por questão (NA = não verificada)
    nome_arquivo = NULL
  )
  observers_mapa <- list()   # handles dos observers do mapa (não reativo)

  # ---------- Carregamento ----------
  carregar_xml <- function(caminho, nome_exibicao) {
    withProgress(message = "Lendo questões…", value = 0.5, {
      questoes <- parse_moodle_xml(caminho)
    })
    if (is.null(questoes)) {
      showNotification("Não foi possível extrair questões deste XML.",
                       type = "error", duration = 6)
      return(invisible(FALSE))
    }
    n <- length(questoes)

    # Remove observers antigos do mapa e cria novos (um por questão)
    for (h in observers_mapa) h$destroy()
    observers_mapa <<- lapply(seq_len(n), function(i) {
      observeEvent(input[[paste0("mapa_q", i)]], {
        freezeReactiveValue(input, "resposta_escolha")
        rv$idx_atual <- i
      }, ignoreInit = TRUE)
    })

    rv$questoes     <- questoes
    rv$idx_atual    <- 1L
    rv$respostas    <- vector("list", n)
    rv$resultados   <- rep(NA, n)
    rv$nome_arquivo <- nome_exibicao

    showNotification(sprintf("%d questões carregadas de '%s'.", n, nome_exibicao),
                     type = "message", duration = 4)
    invisible(TRUE)
  }

  # Seletor de XMLs locais (pasta output_moodle)
  output$ui_xml_local <- renderUI({
    xmls <- listar_xmls_locais()
    if (length(xmls) == 0) {
      return(p(class = "text-muted", style = "font-size:.85em;",
               "Nenhum XML encontrado em 'output_moodle/'."))
    }
    tagList(
      selectInput("sel_xml_local", "XML da pasta output_moodle:",
                  choices = c("— selecione —" = "", xmls), width = "100%"),
      actionButton("btn_carregar_local", "Carregar selecionado",
                   class = "btn btn-primary btn-sm w-100 mb-2")
    )
  })

  observeEvent(input$btn_carregar_local, {
    req(input$sel_xml_local)
    if (!nzchar(input$sel_xml_local)) return()
    carregar_xml(input$sel_xml_local, basename(input$sel_xml_local))
  })

  observeEvent(input$arquivo_xml, {
    req(input$arquivo_xml)
    carregar_xml(input$arquivo_xml$datapath, input$arquivo_xml$name)
  })

  output$ui_info_arquivo <- renderUI({
    req(rv$questoes)
    div(class = "info-arquivo",
        tags$b("Arquivo: "), rv$nome_arquivo, tags$br(),
        tags$b("Questões: "), length(rv$questoes))
  })

  # ---------- Pontuação (recalculada do estado, sem contadores) ----------
  output$ui_pontuacao <- renderUI({
    req(rv$questoes)
    res <- rv$resultados
    n_v <- sum(!is.na(res))
    n_c <- sum(res %in% TRUE)
    pct <- if (n_v > 0) round(100 * n_c / n_v) else 0
    div(class = "pontuacao-box",
        div(class = "pontuacao-num", sprintf("%d / %d", n_c, n_v)),
        div(class = "pontuacao-txt",
            sprintf("acertos (%d%%) — %d de %d verificadas",
                    pct, n_v, length(res))))
  })

  # ---------- Navegação ----------
  output$ui_navegacao <- renderUI({
    req(rv$questoes)
    n_q <- length(rv$questoes)
    idx <- rv$idx_atual
    pct <- 100 * sum(!is.na(rv$resultados)) / n_q
    div(
      tags$b(sprintf("Questão %d de %d", idx, n_q)),
      div(class = "progress my-2", style = "height: 8px;",
          div(class = "progress-bar bg-primary",
              style = sprintf("width:%.1f%%", pct))),
      div(class = "d-flex gap-2 mb-2",
          actionButton("btn_anterior", "‹ Anterior",
                       class = "btn btn-outline-primary btn-sm flex-fill"),
          actionButton("btn_proxima", "Próxima ›",
                       class = "btn btn-outline-primary btn-sm flex-fill")),
      selectInput("sel_questao", NULL,
                  choices  = stats::setNames(seq_len(n_q),
                                             sprintf("Ir para a questão %d", seq_len(n_q))),
                  selected = idx, width = "100%")
    )
  })

  # ---------- Mapa de questões ----------
  output$ui_mapa_questoes <- renderUI({
    req(rv$questoes)
    idx <- rv$idx_atual
    botoes <- lapply(seq_along(rv$questoes), function(i) {
      cls <- if (i == idx) "btn-primary"
        else if (isTRUE(rv$resultados[i]))       "btn-success"
        else if (isFALSE(rv$resultados[i]))      "btn-danger"
        else if (!is.null(rv$respostas[[i]]))    "btn-warning"
        else                                     "btn-outline-secondary"
      actionButton(paste0("mapa_q", i), as.character(i),
                   class = paste("btn btn-sm", cls))
    })
    div(
      h6("Mapa de questões", style = "font-weight:600;"),
      p(style = "font-size:.75em; color:#6c757d; margin-bottom:6px;",
        "azul: atual · amarelo: respondida · verde: correta · vermelho: incorreta"),
      div(class = "mapa-botoes", botoes)
    )
  })

  # ---------- Questão atual ----------
  output$ui_questao_atual <- renderUI({
    req(rv$questoes)
    idx <- rv$idx_atual
    q   <- rv$questoes[[idx]]

    # Rótulos em HTML (letra + conteúdo) e valores "1","2",...
    n_alt   <- length(q$alternativas)
    letras  <- letters[seq_len(n_alt)]
    nomes   <- lapply(seq_len(n_alt), function(j) {
      HTML(sprintf('<span class="letra-alt">%s)</span>%s',
                   letras[j], q$alternativas[[j]]$html))
    })
    valores <- as.character(seq_len(n_alt))
    sel     <- rv$respostas[[idx]]   # character ou NULL

    seletor <- if (q$unica) {
      radioButtons("resposta_escolha", NULL,
                   choiceNames  = nomes,
                   choiceValues = valores,
                   selected     = if (is.null(sel)) character(0) else sel,
                   width        = "100%")
    } else {
      checkboxGroupInput("resposta_escolha", NULL,
                         choiceNames  = nomes,
                         choiceValues = valores,
                         selected     = if (is.null(sel)) character(0) else sel,
                         width        = "100%")
    }

    div(
      div(class = "moodle-header",
          h4(sprintf("Questão %d", idx),
             tags$small(style = "opacity:.8; margin-left:10px;", q$nome))),
      div(class = "questao-box",
          div(class = "enunciado", HTML(q$enunciado)),
          div(class = "alternativas-titulo",
              if (q$unica) "Selecione uma alternativa:"
              else         "Selecione uma ou mais alternativas:"),
          seletor,
          div(class = "d-flex gap-2 mt-3",
              actionButton("btn_verificar", "Verificar resposta",
                           class = "btn btn-primary", icon = icon("check")),
              actionButton("btn_limpar", "Limpar",
                           class = "btn btn-outline-secondary")))
    )
  })

  # ---------- Feedback ----------
  output$ui_feedback <- renderUI({
    req(rv$questoes)
    idx <- rv$idx_atual
    res <- rv$resultados[idx]
    if (is.na(res)) return(NULL)          # ainda não verificada

    q        <- rv$questoes[[idx]]
    corr_idx <- q$corretas
    corr_html <- lapply(corr_idx, function(j) {
      div(HTML(sprintf('<span class="letra-alt">%s)</span>%s',
                       letters[j], q$alternativas[[j]]$html)))
    })

    # Feedback específico da alternativa marcada (se houver e for escolha única)
    fb_alt <- ""
    sel <- rv$respostas[[idx]]
    if (q$unica && !is.null(sel) && length(sel) == 1) {
      j <- suppressWarnings(as.integer(sel))
      if (!is.na(j) && j >= 1 && j <= length(q$alternativas)) {
        fb_alt <- q$alternativas[[j]]$feedback
      }
    }

    tagList(
      if (isTRUE(res)) {
        div(class = "feedback-correto",
            tags$b(icon("circle-check"), " Correto!"),
            if (nzchar(fb_alt)) div(class = "mt-2", HTML(fb_alt)))
      } else {
        div(class = "feedback-incorreto",
            tags$b(icon("circle-xmark"), " Incorreto."),
            if (nzchar(fb_alt)) div(class = "mt-2", HTML(fb_alt)),
            div(class = "feedback-solucao",
                tags$b(if (length(corr_idx) == 1) "Resposta correta:"
                       else "Respostas corretas:"),
                corr_html))
      },
      if (nzchar(q$feedback)) {
        div(class = "feedback-explicacao",
            tags$b("Explicação: "), HTML(q$feedback))
      }
    )
  })

  # ---------- Interações ----------

  # Guarda a seleção da questão atual (e invalida verificação anterior)
  observeEvent(input$resposta_escolha, {
    req(rv$questoes)
    idx <- rv$idx_atual
    rv$respostas[idx] <- list(input$resposta_escolha)
    if (!is.na(rv$resultados[idx])) rv$resultados[idx] <- NA
  }, ignoreInit = TRUE)

  # Verificar
  observeEvent(input$btn_verificar, {
    req(rv$questoes)
    idx <- rv$idx_atual
    sel <- input$resposta_escolha
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Selecione uma alternativa antes de verificar.",
                       type = "warning", duration = 3)
      return()
    }
    rv$respostas[idx] <- list(sel)

    q       <- rv$questoes[[idx]]
    sel_int <- suppressWarnings(as.integer(sel))
    sel_int <- sel_int[!is.na(sel_int)]
    # Correta se o conjunto marcado coincide com o conjunto de corretas
    rv$resultados[idx] <- setequal(sel_int, q$corretas)
  })

  # Limpar
  observeEvent(input$btn_limpar, {
    req(rv$questoes)
    idx <- rv$idx_atual
    rv$respostas[idx]  <- list(NULL)   # mantém o comprimento da lista
    rv$resultados[idx] <- NA
    freezeReactiveValue(input, "resposta_escolha")
    if (rv$questoes[[idx]]$unica) {
      updateRadioButtons(session, "resposta_escolha", selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "resposta_escolha", selected = character(0))
    }
  })

  # Navegação (congela o input para não vazar a resposta entre questões)
  observeEvent(input$btn_anterior, {
    req(rv$questoes)
    if (rv$idx_atual > 1) {
      freezeReactiveValue(input, "resposta_escolha")
      rv$idx_atual <- rv$idx_atual - 1L
    }
  })
  observeEvent(input$btn_proxima, {
    req(rv$questoes)
    if (rv$idx_atual < length(rv$questoes)) {
      freezeReactiveValue(input, "resposta_escolha")
      rv$idx_atual <- rv$idx_atual + 1L
    }
  })
  observeEvent(input$sel_questao, {
    req(rv$questoes)
    novo <- suppressWarnings(as.integer(input$sel_questao))
    if (!is.na(novo) && novo >= 1 && novo <= length(rv$questoes) &&
        novo != rv$idx_atual) {
      freezeReactiveValue(input, "resposta_escolha")
      rv$idx_atual <- novo
    }
  }, ignoreInit = TRUE)

  # Mantém o dropdown sincronizado com a questão atual
  observeEvent(rv$idx_atual, {
    req(rv$questoes)
    updateSelectInput(session, "sel_questao", selected = rv$idx_atual)
  })
}

shinyApp(ui = ui, server = server)
