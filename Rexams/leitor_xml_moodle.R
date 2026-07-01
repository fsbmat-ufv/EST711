# ==============================================================================
# leitor_xml_moodle.R
# App Shiny: Leitor e Simulador de Questões do Moodle (XML)
# Disciplina: EST711 - Inferência Estatística II
# Uso: shiny::runApp("leitor_xml_moodle.R")
# Dependências: shiny, xml2, bslib (ou shinythemes), shinyjs
# ==============================================================================

# Instalar pacotes necessários se ausentes
pacotes <- c("shiny", "xml2", "bslib", "shinyjs", "htmltools")
instalar <- pacotes[!sapply(pacotes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) {
  install.packages(instalar)
}

library(shiny)
library(xml2)
library(bslib)
library(shinyjs)

# ==============================================================================
# FUNÇÕES AUXILIARES DE PARSING DO XML DO MOODLE
# ==============================================================================

#' Extrai o texto de um nó XML, retornando "" se NULL
safe_xml_text <- function(node) {
  if (is.null(node) || length(node) == 0) return("")
  txt <- xml_text(node)
  if (is.na(txt)) return("")
  txt
}

#' Remove tags HTML simples do texto (para exibição básica sem renderizador HTML)
strip_basic_html <- function(txt) {
  # Remove tags HTML comuns
  txt <- gsub("<[^>]+>", "", txt)
  # Decodifica entidades HTML básicas
  txt <- gsub("&amp;",  "&",  txt)
  txt <- gsub("&lt;",   "<",  txt)
  txt <- gsub("&gt;",   ">",  txt)
  txt <- gsub("&quot;", '"',  txt)
  txt <- gsub("&#39;",  "'",  txt)
  txt <- gsub("&nbsp;", " ",  txt)
  trimws(txt)
}

#' Faz parse de um arquivo XML do Moodle e retorna lista de questões
parse_moodle_xml <- function(caminho_xml) {
  doc <- tryCatch(
    xml_read <- read_xml(caminho_xml),
    error = function(e) NULL
  )
  if (is.null(doc)) return(NULL)

  # Seleciona todas as questões (exceto tipo "category")
  nos_questoes <- xml_find_all(doc, "//question[@type != 'category']")

  if (length(nos_questoes) == 0) {
    # Tenta sem filtro de tipo
    nos_questoes <- xml_find_all(doc, "//question")
    nos_questoes <- nos_questoes[xml_attr(nos_questoes, "type") != "category"]
  }

  questoes <- lapply(nos_questoes, function(q) {
    tipo <- xml_attr(q, "type")

    # Nome da questão
    nome_node <- xml_find_first(q, ".//name/text")
    nome <- safe_xml_text(nome_node)

    # Enunciado (questiontext)
    enunciado_node <- xml_find_first(q, ".//questiontext/text")
    enunciado_raw  <- safe_xml_text(enunciado_node)
    enunciado      <- strip_basic_html(enunciado_raw)
    # Guarda versão HTML para renderizar no Shiny
    enunciado_html <- enunciado_raw

    # Feedback geral
    feedback_node <- xml_find_first(q, ".//generalfeedback/text")
    feedback <- safe_xml_text(feedback_node)
    feedback <- strip_basic_html(feedback)

    # Alternativas (answer nodes)
    nos_resp <- xml_find_all(q, ".//answer")

    alternativas <- lapply(nos_resp, function(a) {
      frac   <- as.numeric(xml_attr(a, "fraction"))
      if (is.na(frac)) frac <- 0
      texto_node    <- xml_find_first(a, ".//text")
      texto_raw     <- safe_xml_text(texto_node)
      texto         <- strip_basic_html(texto_raw)
      texto_html    <- texto_raw
      fb_node       <- xml_find_first(a, ".//feedback/text")
      fb            <- strip_basic_html(safe_xml_text(fb_node))
      lista(
        texto      = texto,
        texto_html = texto_html,
        fracao     = frac,
        feedback   = fb,
        correta    = (frac >= 99)   # fraction=100 = correta
      )
    })

    # Filtra alternativas vazias
    alternativas <- alternativas[sapply(alternativas, function(a) nchar(a$texto) > 0)]

    list(
      nome          = nome,
      tipo          = tipo,
      enunciado     = enunciado,
      enunciado_html = enunciado_html,
      feedback      = feedback,
      alternativas  = alternativas,
      n_corretas    = sum(sapply(alternativas, function(a) a$correta))
    )
  })

  # Filtra questões sem enunciado ou sem alternativas
  questoes <- questoes[sapply(questoes, function(q) {
    nchar(q$enunciado) > 0 && length(q$alternativas) > 0
  })]

  questoes
}

# Alias para compatibilidade
lista <- list

# ==============================================================================
# CSS PERSONALIZADO (estilo Moodle)
# ==============================================================================
css_moodle <- "
  body { background-color: #f5f5f5; font-family: 'Segoe UI', Arial, sans-serif; }

  .moodle-header {
    background-color: #0066cc;
    color: white;
    padding: 12px 20px;
    border-radius: 6px 6px 0 0;
    margin-bottom: 0;
  }
  .moodle-header h4 { margin: 0; font-size: 1.1em; }

  .questao-box {
    background: #ffffff;
    border: 1px solid #dee2e6;
    border-radius: 6px;
    padding: 20px;
    margin-bottom: 16px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  }

  .enunciado {
    font-size: 1.05em;
    line-height: 1.6;
    margin-bottom: 18px;
    color: #212529;
    border-left: 4px solid #0066cc;
    padding-left: 14px;
  }

  .alternativas-titulo {
    font-weight: 600;
    color: #495057;
    margin-bottom: 10px;
    font-size: 0.95em;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .alternativa-label {
    display: block;
    padding: 10px 14px;
    margin-bottom: 6px;
    border: 1px solid #dee2e6;
    border-radius: 4px;
    cursor: pointer;
    transition: background 0.15s;
    background: #fafafa;
  }
  .alternativa-label:hover { background: #e8f0fe; border-color: #0066cc; }

  .feedback-correto {
    background-color: #d4edda;
    border: 1px solid #28a745;
    border-radius: 4px;
    padding: 12px 16px;
    color: #155724;
    margin-top: 12px;
    font-weight: 500;
  }
  .feedback-incorreto {
    background-color: #f8d7da;
    border: 1px solid #dc3545;
    border-radius: 4px;
    padding: 12px 16px;
    color: #721c24;
    margin-top: 12px;
    font-weight: 500;
  }
  .feedback-neutro {
    background-color: #fff3cd;
    border: 1px solid #ffc107;
    border-radius: 4px;
    padding: 12px 16px;
    color: #856404;
    margin-top: 12px;
  }
  .feedback-solucao {
    background-color: #e2e3e5;
    border: 1px solid #6c757d;
    border-radius: 4px;
    padding: 10px 14px;
    color: #383d41;
    margin-top: 8px;
    font-size: 0.95em;
  }

  .nav-btn { margin: 4px; }

  .pontuacao-box {
    background: linear-gradient(135deg, #0066cc, #004499);
    color: white;
    border-radius: 8px;
    padding: 16px;
    text-align: center;
    margin-bottom: 16px;
  }
  .pontuacao-num { font-size: 2.2em; font-weight: bold; }
  .pontuacao-txt { font-size: 0.9em; opacity: 0.85; }

  .progresso-label { font-size: 0.85em; color: #6c757d; margin-bottom: 4px; }

  .badge-questao {
    display: inline-block;
    background: #0066cc;
    color: white;
    border-radius: 50%;
    width: 28px;
    height: 28px;
    line-height: 28px;
    text-align: center;
    font-weight: bold;
    font-size: 0.85em;
    margin-right: 8px;
  }

  .questao-respondida { background: #e8f0fe; }
  .questao-correta    { background: #d4edda; }
  .questao-incorreta  { background: #f8d7da; }

  #painel_lateral { position: sticky; top: 20px; }

  .info-arquivo {
    background: #e9ecef;
    border-radius: 6px;
    padding: 10px 14px;
    margin-bottom: 12px;
    font-size: 0.9em;
    color: #495057;
  }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#0066cc"
  ),
  useShinyjs(),
  tags$head(
    tags$style(HTML(css_moodle)),
    tags$script(HTML("
      // Renderiza LaTeX com MathJax após atualização do conteúdo
      function renderMath() {
        if (window.MathJax) {
          MathJax.typesetPromise().catch(function(err) { console.log(err); });
        }
      }
    ")),
    # MathJax para renderização de LaTeX
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js",
      id  = "MathJax-script",
      async = NA
    ),
    tags$script(HTML("
      window.MathJax = {
        tex: {
          inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],
          displayMath: [['$$', '$$'], ['\\\\[', '\\\\]']]
        },
        options: { skipHtmlTags: ['script','noscript','style','textarea','pre'] }
      };
    "))
  ),
  titlePanel(
    div(
      style = "background:#0066cc; color:white; padding:12px 20px; border-radius:6px; margin-bottom:16px;",
      h3(style = "margin:0;", "EST711 — Simulador de Questões Moodle"),
      p(style  = "margin:0; font-size:0.85em; opacity:0.85;",
        "Inferência Estatística II | Departamento de Estatística | UFV")
    )
  ),

  sidebarLayout(
    # ------ PAINEL LATERAL ------
    sidebarPanel(
      id    = "painel_lateral",
      width = 3,

      # Upload do arquivo XML
      div(class = "card mb-3",
        div(class = "card-header fw-bold", "Carregar Questões"),
        div(class = "card-body",
          fileInput(
            inputId  = "arquivo_xml",
            label    = "Arquivo XML do Moodle:",
            accept   = ".xml",
            buttonLabel = "Selecionar...",
            placeholder = "Nenhum arquivo"
          ),
          uiOutput("ui_info_arquivo")
        )
      ),

      # Pontuação
      uiOutput("ui_pontuacao"),

      # Navegação entre questões
      uiOutput("ui_navegacao"),

      hr(),

      # Mapa de questões
      uiOutput("ui_mapa_questoes")
    ),

    # ------ PAINEL PRINCIPAL ------
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

  # ---------- ESTADO REATIVO ----------
  rv <- reactiveValues(
    questoes       = NULL,   # lista de questões parseadas
    idx_atual      = 1,      # índice da questão atual (1-based)
    respostas      = list(), # lista: respostas selecionadas (índice da alternativa)
    verificadas    = list(), # lista: quais questões foram verificadas
    resultados     = list(), # lista: TRUE/FALSE por questão
    n_corretas     = 0,
    n_verificadas  = 0,
    nome_arquivo   = NULL
  )

  # ---------- CARREGAR XML ----------
  observeEvent(input$arquivo_xml, {
    req(input$arquivo_xml)

    withProgress(message = "Carregando questões...", value = 0.5, {
      questoes <- parse_moodle_xml(input$arquivo_xml$datapath)
    })

    if (is.null(questoes) || length(questoes) == 0) {
      showNotification(
        "Erro: não foi possível carregar questões do arquivo XML.",
        type = "error", duration = 6
      )
      return()
    }

    rv$questoes      <- questoes
    rv$idx_atual     <- 1
    rv$respostas     <- vector("list", length(questoes))
    rv$verificadas   <- vector("list", length(questoes))
    rv$resultados    <- vector("list", length(questoes))
    rv$n_corretas    <- 0
    rv$n_verificadas <- 0
    rv$nome_arquivo  <- input$arquivo_xml$name

    showNotification(
      sprintf("Carregadas %d questões de '%s'.", length(questoes), input$arquivo_xml$name),
      type = "message", duration = 4
    )
  })

  # ---------- INFORMAÇÕES DO ARQUIVO ----------
  output$ui_info_arquivo <- renderUI({
    req(rv$questoes)
    div(class = "info-arquivo",
      tags$b("Arquivo: "), rv$nome_arquivo, tags$br(),
      tags$b("Questões: "), length(rv$questoes)
    )
  })

  # ---------- PONTUAÇÃO ----------
  output$ui_pontuacao <- renderUI({
    req(rv$questoes)
    n_q <- length(rv$questoes)
    n_v <- rv$n_verificadas
    n_c <- rv$n_corretas
    pct <- if (n_v > 0) round(100 * n_c / n_v) else 0

    div(class = "pontuacao-box",
      div(class = "pontuacao-num",
        sprintf("%d / %d", n_c, n_v)
      ),
      div(class = "pontuacao-txt",
        sprintf("Acertos (%d%%) | %d questão(ões) restantes", pct, n_q - n_v)
      )
    )
  })

  # ---------- CONTROLES DE NAVEGAÇÃO ----------
  output$ui_navegacao <- renderUI({
    req(rv$questoes)
    n_q <- length(rv$questoes)
    idx <- rv$idx_atual

    div(
      div(class = "d-flex justify-content-between align-items-center mb-2",
        tags$b(sprintf("Questão %d de %d", idx, n_q))
      ),
      div(class = "progresso-label",
        sprintf("%.0f%% concluído", 100 * rv$n_verificadas / n_q)
      ),
      div(class = "progress mb-3",
        div(
          class = "progress-bar bg-primary",
          style = sprintf("width: %.1f%%", 100 * rv$n_verificadas / n_q),
          role  = "progressbar"
        )
      ),
      div(class = "d-flex gap-2",
        actionButton("btn_anterior", "< Anterior",
                     class = "btn btn-outline-primary btn-sm nav-btn flex-fill",
                     disabled = (idx == 1)),
        actionButton("btn_proxima", "Próxima >",
                     class = "btn btn-outline-primary btn-sm nav-btn flex-fill",
                     disabled = (idx == n_q))
      ),
      div(class = "mt-2",
        selectInput("sel_questao", "Ir para a questão:",
                    choices  = setNames(seq_len(n_q), sprintf("Questão %d", seq_len(n_q))),
                    selected = idx,
                    width    = "100%")
      )
    )
  })

  # ---------- MAPA DE QUESTÕES ----------
  output$ui_mapa_questoes <- renderUI({
    req(rv$questoes)
    n_q <- length(rv$questoes)
    idx <- rv$idx_atual

    botoes <- lapply(seq_len(n_q), function(i) {
      cls <- "btn btn-sm m-1 "
      if (i == idx) {
        cls <- paste0(cls, "btn-primary")
      } else if (!is.null(rv$verificadas[[i]]) && rv$verificadas[[i]]) {
        if (isTRUE(rv$resultados[[i]])) {
          cls <- paste0(cls, "btn-success")
        } else {
          cls <- paste0(cls, "btn-danger")
        }
      } else if (!is.null(rv$respostas[[i]])) {
        cls <- paste0(cls, "btn-warning")
      } else {
        cls <- paste0(cls, "btn-outline-secondary")
      }
      actionButton(
        inputId = paste0("mapa_q", i),
        label   = as.character(i),
        class   = cls,
        style   = "width:36px; height:36px; padding:0;"
      )
    })

    div(
      h6("Mapa de Questões:", style = "font-weight:600; margin-bottom:8px;"),
      p(style = "font-size:0.78em; color:#6c757d; margin-bottom:6px;",
        tags$span(style = "color:#0066cc;", "Atual "),
        tags$span(style = "color:#6c757d;", "| "),
        tags$span(style = "color:#ffc107;", "Respondida "),
        tags$span(style = "color:#6c757d;", "| "),
        tags$span(style = "color:#28a745;", "Correta "),
        tags$span(style = "color:#6c757d;", "| "),
        tags$span(style = "color:#dc3545;", "Incorreta")
      ),
      div(style = "display:flex; flex-wrap:wrap;", botoes)
    )
  })

  # Observar cliques no mapa
  observe({
    req(rv$questoes)
    n_q <- length(rv$questoes)
    for (i in seq_len(n_q)) {
      local({
        ii <- i
        observeEvent(input[[paste0("mapa_q", ii)]], {
          rv$idx_atual <- ii
          updateSelectInput(session, "sel_questao", selected = ii)
        }, ignoreInit = TRUE)
      })
    }
  })

  # ---------- QUESTÃO ATUAL ----------
  output$ui_questao_atual <- renderUI({
    req(rv$questoes)
    idx <- rv$idx_atual
    q   <- rv$questoes[[idx]]
    ja_verificada <- isTRUE(rv$verificadas[[idx]])

    # Enunciado (com renderização HTML + LaTeX via MathJax)
    enunciado_html <- if (nchar(q$enunciado_html) > 0) {
      q$enunciado_html
    } else {
      q$enunciado
    }

    # Alternativas como radio buttons
    n_alt <- length(q$alternativas)
    choices_labels <- lapply(seq_len(n_alt), function(j) {
      alt <- q$alternativas[[j]]
      txt <- if (nchar(alt$texto_html) > 0) alt$texto_html else alt$texto
      HTML(sprintf('<span>%s</span>', txt))
    })

    choices_vals <- as.character(seq_len(n_alt))
    names(choices_labels) <- choices_vals

    resposta_atual <- rv$respostas[[idx]]

    div(
      # Cabeçalho da questão
      div(class = "moodle-header mb-0",
        h4(
          tags$span(class = "badge-questao", idx),
          if (nchar(q$nome) > 0) q$nome else sprintf("Questão %d", idx)
        )
      ),
      div(class = "questao-box",
        # Enunciado
        div(class = "enunciado",
          HTML(enunciado_html)
        ),
        hr(style = "margin: 12px 0;"),
        # Alternativas
        div(class = "alternativas-titulo", "Selecione uma alternativa:"),
        radioButtons(
          inputId  = "resposta_radio",
          label    = NULL,
          choices  = choices_labels,
          selected = if (!is.null(resposta_atual)) as.character(resposta_atual) else character(0),
          width    = "100%"
        ),
        # Botões de ação
        div(class = "d-flex gap-2 mt-3",
          actionButton(
            inputId = "btn_verificar",
            label   = if (ja_verificada) "Reverificar" else "Verificar Resposta",
            class   = "btn btn-primary",
            icon    = icon("check-circle")
          ),
          actionButton(
            inputId = "btn_limpar",
            label   = "Limpar",
            class   = "btn btn-outline-secondary btn-sm",
            icon    = icon("eraser")
          )
        )
      )
    )
  })

  # ---------- FEEDBACK ----------
  output$ui_feedback <- renderUI({
    req(rv$questoes)
    idx <- rv$idx_atual
    if (!isTRUE(rv$verificadas[[idx]])) return(NULL)

    q        <- rv$questoes[[idx]]
    resp_idx <- rv$respostas[[idx]]
    correta  <- isTRUE(rv$resultados[[idx]])

    if (is.null(resp_idx) || resp_idx == 0) return(NULL)

    alt_escolhida <- q$alternativas[[resp_idx]]

    # Encontra alternativa correta
    idx_correta <- which(sapply(q$alternativas, function(a) a$correta))
    alt_correta <- if (length(idx_correta) > 0) q$alternativas[[idx_correta[1]]] else NULL

    div(
      if (correta) {
        div(class = "feedback-correto",
          tags$b(icon("check-circle"), " Correto!"),
          if (nchar(alt_escolhida$feedback) > 0) {
            div(class = "mt-2", alt_escolhida$feedback)
          }
        )
      } else {
        div(class = "feedback-incorreto",
          tags$b(icon("times-circle"), " Incorreto."),
          if (nchar(alt_escolhida$feedback) > 0) {
            div(class = "mt-2", alt_escolhida$feedback)
          },
          if (!is.null(alt_correta)) {
            div(class = "feedback-solucao mt-2",
              tags$b("Resposta correta: "),
              HTML(if (nchar(alt_correta$texto_html) > 0) alt_correta$texto_html else alt_correta$texto)
            )
          }
        )
      },
      if (nchar(q$feedback) > 0) {
        div(class = "feedback-neutro mt-2",
          tags$b("Explicação: "),
          HTML(q$feedback)
        )
      }
    )
  })

  # ---------- OBSERVADORES ----------

  # Salvar resposta selecionada ao mudar o radio
  observeEvent(input$resposta_radio, {
    req(rv$questoes)
    idx <- rv$idx_atual
    rv$respostas[[idx]] <- as.integer(input$resposta_radio)
    # Se já verificada, limpa verificação ao mudar resposta
    if (isTRUE(rv$verificadas[[idx]])) {
      rv$verificadas[[idx]] <- FALSE
    }
  }, ignoreInit = TRUE)

  # Botão Verificar
  observeEvent(input$btn_verificar, {
    req(rv$questoes)
    idx <- rv$idx_atual
    resp_idx <- rv$respostas[[idx]]

    if (is.null(resp_idx) || length(resp_idx) == 0) {
      showNotification("Selecione uma alternativa antes de verificar.", type = "warning", duration = 3)
      return()
    }

    q       <- rv$questoes[[idx]]
    correta <- isTRUE(q$alternativas[[resp_idx]]$correta)

    # Atualiza estado
    ja_verificada_antes <- isTRUE(rv$verificadas[[idx]])

    # Desconta ponto anterior se já estava verificada
    if (ja_verificada_antes) {
      if (isTRUE(rv$resultados[[idx]])) rv$n_corretas <- rv$n_corretas - 1
      rv$n_verificadas <- rv$n_verificadas - 1
    }

    rv$verificadas[[idx]] <- TRUE
    rv$resultados[[idx]]  <- correta
    rv$n_verificadas      <- rv$n_verificadas + 1
    if (correta) rv$n_corretas <- rv$n_corretas + 1

    # Chama MathJax para re-renderizar LaTeX no feedback
    shinyjs::runjs("setTimeout(function(){ if(window.MathJax){ MathJax.typesetPromise(); } }, 200);")
  })

  # Botão Limpar
  observeEvent(input$btn_limpar, {
    req(rv$questoes)
    idx <- rv$idx_atual

    # Desconta da pontuação se estava verificada
    if (isTRUE(rv$verificadas[[idx]])) {
      if (isTRUE(rv$resultados[[idx]])) rv$n_corretas <- rv$n_corretas - 1
      rv$n_verificadas <- rv$n_verificadas - 1
    }

    rv$respostas[[idx]]  <- NULL
    rv$verificadas[[idx]] <- FALSE
    rv$resultados[[idx]]  <- NULL
    updateRadioButtons(session, "resposta_radio", selected = character(0))
  })

  # Botão Anterior
  observeEvent(input$btn_anterior, {
    req(rv$questoes)
    if (rv$idx_atual > 1) {
      rv$idx_atual <- rv$idx_atual - 1
      updateSelectInput(session, "sel_questao", selected = rv$idx_atual)
    }
  })

  # Botão Próxima
  observeEvent(input$btn_proxima, {
    req(rv$questoes)
    n_q <- length(rv$questoes)
    if (rv$idx_atual < n_q) {
      rv$idx_atual <- rv$idx_atual + 1
      updateSelectInput(session, "sel_questao", selected = rv$idx_atual)
    }
  })

  # Seleção via dropdown
  observeEvent(input$sel_questao, {
    req(rv$questoes)
    novo_idx <- as.integer(input$sel_questao)
    if (!is.na(novo_idx) && novo_idx != rv$idx_atual) {
      rv$idx_atual <- novo_idx
    }
  }, ignoreInit = TRUE)

  # Ao mudar de questão, sincroniza o radio button com resposta salva
  observe({
    req(rv$questoes)
    idx          <- rv$idx_atual
    resp_salva   <- rv$respostas[[idx]]
    isolate({
      if (!is.null(resp_salva)) {
        updateRadioButtons(session, "resposta_radio", selected = as.character(resp_salva))
      } else {
        updateRadioButtons(session, "resposta_radio", selected = character(0))
      }
    })
    # Re-renderiza MathJax
    shinyjs::runjs("setTimeout(function(){ if(window.MathJax){ MathJax.typesetPromise(); } }, 300);")
  })

}

# ==============================================================================
# EXECUTAR O APP
# ==============================================================================
shinyApp(ui = ui, server = server)
