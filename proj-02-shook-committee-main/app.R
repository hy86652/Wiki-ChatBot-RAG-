library(shiny)
library(httr)
library(openai)

# -------------------------------------------------------
# Load API Key
# -------------------------------------------------------
openai_key <- Sys.getenv("OPENAI_API_KEY")

if (openai_key == "") {
  stop(
    "OpenAI API key not found. Please set it as an environment variable 'OPENAI_API_KEY'."
  )
}

# -------------------------------------------------------
# Rewrite query using LLM for better Wikipedia search
# -------------------------------------------------------
rewrite_query <- function(question) {
  response <- create_chat_completion(
    model = "gpt-4.1-mini",
    messages = list(
      list(
        role = "system",
        content = "Rewrite the user's question as a short, clean keyword-style query for Wikipedia. Return ONLY the query."
      ),
      list(role = "user", content = question)
    )
  )

  query <- response$choices[1, "message.content"]
  query <- gsub("[^a-zA-Z0-9 \\-]", "", query)
  query <- trimws(query)

  return(query)
}

# -------------------------------------------------------
# Fetch raw wikitext from Wikipedia using MediaWiki API
# -------------------------------------------------------
fetch_wikitext <- function(title) {
  url <- "https://en.wikipedia.org/w/api.php"

  res <- httr::GET(
    url,
    query = list(
      action = "query",
      prop = "revisions",
      titles = title,
      rvprop = "content",
      rvslots = "main",
      format = "json"
    )
  )

  json <- httr::content(res, "parsed")
  page <- json$query$pages[[1]]

  text <- page$revisions[[1]]$slots$main$`*`

  return(text)
}

# -------------------------------------------------------
# Search Wikipedia + fetch wikitext for each result
# -------------------------------------------------------
fetch_wikipedia <- function(query, limit = 5) {
  url <- "https://en.wikipedia.org/w/api.php"
  res <- httr::GET(
    url,
    query = list(
      action = "query",
      list = "search",
      srsearch = query,
      srlimit = limit,
      format = "json"
    )
  )

  json <- httr::content(res, "parsed")
  titles <- sapply(json$query$search, function(x) x$title)

  pages <- lapply(titles, function(title) {
    wt <- fetch_wikitext(title)
    if (is.null(wt)) {
      wt <- ""
    }

    list(
      title = title,
      url = paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", title)),
      text = wt
    )
  })

  return(pages)
}

# -------------------------------------------------------
# LLM Summarization
# -------------------------------------------------------
summarize_with_llm <- function(question, pages) {
  retrieved_text <- paste(
    sapply(pages, function(p) {
      paste0(
        "### ",
        p$title,
        "\n",
        substr(p$text, 1, 1200),
        "...\n"
      )
    }),
    collapse = "\n\n"
  )

  prompt <- paste0(
    "You are assisting a user by answering their question using ONLY the Wikipedia excerpts provided.\n\n",
    "User question: ",
    question,
    "\n\n",
    "Wikipedia excerpts:\n\n",
    retrieved_text,
    "\n\n",
    "Using ALL of the provided Wikipedia excerpts, write a single unified answer.\n",
    "Synthesize the information across all pages.\n",
    "Do NOT list separate excerpts.\n\n",
    "At the end, include a Citations section using ONLY the pages provided.\n",
    "Format each line as:\n",
    "- TITLE — URL\n"
  )

  response <- create_chat_completion(
    model = "gpt-4.1",
    messages = list(
      list(
        role = "system",
        content = "You accurately summarize Wikipedia content."
      ),
      list(role = "user", content = prompt)
    )
  )

  answer_raw <- response$choices[1, "message.content"]

  return(answer_raw)
}

# -------------------------------------------------------
# Extract citations
# -------------------------------------------------------
extract_citations <- function(answer_text) {
  lines <- unlist(strsplit(answer_text, "\n"))
  cite_lines <- grep("^\\- ", lines, value = TRUE)

  citations <- lapply(cite_lines, function(line) {
    parts <- unlist(strsplit(sub("^\\- ", "", line), " — "))
    list(
      title = parts[1],
      url = parts[2]
    )
  })

  return(citations)
}

# -------------------------------------------------------
# Remove citations section before printing
# -------------------------------------------------------
clean_answer <- function(answer_text) {
  lines <- unlist(strsplit(answer_text, "\n"))
  idx <- grep("^Citations", lines)

  if (length(idx) > 0) {
    cleaned <- lines[1:(idx[1] - 1)]
  } else {
    cleaned <- lines
  }

  paste(cleaned, collapse = "\n")
}

# -------------------------------------------------------
# UI (updated to match new design)
# -------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      body {
        background-color: #f8f9fa;
        font-family: 'Segoe UI', sans-serif;
      }

      .banner {
        background: linear-gradient(90deg, #5a00d1, #0091ff);
        color: white;
        padding: 20px;
        text-align: center;
        border-radius: 8px;
        margin-bottom: 20px;
        font-size: 26px;
        font-weight: bold;
      }

      .card-box {
        background: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0px 2px 6px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }

      .answer-box pre {
        white-space: pre-wrap !important;
        word-break: break-word !important;
      }
    "
    ))
  ),

  div(class = "banner", "Info 5940 By Shook-Committee"),

  div(
    class = "card-box",
    textInput("question", "Enter a question:", width = "80%"),
    div(
      style = "margin-top: 5px; color: gray; font-size: 14px;",
      "Recent queries: what is LLM | what is apple"
    ),
    br(),
    actionButton("ask", "Ask", class = "btn btn-primary"),
    downloadButton("download_citations", "Download citations")
  ),

  div(
    class = "card-box",
    h3("Answer"),
    div(class = "answer-box", verbatimTextOutput("answer"))
  ),

  div(class = "card-box", h3("Citations"), uiOutput("citations"))
)

# -------------------------------------------------------
# SERVER
# -------------------------------------------------------
server <- function(input, output, session) {
  results <- eventReactive(input$ask, {
    req(input$question)

    query <- rewrite_query(input$question)
    pages <- fetch_wikipedia(query)

    answer_raw <- summarize_with_llm(input$question, pages)

    answer_clean <- clean_answer(answer_raw)
    citations <- extract_citations(answer_raw)

    list(
      answer = answer_clean,
      citations = citations
    )
  })

  output$answer <- renderText({
    req(results())
    results()$answer
  })

  output$citations <- renderUI({
    req(results())
    lapply(results()$citations, function(c) {
      tags$div(tags$a(href = c$url, c$title, target = "_blank"))
    })
  })

  # NEW: download citations handler
  output$download_citations <- downloadHandler(
    filename = function() {
      "citations.txt"
    },
    content = function(file) {
      req(results())
      cites <- results()$citations

      lines <- sapply(cites, function(c) {
        paste0(c$title, " — ", c$url)
      })

      writeLines(lines, file)
    }
  )
}

shinyApp(ui, server)
