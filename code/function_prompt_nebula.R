# ============================================================
# nebula_prompt.R
#
# Access the Nebula LLM API and run a prompt.
#
# Requirements:
#   install.packages(c("httr2", "jsonlite", "dotenv"))
#
# Setup:
#   Create a .env file in the same directory containing:
#     NEBULA_API_KEY=your_key_here
# ============================================================

library(httr2)
library(jsonlite)
library(dotenv)

# ------------------------------------------------------------
# 1. NEBULA CLIENT SETUP
# ------------------------------------------------------------

load_dot_env()                                        # reads .env file

NEBULA_BASE_URL <- "https://nebula.cs.vu.nl/api/"
NEBULA_API_KEY  <- Sys.getenv("NEBULA_API_KEY")
MODEL           <- "llama3.1:8b"                      # change to any Nebula model
MAX_TOKENS      <- 2000

# ------------------------------------------------------------
# 2. LIST AVAILABLE MODELS
# ------------------------------------------------------------

#' Retrieve available models from the Nebula API
#'
#' @return A character vector of model IDs
get_nebula_models <- function() {
  
  request(paste0(NEBULA_BASE_URL, "models")) |>
    req_headers(Authorization = paste("Bearer", NEBULA_API_KEY)) |>
    req_perform() |>
    resp_body_json() |>
    pluck("data") |>
    map_chr("id")
}

# ------------------------------------------------------------
# 3. PROMPT FUNCTION
# ------------------------------------------------------------

#' Send a prompt to the Nebula LLM API
#'
#' @param system_prompt  Character string — instructions for the model
#' @param user_prompt    Character string — the user message / question
#' @param model          Model ID to use (default: global MODEL)
#' @param max_tokens     Maximum tokens to generate (default: global MAX_TOKENS)
#' @param configs        Optional named list of extra parameters (e.g. temperature)
#' @return The full response object, or NULL if both attempts fail

prompt_nebula <- function(system_prompt,
                          user_prompt,
                          model      = MODEL,
                          max_tokens = MAX_TOKENS,
                          configs    = NULL) {
  
  body <- list(
    model      = model,
    max_tokens = max_tokens,
    messages   = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_prompt)
    )
  )
  
  if (!is.null(configs)) {
    body <- c(body, configs)                          # merge extra parameters
  }
  
  # Try twice, mirroring the Python retry logic
  for (attempt in 1:2) {
    
    message(glue::glue("Sending request (attempt {attempt})..."))
    
    result <- tryCatch({
      
      resp <- request(paste0(NEBULA_BASE_URL, "chat/completions")) |>
        req_headers(
          Authorization  = paste("Bearer", NEBULA_API_KEY),
          `Content-Type` = "application/json"
        ) |>
        req_body_json(body) |>
        req_timeout(360) |>
        req_perform()
      
      message("Response received!")
      resp |> resp_body_json()
      
    }, error = function(e) {
      message(glue::glue("Attempt {attempt} failed: {e$message}"))
      NULL
    })
    
    if (!is.null(result)) return(result)
  }
  
  message("Both attempts failed, returning NULL.")
  NULL
}

#' Extract the text content from a Nebula response object
#'
#' @param response  The response object returned by prompt_nebula()
#' @return A character string with the model's reply
get_response_text <- function(response) {
  response |> pluck("choices", 1, "message", "content")
}

# ------------------------------------------------------------
# 4. EXAMPLE USAGE
# ------------------------------------------------------------

# -- List available models
# get_nebula_models()

# -- Run a prompt
# system_prompt <- "You are a helpful assistant."
# user_prompt   <- "Summarize the key causes of the 2008 financial crisis in three bullet points."

# response <- prompt_nebula(system_prompt, user_prompt)
# cat(get_response_text(response))
