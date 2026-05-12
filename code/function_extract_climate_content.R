# ============================================================
# extract_climate_context.R
#
# Parses text (from a PDF or plain string) for keywords and
# concepts related to climate goals, commitments, policies and
# pledges. For each match, extracts a ~100-word window of
# context (before and after) and stores results in a tibble.
#
# Requirements:
#   install.packages(c("tidyverse", "glue"))
#
# Source read_pdf.R first if your input is a PDF:
#   source("read_pdf.R")
# ============================================================

library(tidyverse)
library(glue)

# ------------------------------------------------------------
# 1. KEYWORD DICTIONARY
# ------------------------------------------------------------

# Each named entry is a category; the vector contains regex
# patterns matched case-insensitively. Add or remove terms freely.

CLIMATE_KEYWORDS <- list(
  
  net_zero = c(
    "net[- ]zero",
    "net zero",
    "carbon neutral(ity)?",
    "climate neutral(ity)?"
  ),
  
  emissions_targets = c(
    "emission(s)? (reduction|target|goal|cut|limit)",
    "greenhouse gas(es)? (target|reduction|limit)",
    "GHG (target|reduction|limit)",
    "carbon (target|budget|reduction|cap)",
    "decarboni[sz](ation|e|ing)",
    "zero[- ]emission(s)?",
    "emission(s)? trading"
  ),
  
  offsetting = c(
    "carbon offset(ting|s)?",
    "carbon credit(s)?",
    "carbon market(s)?",
    "emission(s)? offset(ting|s)?",
    "voluntary carbon",
    "REDD\\+?",
    "carbon removal",
    "carbon capture",
    "carbon sequestration",
    "nature[- ]based solution(s)?"
  ),
  
  pledges_commitments = c(
    "climate pledge(s)?",
    "climate commitment(s)?",
    "climate promise(s)?",
    "nationally determined contribution(s)?",
    "NDC(s)?",
    "Paris (Agreement|accord|goal(s)?|target(s)?)",
    "COP\\d{0,2}",
    "climate action plan",
    "net[- ]zero pledge"
  ),
  
  policies_legislation = c(
    "climate (polic(y|ies)|legislation|law|regulation|bill|act|framework)",
    "green (deal|new deal|transition|economy|finance)",
    "climate finance",
    "carbon (tax|price|pricing|levy)",
    "emissions trading scheme",
    "ETS",
    "renewable energy (target|policy|mandate)",
    "just transition",
    "climate risk(s)?"
  ),
  
  adaptation_resilience = c(
    "climate adaptation",
    "climate resilience",
    "climate[- ]proof(ing)?",
    "adaptive capacit(y|ies)",
    "loss and damage"
  )
)

# Collapse all patterns into one master regex (used for fast scanning)
ALL_PATTERNS <- CLIMATE_KEYWORDS |>
  unlist() |>
  str_c(collapse = "|")

# ------------------------------------------------------------
# 2. HELPER — WORD-BASED CONTEXT WINDOW
# ------------------------------------------------------------

#' Extract a window of words around a match position in a string
#'
#' @param text        Full text string
#' @param match_start Integer — character start position of the match
#' @param match_end   Integer — character end position of the match
#' @param words_each  Number of words to include before and after (default 100)
#'
#' @return A named list: before, keyword, after, context
extract_window <- function(text, match_start, match_end, words_each = 100L) {
  
  # Split full text into words and track their character positions
  # We locate word boundaries and count outward from the match
  before_text  <- str_sub(text, 1L, match_start - 1L)
  after_text   <- str_sub(text, match_end + 1L, -1L)
  keyword_text <- str_sub(text, match_start, match_end)
  
  # Take last `words_each` words before the match
  before_words <- str_split(str_trim(before_text), "\\s+")[[1]]
  before_words <- before_words[before_words != ""]
  n_before     <- length(before_words)
  before_ctx   <- before_words[max(1L, n_before - words_each + 1L):n_before] |>
    str_c(collapse = " ")
  
  # Take first `words_each` words after the match
  after_words <- str_split(str_trim(after_text), "\\s+")[[1]]
  after_words <- after_words[after_words != ""]
  after_ctx   <- after_words[1:min(words_each, length(after_words))] |>
    str_c(collapse = " ")
  
  list(
    before  = before_ctx,
    keyword = keyword_text,
    after   = after_ctx,
    context = str_c(before_ctx, " **", keyword_text, "** ", after_ctx)
  )
}

# ------------------------------------------------------------
# 3. HELPER — CATEGORY LOOKUP
# ------------------------------------------------------------

#' Identify which category a matched keyword belongs to
#'
#' @param matched_text  The matched string from the text
#' @return Category name as a character string
identify_category <- function(matched_text) {
  for (category in names(CLIMATE_KEYWORDS)) {
    patterns <- str_c(CLIMATE_KEYWORDS[[category]], collapse = "|")
    if (str_detect(matched_text, regex(patterns, ignore_case = TRUE))) {
      return(category)
    }
  }
  "other"
}

# ------------------------------------------------------------
# 4. MAIN FUNCTION
# ------------------------------------------------------------

#' Parse text for climate keywords and extract context windows
#'
#' For each keyword match found in the text, a ~100-word window
#' (before and after) is extracted and stored as a row in a tibble.
#' Overlapping matches within 50 words of each other are deduplicated.
#'
#' @param text        A single character string (full document text)
#' @param source_name Label for the source document (e.g. filename)
#' @param words_each  Words of context to capture before and after (default 100)
#'
#' @return A tibble with one row per match and columns:
#'   - source        : document label
#'   - match_id      : sequential match number
#'   - category      : keyword category
#'   - keyword_matched: the exact string that triggered the match
#'   - char_position : character position of match in source text
#'   - before        : up to `words_each` words before the match
#'   - after         : up to `words_each` words after the match
#'   - context       : full context window (before + keyword + after)
#'   - context_words : total word count of the context string
extract_climate_context <- function(text,
                                    source_name = "document",
                                    words_each  = 100L) {
  
  if (!is.character(text) || length(text) != 1L) {
    stop("`text` must be a single character string.")
  }
  
  message(glue("Scanning: {source_name}  ({nchar(text)} characters)"))
  
  # Find all matches
  matches <- gregexpr(
    pattern     = ALL_PATTERNS,
    text        = text,
    ignore.case = TRUE,
    perl        = TRUE
  )[[1]]
  
  if (matches[1] == -1L) {
    message("  No climate keywords found.")
    return(tibble())
  }
  
  match_starts  <- as.integer(matches)
  match_lengths <- attr(matches, "match.length")
  match_ends    <- match_starts + match_lengths - 1L
  
  message(glue("  {length(match_starts)} raw matches found"))
  
  # Build a row for each match
  rows <- map(seq_along(match_starts), function(i) {
    
    win <- extract_window(text, match_starts[i], match_ends[i], words_each)
    
    tibble(
      source          = source_name,
      match_id        = i,
      category        = identify_category(win$keyword),
      keyword_matched = win$keyword,
      char_position   = match_starts[i],
      before          = win$before,
      after           = win$after,
      context         = win$context,
      context_words   = str_count(win$context, "\\S+")
    )
  })
  
  result <- bind_rows(rows)
}
  
# ------------------------------------------------------------
# 5. CONVENIENCE WRAPPER — PDF INPUT
# ------------------------------------------------------------

#' Read a PDF and extract climate context windows in one call
#'
#' Requires read_pdf.R to be sourced.
#'
#' @param pdf_path   Path to the PDF file
#' @param words_each Words of context before and after each match
#' @return A tibble as described in extract_climate_context()
extract_climate_from_pdf <- function(pdf_path, words_each = 100L) {
  
  if (!exists("read_pdf", mode = "function")) {
    stop("read_pdf() not found. Please source('read_pdf.R') first.")
  }
  
  pdf_tbl   <- read_pdf(pdf_path)
  full_text <- str_c(pdf_tbl$text, collapse = "\n\n")
  
  extract_climate_context(
    text        = full_text,
    source_name = basename(pdf_path),
    words_each  = words_each
  )
}

# ------------------------------------------------------------
# 6. EXAMPLE USAGE
# ------------------------------------------------------------

# -- From a PDF (requires read_pdf.R)
source("code/function_read_parse_clean_pdf_2.R")
results <- extract_climate_from_pdf("data/2024_amazon_report.pdf")

# -- From a plain text string
# text    <- readr::read_file("article.txt")
# results <- extract_climate_context(text, source_name = "article.txt")

# -- Inspect results
results |> count(category, sort = TRUE)
results |> filter(category == "net_zero") |> pull(context)

# -- Save to CSV
# readr::write_csv(results, "climate_contexts.csv")